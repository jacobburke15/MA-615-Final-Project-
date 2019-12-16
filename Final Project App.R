#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinydashboard)
library(RMySQL)
library(dplyr)
library(tidyverse) 
library(gridExtra)
library(maps)     
library(mapdata) 
library(mapview)
library(Hmisc)
library(leaflet)
library(psych)
library(GPArotation)
library(nFactors)
library(DBI)
library(RSQLite)

## Getting our BNB data set together for app use

#################################################################################333
read <- function(data){
    
    return(read.csv(data, header = T))
}

Boston <- read("Boston_s.csv")
Houston <- read("Houston_s.csv")
Miami <- read("Miami_s.csv")
Nashville <- read("Nashville_s.csv")
NY <- read("NY_s.csv")
Philly <- read("Philly_s.csv")
Washington <- read("Washington_s.csv")

## adding city attributes, we can then combine all city data for analysis

addCity <- function(df, city){
    return(mutate(df, City = city))
}

Boston <- addCity(Boston, "Boston")
Houston <- addCity(Houston, "Houston")
Miami <- addCity(Miami, "Miami")
Nashville <- addCity(Nashville, "Nashville")
NY <- addCity(NY, "New York")
Philly <- addCity(Philly, "Philadelphia")
Washington <- addCity(Washington, "Washington")

## dropping columns we won't be using for analysis, so each data frame is of the same value of 
## variables, and then we can combine data sets 

Boston <- dplyr::select(Boston, -c(minstay, borough, neighborhood, overall_satisfaction))
Houston <- dplyr::select(Houston, -c(survey_id, country, city, borough, minstay, bathrooms, name, location, 
                                     neighborhood, overall_satisfaction))
Miami <- dplyr::select (Miami, -c(borough, minstay, neighborhood, overall_satisfaction))
Nashville <- dplyr::select(Nashville, -c(survey_id, country, city, borough, bathrooms, minstay, location, 
                                         neighborhood, overall_satisfaction))
NY <- dplyr::select(NY, -c(survey_id, borough, minstay, country, city, name, property_type, location, bathrooms, 
                           neighborhood, overall_satisfaction))
Philly <- dplyr::select(Philly, -c(borough, minstay, neighborhood, overall_satisfaction))
Washington <- dplyr::select(Washington, -c(survey_id, country, city, borough, bathrooms, minstay, location, 
                                           neighborhood, overall_satisfaction))

## Now can combining 

BNB <- rbind(Boston, Houston, Miami, Nashville, NY, Philly, Washington)

BNB <- BNB[, -1]

#######################################################################################################

## setting mapping bounds and icons 

bounds <- map('state', c('Massachusetts', 'New York', 'Florida', 'Texas', 
                         'Pennsylvania', 'Tennessee', 'Maryland'), fill=TRUE, plot=FALSE)


icons <- awesomeIcons(
    icon = 'disc',
    iconColor = 'white',
    library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
    markerColor = 'black',
    spin = T, 
    squareMarker = TRUE, 
)


####################################################################################################

# App

ui <- fluidPage(
    tabsetPanel(id = "tabs",
                tabPanel(value = "data", title = "AirBNB Data",
                         box(title = "What data would you like to see?", width = NULL, 
                             checkboxGroupInput("variable", "Variables:",
                                                c("Room ID" = "room_id",
                                                  "Host ID" = "host_id",
                                                  "Room Type" = "room_type",
                                                  "Reviews" = "reviews",
                                                  "Listing Accommodation No." = "accommodates",
                                                  "Listing Bedroom No." = "bedrooms",
                                                  "Listing Price" = "price",
                                                  "Listing Latitude" = "latitude",
                                                  "Listing Longitude" = "longitude", 
                                                  "Last Modified Date" = "last_modified",
                                                  "City" = "City")) ),
                         
                         box(title = "AirBNB Listing Data (Scrollbar below)", width = NULL, 
                             div(style = 'overflow-x:scroll', dataTableOutput("data")))
                         
                ),
                tabPanel(value = "exp", title = "AirBNB EDA", 
                         titlePanel("AirBNB Eastern US Listings EDA"), 
                         fluidRow(
                         column(width = 10, 
                                box(title = "Choose a Histogram", width = NULL, selectInput("hist", "Histograms", choices = c("Accommodations by City" = "accommodates",
                                                                                                                               "Room Type by City" = "room_type"
                                                                                                                         ))),
                                box("Distributions Amongst City", width = NULL, plotOutput("hist"))
                         )
                )
    ), 
    tabPanel(value = "mapping", title = "AirBNB Mapping",
             titlePanel("Mapped AirBNB Listings (Select Your Listing Filters)"), 
             
             #Select input for Dist of traffic signals 
             sidebarPanel(
                 selectInput("accomm", "Accomodation Number", choices = c(1,2,3,4,5,6,7,8,9,10,12)), 
                 selectInput('beds', "Bedroom Counts", choices = c(0,1,2,3,4,5,6)), 
                 selectInput('room_type', "Room Type", choices = c("Entire home/apt", "Private room", "Shared room"))
             ),
             mainPanel(
                 titlePanel("This is where we could find your listing preferences:"),
                 leafletOutput("MyMap"), 
                 width = 12
             )
    ), 
    tabPanel(value = "sql", title = "AirBNB SQLite DataBase", 
             titlePanel("Dynamically Query From Built in AirBNB Database"),
             sidebarPanel(
             numericInput("nrows", "Enter the number of rows to display: (limit 50)", 50),
             numericInput("nrooms", "Number of bedrooms:", 5),
             numericInput("naccomm", "Number of People Accommodating:", 12), 
             selectInput('price', "Price Range", choices = c("Less than $100", "$100 - $200", "Over $200"))),
             
             mainPanel(
             tableOutput("tbl"))
    )
    
)
)


server <- function(input, output, session) {
    
    tableData = reactiveVal()
    plotData = reactiveVal()
    
    observeEvent(input$tabs, {
        
        if(input$tabs == "data"){
            output$data <- renderDataTable({
                BNB[, c(input$variable), drop = FALSE]
            })
            
        } else if(input$tabs == "mapping"){
            output$MyMap <- renderLeaflet({
                Listing1 <- BNB %>% filter(accommodates == input$accomm) %>% filter(bedrooms == input$beds) %>%
                                filter(room_type == input$room_type)
                map <- leaflet(data = BNB) %>%
                    addProviderTiles("CartoDB.Positron", group = "Map") %>%
                    addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
                    addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
                    addAwesomeMarkers(~Listing1$longitude, ~Listing1$latitude, group = "Sites", icon=icons) %>%
                    addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
                    addScaleBar(position = "bottomleft") %>%
                    addLayersControl(
                        baseGroups = c("Map", "Satellite", "Relief"),
                        overlayGroups = c("Sites", "States"),
                        options = layersControlOptions(collapsed = FALSE),
                    )
            })
        }
        else if(input$tabs == "exp"){
              
              output$hist <- renderPlot({
                  
                  if(input$hist == "Accommodations by City"){
                  ggplot(BNB) + geom_histogram(aes(x = BNB[,input$hist]), binwidth = 1, color = 'black', fill = 'blue') + 
                      facet_wrap(~City) }
                  
                  else
                      {
                      ggplot(BNB) + geom_bar(aes(x = BNB[, input$hist]), col = 'black', 
                               fill = 'blue') + labs(y = 'Count', x = 'Room Type') + facet_wrap(~City)
                  }
                  
              })
              
        }
    else if(input$tabs == "sql"){
        output$tbl <- renderTable({
            
            BNB_db <- dbConnect(SQLite(), "BNB_db.sqlite")
            
            if(input$price == "Less than $100"){
                dbGetQuery(BNB_db, paste0(
                "SELECT * FROM Room where bedrooms = ", input$nrooms, " and accommodates = ", input$naccomm, 
                " and price < 100 LIMIT ", input$nrows, ";"))
            }
            else if(input$price == "$100 - $200"){
                dbGetQuery(BNB_db, paste0(
                "SELECT * FROM Room where bedrooms = ", input$nrooms, " and accommodates = ", input$naccomm, 
                " and price > 100 and price < 200 LIMIT ", input$nrows, ";"))
            }
            else{
                dbGetQuery(BNB_db, paste0(
                "SELECT * FROM Room where bedrooms = ", input$nrooms, " and accommodates = ", input$naccomm, 
                " and price > 200 LIMIT ", input$nrows, ";"))
                }
           ## dbGetQuery(BNB_db, paste0(
               ## "SELECT * FROM Room where bedrooms = ", input$nrooms, " and accommodates = ", input$naccomm, 
                ##" LIMIT ", input$nrows, ";"))
        })
    }
    }
)
    
    
}   



shinyApp(ui, server)
