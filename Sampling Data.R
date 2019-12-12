
## Raw Data Reading and Sampling 

temp = list.files(pattern = "*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

Boston <- as_tibble(Boston.csv)
Houston <- as_tibble(Houston.csv)
Miami <- as_tibble(Miami.csv)
Nashville <- as_tibble(Nashville.csv)
NY <- as_tibble(NY.csv)
Philly <- as_tibble(Philly.csv)
Washington <- as_tibble(Washington.csv)


## Now, for this analysis, we want to sample from each data set so that each city's data consists of the same number of listings. For this analysis, we will sample 75 listings within each city. 

sample_city <- function(data) {
  
  set.seed(2019) ## not keeping things consistent
  
  return(data[sample(x = 1:nrow(data), size = 75, replace = F), ])
  
}

set.seed(2019) ## this will not seem to work for consistent sampling

## Therefore will save these sampled files as .csv's to keep the data analysis reproducible in 
## the report. 

Boston <- sample_city(Boston)
Houston <- sample_city(Houston)
Miami <- sample_city(Miami)
Nashville <- sample_city(Nashville)
NY <- sample_city(NY)
Philly <- sample_city(Philly)
Washington <- sample_city(Washington)

write_csv <- function(name, data){
  
  return(write.csv(x = data, file = name)) 
}

write_csv("Boston_s.csv", Boston)
write_csv("Houston_s.csv", Houston)
write_csv("Miami_s.csv", Miami)
write_csv("Nashville_s.csv", Nashville)
write_csv("NY_s.csv", NY)
write_csv("Philly_s.csv", Philly)
write_csv("Washington_s.csv", Washington)
