# MA-615-Final-Project-

For the AirBNB Report, and AirBNB Ioslides presentation to run and knit properly, you will need each city's .csv file within 
your working directory (ie. Boston_s.csv, NY_s.csv, Philadelphia_s.csv, Washington_s.csv, Miami_s.csv, Nashville_s.csv, 
and Houston_s.csv)

For the "Final Report App.R" app file to run properly, you will also need the above city .csv data files within your working 
directory, along with the "BNB_db.sqlite" database file. 

The "Sampling Data.R" file shows how the city .csv files sampled 75 city listings from each large raw data file, if you are interested
in how the city .csv files were generated. The reason that these files were saved externally, is because the R function "set.seed()"
was not staying consistent with generating the same listing values each time, and so to ensure reproducibility of the report, slides 
presentation and the App, the original samples were saved (files you see above).
