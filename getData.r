library(lubridate)
library(sqldf)

readData <- function(){
#================================================
#This function returns data required for plotting
#================================================ 
  #download data from URL
  print ("file is going to be downloaded")
  dataFileURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  #https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
  #I am using Windows so https is removed and replaced with http curl does not work in my machine
  if (!file.exists(".\\data")){dir.create(".\\data")}#if there is no directory called data create one
  #now store downloaded file in data directory
  download.file(dataFileURL, destfile = ".\\data\\data.zip", mode ="wb")
  #mode set to "wb" because this is binary data
  unzip (".\\data\\data.zip")
 				
  ### READING data  
  activityMonitoring <- read.csv("activity.csv", header = TRUE)
  ## steps       date interval
  ## 1    NA 2012-10-01        0
  ## 2    NA 2012-10-01        5
  activityMonitoring
}