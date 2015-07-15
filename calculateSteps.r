#This code takes a data table and calculates the total steps
#steps, date,interval are the columns in the data table
library(plyr)

calculateSteps <- function (data) {
	dateSteps <- data[, c(1,2)]# only take date and steps columns
	NARemovedSteps <- dateSteps[complete.cases(dateSteps),] #remove all NAs
	#using ddply summarise steps by date
	DailyTotal <- ddply(NARemovedSteps, "date", summarise, sum = sum(steps))
 }
 
 StepsHistogram <- function (DailyTotal){
 #takes daily total and prints a histogram
	hist(DailyTotal$sum, xlab = "Daily Total", ylab = "Frequency", main = "Histogram of Daily Total of Steps" )
 }
 
 calculateMean <- function (DailyTotal){
	mean(DailyTotal$sum)
 }
 
 calculateMedian <- function (DailyTotal) {
	median (DailyTotal$sum)
 }