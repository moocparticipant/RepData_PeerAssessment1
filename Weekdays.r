#use the data set with missing values replaced
#find whether the date is a week day or otherwise
# if weekday 1 weekend 0
day.type <- function (day){
	if (day == "Saturday" || day == "Sunday"){
		return ("weekend") #weekend
	}
	else{
		return ("weekday") #weekday
	}
}
# data set that will be used is x
z <- sapply(weekdays(as.Date(x$date)), FUN = day.type )
#convert to factor variable and append it to the data set
NoNAs$day <-as.factor(z)

#draw panel plots
library(lattice)
panelplot <- function (NoNAs) {
	#average the values 
	#calculate average for each interval peroid across weekdays and weekends
    avgSteps <- aggregate(NoNAs$steps, list(interval = as.numeric(NoNAs$interval), day = NoNAs$day), FUN=mean)
	xyplot (avgSteps$x ~ avgSteps$interval | as.factor(avgSteps$day), layout = c(1,2), type = "l", xlab = "Interval", ylab = "Number of Steps")
}
