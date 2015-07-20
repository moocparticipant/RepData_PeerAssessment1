# This code answers the question
# What is the average daily activity pattern

source("calculateSteps.r")

timeSeriesPlot <- function (data){
  ##      steps       date          interval
  ## 1    NA         2012-10-01        0
  ## 2    NA         2012-10-01        5
  NARemovedData <- data[complete.cases(data),]
  # now all NA s are removed from the data set
  
  #calculate average for each interval peroid across days
  intervalAvg <- ddply(NARemovedData, "interval", summarise, sum = mean(steps))
  
  # plot the time series of 5 min intervals
  with (intervalAvg,  plot(intervalAvg$interval,intervalAvg$sum, 
					type ="l", ylab = "Average Steps across all days", xlab ="Five minute interval"))
  maxAvg = max(intervalAvg$sum) #find maximum value
  maxrow <- subset(intervalAvg,sum == maxAvg)#find the row with max value 
  abline (v= maxrow[1,1], col = "blue" ) #draw vertical line to show the max value interval
  maxrow[1,1]
 }

calculateNA <- function (data) {
  NARemovedData <- data[complete.cases(data),]
  nrow(data) - nrow (NARemovedData)
}

fillMissing <- function (data){
	NARemovedSteps <- dateSteps[complete.cases(data),] #remove all NAs
	DailyAvg <- ddply(NARemovedSteps, "date", summarise, avg = round(mean(steps)))#rounded value
	NoNAs <- data #[c(1,2, 17568, 16858),]
	newSteps <- numeric() #create empty vector of type numeric
	for (i in 1:nrow(NoNAs)){
	  if (is.na(NoNAs[i,c(1)]) ){ #c(1) is steps
	    calendarDate <- NoNAs[i,c(2)]#c(2) is date
		#this means the value is NA so we need to fill the value with mean
		value <- DailyAvg[DailyAvg$date == calendarDate,]
		steps = 0
		if (nrow(value) == 0){
		  steps = 0
		}
		else {
         steps = 	value[1,c(2)]
        }		 
		newSteps <- c(newSteps, steps )
			
	  }
	  else {
		#this element is not NA
		#so add that value to the vector newSteps
		newSteps <- c(newSteps,NoNAs[i, c(1)])
		#print("#")
	  }
	}
	#Now the newSteps have all the values for the missing values in them
	#drop the column steps from NoNAs and add the column newSteps
	
	NoNAs[,"steps1"] <- newSteps 
	NoNAs$steps <- NULL
	#change colunm name
	colnames(NoNAs)[3] <- "steps"
	#reorder columns to have # steps date interval
	x <- NoNAs[, c(3,1,2)]
	
}


