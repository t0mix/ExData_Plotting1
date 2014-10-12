## Set of functions to load household_power_consumption.txt as well as to generate and save plot1 to a png file for Exploratory Data Analysis Assignment 1

## function to load data from household_power_consumption.txt file into R and keep rows for indicated timeframe
loaddata <- function(filepath = "household_power_consumption.txt", startdate = "2007-02-01 00:00:00" , enddate = "2007-02-02 23:59:59") {
        # read file
        data <- read.table(file = filepath , na.strings = "?", sep = ";", header = TRUE, as.is = TRUE )
        # convert Date variable from character to Date
        data[,1] <- as.Date(data[,1], "%d/%m/%Y")
        # create a DateTime variable from Date and Time
        DateTime <- strptime( paste( as.character(data[,1]) , data[,2] ) , format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        # Add DateTime column to data.table (wshould become the 10th column)
        data <- cbind(data,DateTime)
        # filter to rows matching indicated time frame
        data[ data[,"DateTime"]>=strptime( startdate , format = "%Y-%m-%d %H:%M:%S", tz = "GMT") & data[,"DateTime"]<strptime( enddate , format = "%Y-%m-%d %H:%M:%S", tz = "GMT") ,  ] 
}

## generate plot and save it to a png file
plot1topng <- function(data) {
        custompar=par(ps=12,font=2,font.axis=1,font.lab=1,mfrow = c(1,1))
        hist(data[,3] , xlab = "Global Active Power (kilowatts)",main = "Global Active Power", col = "red")
        custompar
        dev.copy(png,"plot1.png",width=480,height=480)
        dev.off()
}