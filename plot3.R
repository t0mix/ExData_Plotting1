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
plot3topng <- function(data) {
        custompar=par(ps=12,font=1,font.axis=1,font.lab=1,mfrow = c(1,1))
        plot(data[,10],data[,7], xlab = "", ylab = "Energy sub metering", col = "black", type="l")
        lines(data[,10],data[,8], col="red", lwd=1)
        lines(data[,10],data[,9], col="blue", lwd=1)
        legend(
            "topright", 
            legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
            lty=c(1,1,1),  
            lwd=c(1,1,1),
            col=c("black","red","blue"),
            text.width=50000)
        custompar
        dev.copy(png,"plot3.png",width=480,height=480)
        dev.off()
}


