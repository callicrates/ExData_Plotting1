## Andy Wilson
## Exploratory Data Analysis
## Assignment 1 Part 1: Active Power Histogram


## Load the data for Exercise 1 from a flat file.
##
## I'm adhering to the notion that the data cleaning / preparation
## script should have no arguments.  It will certainly make it easier for
## me as I'm exploring than if I had to remember the filename every time.
##
## NOTE: This assumes that you have the file
## 'household_power_consumption.txt' in the current directory.
##
## Args:
##   None.  File path is hardcoded in the name of reproducibility.
##
## Returns:
##   Data frame with entire data set.
##

loadAllData <- function() {
    read.table("household_power_consumption.txt",
               header=TRUE,
               sep=";",
               na.strings="?",
               colClasses=c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
}

## Convert the Date and Time columns into a single timestamp.
##
## Args:
##   df (data.frame): Frame to add column to
##   outputColumn (character): Name of timestamp column
##
## Returns:
##   New data frame with column added

addTimestampColumn <- function(df, outputColumn="Timestamp") {
    df[outputColumn] <- with(df, as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    df
}

## Extract all rows for a timestamp range from a frame containing a column named Date.
##
## Args:
##    df (data.frame): Frame to subset
##    startTime (Date): Start date
##    endTime (Date): End date
##
## Returns:
##    data.frame containing rows whose Timestamp is between startTime and endTime inclusive

extractDateSubset <- function(df, startTime, endTime) {
    subset(df, Timestamp >= as.POSIXct(startTime) & Timestamp <= as.POSIXct(endTime))
}

## Load the data we need for Assignment 1.
##
## Args:
##   None.
##
## Returns:
##   Data frame containing exercise data subsetted to 2007-02-01 and 2007-02-02.

loadDataForExercise <- function() {
    originalData <- loadAllData()
    dataWithTimestamps <- addTimestampColumn(originalData)
    extractDateSubset(dataWithTimestamps, "2007-02-01 00:00:00 MST", "2007-02-02 23:59:59 MST")
}

## Render subplot #1 for plot 4
##
## Plot global active power against time (line chart).
##
## Args:
##   df (data.frame): Data to render
##
## Returns:
##   Nothing
##
## Side Effects:
##   Figure 4.1 will be rendered to the current device.

drawSubplot1 <- function(df) {
    with(df,
         {
             plot(Timestamp, Global_active_power,
                  xlab="", ylab="Global Active Power",
                  type="n")
             lines(Timestamp, Global_active_power)
     })
}

## Render subplot #2 for plot 4
##
## Plot voltage against time (line chart).
##
## Args:
##   df (data.frame): Data to render
##
## Returns:
##   Nothing
##
## Side Effects:
##   Figure 4.2 will be rendered to the current device.

drawSubplot2 <- function(df) {
    with(df,
         {
             plot(Timestamp, Voltage,
                  xlab="datetime", ylab="Voltage",
                  type="n")
             lines(Timestamp, Voltage)
         })
}


## Render subplot #3 for plot 4
##
## Plot the three sub-metered quantities against time (superimposed line charts).
##
## Args:
##   df (data.frame): Data to render
##
## Returns:
##   Nothing
##
## Side Effects:
##   Figure 4.3 will be rendered to the current device.

drawSubplot3 <- function(df) {

    # Compute the common data range so that we can be
    # sure that all three series will fit within the
    # plot
    yRange <- with(df, range(c(Sub_metering_1, Sub_metering_2, Sub_metering_3)))

    with(df,
         {
             plot(Timestamp, Sub_metering_1,
                  type="n",
                  ylim=yRange,
                  ylab="Energy sub metering",
                  xlab="")
             lines(Timestamp, Sub_metering_1, col="black")
             lines(Timestamp, Sub_metering_2, col="red")
             lines(Timestamp, Sub_metering_3, col="blue")
             legend.text <- c("Sub_metering_1",
                              "Sub_metering_2",
                              "Sub_metering_3")
             legend.colors <- c("black", "red", "blue")
             # The 'bty="n"' instructs R not to draw a
             # box around the legend
             legend("topright", lty=c(1, 1),
                    col=legend.colors,
                    legend=legend.text, bty="n")
         })
}


## Render subplot #4 for plot 4
##
## Plot the global reactive power against time (line chart).
##
## Args:
##   df (data.frame): Data to render
##
## Returns:
##   Nothing
##
## Side Effects:
##   Figure 4.4 will be rendered to the current device.

drawSubplot4 <- function(df) {
    with(df,
         {
             plot(Timestamp, Global_reactive_power,
                  type="n",
                  ylab="Global_reactive_power",
                  xlab="datetime")
             lines(Timestamp, Global_reactive_power)
         })
}

## Create the 4-up plot required for Assignment 1 part 4.
##
## Args:
##   None.
##
## Returns:
##   Nothing.
##
## Side Effects:
##   The plot will be written to the file 'plot4.png' in the current directory.

plot4 <-  function() {
    powerData <- loadDataForExercise()
    png("plot4.png", width=480, height=480)

    par("cex", 0.5)

    par(mfrow=c(2, 2))

    with(powerData,
         {
             drawSubplot1(powerData)
             drawSubplot2(powerData)
             drawSubplot3(powerData)
             drawSubplot4(powerData)
         })

    dev.off()
}
