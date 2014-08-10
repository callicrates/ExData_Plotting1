## Andy Wilson
## Exploratory Data Analysis
## Assignment 1 Part 2: Active Power Line Chart


## Load the data for Exercise 1 from a flat file.
##
## I'm adhering to the notion that the data cleaning / preparation
## script should have no arguments.  It will certainly make it easier for
## me as I'm exploring than if I had to remember the filename every time.
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

## Extract all rows for a date range from a frame containing a column named Date.
##
## Args:
##    df (data.frame): Frame to subset
##    startTime (Date): Start date
##    endTime (Date): End date
##
## Returns:
##    data.frame containing rows whose Date is between startTime and endTime inclusive

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

## Create the line chart required for Assignment 1 part 2.
##
## Args:
##   None.
##
## Returns:
##   Nothing.
##
## Side Effects:
##   The plot will be written to the file 'plot2.png' in the current directory.

plot2 <-  function() {
    powerData <- loadDataForExercise()
    png("plot2.png", width=480, height=480)

    # Set up the plot with appropriate dimensions but don't draw data
    plot(powerData$Timestamp, powerData$Global_active_power,
         xlab="",
         ylab="Global Active Power (kilowatts)",
         type="n")
    # This is where we actually render the data
    lines(powerData$Timestamp, powerData$Global_active_power)

    dev.off()
}
