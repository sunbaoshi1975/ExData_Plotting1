# Exploratory Data Analysis
# Course Porject 1 - Plot 1
#
# Data source: 
#   Individual household electric power consumption Data Set
#   from  the UC Irvine Machine Learning Repository, a popular repository for machine learning datasets
#
# Histograph on Global Active Power from the dates 2007-02-01 and 2007-02-02
#
#--------------------------------
plot1 <- function(genPNG=TRUE) {
    
    # The following code snippet is shared through plot 1 to plot 4
    ## Loading Data
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipfn <- "exdata-data-household_power_consumption.zip"
    destdir <- "household_power_consumption.txt"
    if (!file.exists(destdir)) {
        if(!file.exists(zipfn)) {
            ## Download the data file
            download.file(fileURL, destfile=zipfn)
        }
        ## Unzip the data file
        unzip(zipfn)
    }
    
    ## Read txt file, only get data those from the dates 2007-02-01 and 2007-02-02
    v_colClass <- c("character", "character", rep("numeric", 7))
    raw.header <- read.table(destdir, sep=";", header=FALSE, nrows=1, stringsAsFactors=FALSE)
    raw.data <- read.table(destdir, sep=";", header=FALSE, colClasses=v_colClass, nrows=2880, skip=66637, na.strings = "?")
    names(raw.data) <- raw.header
    ## Or we can read all of the data and filter
    ## raw.all <- read.table(destdir, sep=";", header=TRUE, na.strings = "?")
    ## raw.sub <- raw.all[raw.all$Date=="1/2/2007" | raw.all$Date=="2/2/2007",]
    
    ## Convert the first 2 columns to Date/Time
    raw.data$Time <- paste(raw.data$Date, raw.data$Time)
    raw.data$Date <- strptime(raw.data$Date, format="%d/%m/%Y")
    raw.data$Time <- strptime(raw.data$Time, format="%d/%m/%Y %H:%M:%S")
    #--------------------------------
    
    #--------------------------------
    ## Save as png
    if (genPNG) {
        png("./plot1.png")
    }
    
    ## Draw Plot
    hist(raw.data$Global_active_power, xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power", col="red", ylim=c(0,1200))
    #v_ytick <- seq(0, 1200, by=200)
    #axis(side=2, at=v_ytick)
    
    ## Close device
    if (genPNG) {
        dev.off()
    }
    #--------------------------------
}