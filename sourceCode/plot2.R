## Aim of this function is to 
### 1. Read the household_power_consumption.txt file
### 2. Subset for data taken from 2 days: 2007-02-01 and 2007-02-02
### 3. Generate a plot of global active power vs. time

## Parameters
### 1. Full path directory to where the data set exist
## Assumption 
### 1. Dataset exist in the file household_power_consumption.txt file 
### 2. The file is located in working dir or given directory input

plot2 <- function(dir = ""){
    ## getting the directory
    if(dir == ""){
        dir = paste(getwd(),"/dataset/", sep ="");
    }
    
    ## Read and loads the data into memory by reading the file from the given directory name
    data_power = c();
    
    if(file.exists(paste(dir,"household_power_consumption.txt", sep = ""))){
        data_power <- read.table(paste(dir,"household_power_consumption.txt", sep = ""), stringsAsFactors = FALSE, header = TRUE, sep =";" );
    } else {
        stop(paste(paste(dir,"household_power_consumption.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    ## Create column in table with date and time merged together
    data_datetimecombine <- strptime(paste(data_power$Date, data_power$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
    data_power <- cbind(data_power, data_datetimecombine)
    
    ## change class of all columns to correct class
    data_power$Date <- as.Date(data_power$Date, format="%d/%m/%Y")
    data_power$Time <- format(data_power$Time, format="%H:%M:%S")
    data_power$Global_active_power <- as.numeric(data_power$Global_active_power)
    data_power$Global_reactive_power <- as.numeric(data_power$Global_reactive_power)
    data_power$Voltage <- as.numeric(data_power$Voltage)
    data_power$Global_intensity <- as.numeric(data_power$Global_intensity)
    data_power$Sub_metering_1 <- as.numeric(data_power$Sub_metering_1)
    data_power$Sub_metering_2 <- as.numeric(data_power$Sub_metering_2)
    data_power$Sub_metering_3 <- as.numeric(data_power$Sub_metering_3)
    
    ## subset data from 2007-02-01 and 2007-02-02
    data_subset <- subset(data_power, Date == "2007-02-01" | Date =="2007-02-02")
    
    ## plot histogram of global active power for those 2 days
    png(paste(getwd(),"/plot-png/plot2.png", sep =""), width=480, height=480)
    with(data_subset, plot(data_datetimecombine, Global_active_power, type="l", xlab="Day", ylab="Global Active Power (kilowatts)"))
    dev.off()
}