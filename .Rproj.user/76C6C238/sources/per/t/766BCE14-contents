## Aim of this function is to 
### 1. Read the household_power_consumption.txt file
### 2. Subset for data taken from 2 days: 2007-02-01 and 2007-02-02
### 3. Generate 4 plots in 1 space (GAP vs. time, Vol vs. time, submetering vs. time and GRP vs. time)

## Parameters
### 1. Full path directory to where the data set exist
## Assumption 
### 1. Dataset exist in the file household_power_consumption.txt file 
### 2. The file is located in working dir or given directory input

plot4 <- function(dir = ""){
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
    
    ## plot globalactivepower vs date&time
    png(paste(getwd(),"/plot-png/plot4.png", sep =""), width=480, height=480)
    par(mfrow=c(2,2))
    with(data_subset, plot(data_datetimecombine, Global_active_power, type="l", xlab="", ylab="Global Active Power"))
    with(data_subset, plot(data_datetimecombine, Voltage, type = "l", xlab="Day", ylab="Voltage"))
    with(data_subset, plot(data_datetimecombine, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering"))
    lines(data_subset$data_datetimecombine, data_subset$Sub_metering_2,type="l", col= "red")
    lines(data_subset$data_datetimecombine, data_subset$Sub_metering_3,type="l", col= "blue")
    legend(c("topright"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty= 1, lwd=2, col = c("black", "red", "blue"))
    with(data_subset, plot(data_datetimecombine, Global_reactive_power, type="l", xlab="Day", ylab="Global Reactive Power"))
    dev.off()
}