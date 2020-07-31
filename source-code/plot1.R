## Aim of this function is to 
### 1. Read the household_power_consumption.txt file
### 2. Subset for data taken from 2 days: 2007-02-01 and 2007-02-02
### 3. Generate a histogram of global active power(kilowatts)

## Parameters
### 1. Full path directory to where the data set exist
## Assumption 
### 1. Dataset exist in the file household_power_consumption.txt file 
### 2. The file is located in working dir or given directory input

plot1 <- function(dir = ""){
    ## getting the directory
    if(dir == ""){
        dir = paste(getwd(),"/dataset/", sep ="");
    }
    
    ## Read and loads the data into memory by reading the file from the given directory name
    if(file.exists(paste(dir,"household_power_consumption.txt", sep = ""))){
        data_power <- read.table(paste(dir,"household_power_consumption.txt", sep = ""), stringsAsFactors = FALSE, header = TRUE, sep =";" );
    } else {
        stop(paste(paste(dir,"household_power_consumption.txt", sep = ""), " does not exist or inaccessible"));
    }
     
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
    png(paste(getwd(),"/plot-png/plot1.png", sep =""), width=480, height=480)
    hist(data_subset$Global_active_power, col="red", border="black", main ="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")
    dev.off()
}