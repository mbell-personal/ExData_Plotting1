library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Load the file
powerConsumptionDF <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)

# Add a column combining Date/Time
powerConsumptionDF$MeasurementDateTime <- paste(powerConsumptionDF$Date, powerConsumptionDF$Time)

# Convert columns to the proper data type
powerConsumptionDF$Global_active_power <- as.numeric(powerConsumptionDF$Global_active_power)
powerConsumptionDF$Global_reactive_power <- as.numeric(powerConsumptionDF$Global_reactive_power)
powerConsumptionDF$Voltage <- as.numeric(powerConsumptionDF$Voltage)
powerConsumptionDF$Global_intensity <- as.numeric(powerConsumptionDF$Global_intensity)
powerConsumptionDF$Sub_metering_1 <- as.numeric(powerConsumptionDF$Sub_metering_1)
powerConsumptionDF$Sub_metering_2 <- as.numeric(powerConsumptionDF$Sub_metering_2)
powerConsumptionDF$Sub_metering_3 <- as.numeric(powerConsumptionDF$Sub_metering_3)
powerConsumptionDF$MeasurementDateTime <- as.POSIXct(powerConsumptionDF$MeasurementDateTime, format = "%d/%m/%Y %H:%M:%S")

# Convert Date and Time to the proper data types
powerConsumptionDF$Date <- as.Date(powerConsumptionDF$Date, "%d/%m/%Y")
#powerConsumptionDF$Time <- strptime(powerConsumptionDF$Time, "%H:%M:%S")

# Filter the data set and use records where 2/1/2007 <= Date <= 2/2/2007
data <- filter(powerConsumptionDF, Date >= as.Date("2/1/2007", "%m/%d/%Y") & Date <= as.Date("2/2/2007", "%m/%d/%Y"))

# Create the time series for Global Active Power - do not plot 
globalActivePowerTS <- ggplot(data, aes(x = MeasurementDateTime, y = Global_active_power)) + geom_line() +
                        scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
                        labs(x = "", y = "Global Active Power") +
                        theme_bw() +
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.border = element_rect(colour = "black"),
                              axis.text.y = element_text(angle = 90))

# Create the time series for Voltage - do not plot 
voltageTS <- ggplot(data, aes(x = MeasurementDateTime, y = Voltage)) +
    scale_y_continuous(breaks = seq(234, 246, by=4), limits=c(234, 246)) +
    geom_line() +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    labs(x = "datetime", y = "Voltage") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.text.y = element_text(angle = 90))

# Create the time series for Sub Metering - do not plot 
meteringDF <- data %>%
    select(MeasurementDateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
    gather(key = "variable", value = "value", -MeasurementDateTime)

subMeteringTS <- ggplot(meteringDF, aes(x = MeasurementDateTime, y = value)) + geom_line(aes(color = variable)) +
        scale_color_manual(values = c("black", "red",  "blue")) +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
        labs(x = "", y = "Energy sub metering") +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black"),
              legend.position = c(.999, .999),
              legend.justification = c("right", "top"),
              legend.title = element_blank(),
              axis.text.y = element_text(angle = 90))

# Create the time series for Global Reactive Power - do not plot 
globalReactivePowerTS <- ggplot(data, aes(x = MeasurementDateTime, y = Global_reactive_power)) + geom_line() +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    labs(x = "datetime") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.text.y = element_text(angle = 90))

# Finally, plot all time series on a single chart
png("plot4.png", height = 480, width = 480)
grid.arrange(globalActivePowerTS, voltageTS, subMeteringTS, globalReactivePowerTS, nrow = 2, ncol = 2)
dev.off()
