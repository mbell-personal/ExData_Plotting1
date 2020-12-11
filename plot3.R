library(dplyr)
library(ggplot2)
library(tidyr)

# Load the file
powerConsumptionDF <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)

# Add a column combining Date/Time
powerConsumptionDF$MeasurementDateTime <- paste(powerConsumptionDF$Date, powerConsumptionDF$Time)

# Convert columns to the proper data type
powerConsumptionDF$Sub_metering_1 <- as.numeric(powerConsumptionDF$Sub_metering_1)
powerConsumptionDF$Sub_metering_2 <- as.numeric(powerConsumptionDF$Sub_metering_2)
powerConsumptionDF$Sub_metering_3 <- as.numeric(powerConsumptionDF$Sub_metering_3)
powerConsumptionDF$MeasurementDateTime <- as.POSIXct(powerConsumptionDF$MeasurementDateTime, format = "%d/%m/%Y %H:%M:%S")

# Convert Date and Time to the proper data types
powerConsumptionDF$Date <- as.Date(powerConsumptionDF$Date, "%d/%m/%Y")
#powerConsumptionDF$Time <- strptime(powerConsumptionDF$Time, "%H:%M:%S")

# Filter the data set and use records where 2/1/2007 <= Date <= 2/2/2007 and create the data set to plot
metermingDF <- powerConsumptionDF %>%
                filter(Date >= as.Date("2/1/2007", "%m/%d/%Y") & Date <= as.Date("2/2/2007", "%m/%d/%Y")) %>%
                select(MeasurementDateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
                gather(key = "variable", value = "value", -MeasurementDateTime)

# Write the Time Series to a png file
png("plot3.png", height = 480, width = 480)
ggplot(metermingDF, aes(x = MeasurementDateTime, y = value)) + geom_line(aes(color = variable)) +
    scale_color_manual(values = c("black", "red",  "blue")) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
    labs(x = "", y = "Energy sub metering") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          legend.position = c(.999, .999),
          legend.box.background = element_rect(),
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          axis.text.y = element_text(angle = 90))
dev.off()
