library(dplyr)
library(ggplot2)

# Load the file
powerConsumptionDF <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)

# Add a column combining Date/Time
powerConsumptionDF$MeasurementDateTime <- paste(powerConsumptionDF$Date, powerConsumptionDF$Time)

# Convert columns to the proper data type
powerConsumptionDF$Global_active_power <- as.numeric(powerConsumptionDF$Global_active_power)
powerConsumptionDF$MeasurementDateTime <- as.POSIXct(powerConsumptionDF$MeasurementDateTime, format = "%d/%m/%Y %H:%M:%S")

# Convert Date and Time to the proper data types
powerConsumptionDF$Date <- as.Date(powerConsumptionDF$Date, "%d/%m/%Y")

# Filter the data set and use records where 2/1/2007 <= Date <= 2/2/2007
data <- filter(powerConsumptionDF, Date >= as.Date("2/1/2007", "%m/%d/%Y") & Date <= as.Date("2/2/2007", "%m/%d/%Y"))

# Write the Time Series to a png file
png("plot2.png", height = 480, width = 480)
ggplot(data, aes(x = MeasurementDateTime, y = Global_active_power)) + geom_line() +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
        labs(x = "", y = "Global Active Power (kilowatts)") +
        theme_bw() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black"),
              axis.text.y = element_text(angle = 90))
dev.off()
