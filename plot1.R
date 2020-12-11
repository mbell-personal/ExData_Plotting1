library(dplyr)

# Load the file
powerConsumptionDF <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)

# Convert columns to the proper data type
powerConsumptionDF$Global_active_power <- as.numeric(powerConsumptionDF$Global_active_power)
powerConsumptionDF$Date <- as.Date(powerConsumptionDF$Date, "%d/%m/%Y")

# Filter the data set and use records where 2/1/2007 <= Date <= 2/2/2007
data <- filter(powerConsumptionDF, Date >= as.Date("2/1/2007", "%m/%d/%Y") & Date <= as.Date("2/2/2007", "%m/%d/%Y"))

# Write the Histogram to a png file
png("plot1.png", height = 480, width = 480)
hist(data$Global_active_power, ylim = c(0, 1200), xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
dev.off()