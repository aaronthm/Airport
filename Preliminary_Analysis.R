# Import library
library(ggplot2)
library(GGally)
require(forcats)

# Import data
raw.data <- read.csv("Delay.csv", check.names = FALSE)

# Copy the data to use
delay.data <- raw.data

# Function to add regression line to the ggpairs plot
add_line <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="red", color="red", ...)
  p
}

# Scatterplot matrix to compare correlation across factors
ggpairs(delay.data[,c(9:13, 16)], lower = list(continuous = add_line))

# Histogram of Airports
ggplot(delay.data, aes(fct_infreq(airport))) +
  geom_bar()
barplot(summary(delay.data$airport))

# Summary Airport Info
sum(summary(delay.data$airport))


# Pie chart for factor percentage by time
carrier.sum <- sum(delay.data$` carrier_delay`, na.rm = TRUE)
weather.sum <- sum(delay.data$weather_delay, na.rm = TRUE)
nas.sum <- sum(delay.data$nas_delay, na.rm = TRUE)
security.sum <- sum(delay.data$security_delay, na.rm = TRUE)
aircraft.sum <- sum(delay.data$late_aircraft_delay, na.rm = TRUE)

pie(c(carrier.sum, weather.sum, nas.sum, security.sum, aircraft.sum), labels = c("Carrier", "Extreme Weather", "NAS", "Security", "Late Aircraft"))
