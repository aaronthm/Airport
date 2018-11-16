#setwd("~/Desktop/INFO370/Final_Project")
library(corrplot)
library(dplyr)
library(ggplot2)

flight_delay <- read.csv("airline_delay_causes.csv")
head(flight_delay)

flight_delay <- flight_delay %>%
  group_by(airport) %>%
  select(year, X.month, carrier, carrier_name, airport, airport_name, arr_flights, arr_del15, carrier_ct,
                      X.weather_ct, nas_ct, late_aircraft_ct, arr_diverted)

by_flight <- flight_delay %>% group_by(airport, year, X.month)

# Calculate the min, max, median, and mean.
summary(by_flight)

# Calculate the standard deviation for each column per airport
by_flight %>%
  group_by(airport) %>%
  select(airport, year, X.month, arr_flights, arr_del15, carrier_ct,
         X.weather_ct, nas_ct, late_aircraft_ct, arr_diverted) %>%
  summarise_all(funs(sd(., na.rm = TRUE)))

## Total occurences of flights delayed per airport
# All airports
airportGrouped = by_flight %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  na.omit(sum_arr_flight)

ggplot(airportGrouped, aes(x=airport, y= sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank())

#top 50
by_airport_top50 = by_flight %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  arrange(desc(sum_arr_flight)) %>%
  head(50)

ggplot(by_airport_top50, aes(x=airport, y= sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 50 Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 0.5))

#top 10
by_airport_top10 = by_flight %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  arrange(desc(sum_arr_flight)) %>%
  head(10)

ggplot(by_airport_top10, aes(x=airport, y= sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
  

# Correlation Plot 
relationships <- by_flight %>%
  group_by(carrier_ct) %>%
  select(year, X.month, arr_flights, arr_del15, carrier_ct,
         X.weather_ct, nas_ct, late_aircraft_ct, arr_diverted)

correlations <- cor(relationships, use = "pairwise.complete.obs")
corrplot(correlations, type = "upper")
