#setwd("~/Desktop/INFO370/Final_Project")
library(corrplot)
library(dplyr)

flight_delay <- read.csv("airline_delay_causes.csv")
head(flight_delay)
flight_delay <- na.omit(flight_delay)

flight_delay <- flight_delay %>%
  group_by(airport) %>%
  select("year", "X.month", "carrier", "carrier_name","airport", "airport_name", "arr_flights", "arr_del15", "carrier_ct",
                      "X.weather_ct","nas_ct", "late_aircraft_ct", "arr_diverted")

by_flight <- flight_delay %>% group_by(airport, year, X.month)

# Total occurences of flights delayed per airport
airportGrouped <- by_flight %>% 
  group_by(airport_name) %>%
  summarise(sum_arr_flight = sum(arr_flights))

# Calculate the min, max, median, and mean.
summary(by_flight)

# Calculate the standard deviation for each column
flight_delay %>%
  select("airport","arr_flights", "arr_del15", "carrier_ct",
         "X.weather_ct","nas_ct", "late_aircraft_ct", "arr_diverted") %>%
  summarise_all(funs(sd(., na.rm = TRUE)))
  
