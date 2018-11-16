#setwd("~/Desktop/INFO370/Final_Project")
library(corrplot)
library(dplyr)

flight_delay <- read.csv("airline_delay_causes.csv")
head(flight_delay)

flight_delay <- flight_delay %>%
  group_by(airport) %>%
  select("year", "X.month", "carrier", "carrier_name","airport", "airport_name", "arr_flights", "arr_del15", "carrier_ct",
                      "X.weather_ct","nas_ct", "late_aircraft_ct", "arr_diverted")

by_flight <- flight_delay %>% group_by(airport, year, X.month)

# Calculate the min, max, median, and mean.
summary(by_flight)

# Calculate the standard deviation for each column
by_flight %>%
  select("year", "X.month", "airport","arr_flights", "arr_del15", "carrier_ct",
         "X.weather_ct","nas_ct", "late_aircraft_ct", "arr_diverted") %>%
  summarise_all(funs(sd(., na.rm = TRUE)))

## Total occurences of flights delayed per airport
# All airports
airportGrouped = by_flight %>% 
  group_by(airport_name) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  na.omit(sum_arr_flight)

barplot(airportGrouped$sum_arr_flight, xlab = 'Airport', ylab = 'Flight Delayed', 
        main = 'Flight Delayed for Airports')

#top 50
by_airport_top50 = by_flight %>% 
  group_by(airport_name) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  filter(rank(desc(sum_arr_flight))<= 50)

barplot(by_airport_top50$sum_arr_flight, xlab = 'Airport', ylab = 'Flight Delayed', 
        main = 'Flight Delayed for Airports')

#top 10
by_airport_top10 = by_flight %>% 
  group_by(airport_name) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  filter(rank(desc(sum_arr_flight))<=10)

barplot(by_airport_top10$sum_arr_flight, xlab = 'Airport', ylab = 'Flight Delayed', 
        main = 'Flight Delayed for Airports')

# Correlation Plot 
relationships <- by_flight %>%
  group_by(carrier_ct) %>%
  select(year, X.month, arr_flights, arr_del15, carrier_ct,
         X.weather_ct, nas_ct, late_aircraft_ct, arr_diverted)

correlations <- cor(relationships, use = "pairwise.complete.obs")
corrplot(correlations, type = "upper")
