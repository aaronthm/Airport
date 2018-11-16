setwd("~/Desktop/INFO370/Final_Project")
library(corrplot)
library(dplyr)
flight_delay <- read.csv("airline_delay_causes.csv")
head(flight_delay)

flight_delay = select(flight_delay, "year", "X.month", "carrier", "carrier_name","airport", "airport_name", "arr_flights", "arr_del15", "carrier_ct",
                      "X.weather_ct","nas_ct", "late_aircraft_ct", "arr_diverted")

by_flight = flight_delay %>% group_by(airport, year, month)

summary(by_flight)
