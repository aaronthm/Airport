library(psych)
library(MASS)
library(corrplot)
library(dplyr)
library(ggplot2)

# Read raw data
## The column name was fixed so no strange "X." prefixes
raw_data <- read.csv("airline_delay_causes.csv")

# Leave only the columns needed
## Group the rows from the same airport into a single record
## Sum up the total flight counts and delayed time
## Omit the N/A values
clean_data <- raw_data %>%
  group_by(year, month, carrier, carrier_name, airport, airport_name) %>% 
  dplyr::select(year, month, carrier, carrier_name, airport, airport_name, arr_flights, arr_del15, carrier_ct,
                      weather_ct, nas_ct, late_aircraft_ct, arr_delay, security_ct) %>% 
  summarise(arr_flights = sum(arr_flights), arr_del15 = sum(arr_del15), carrier_ct = sum(carrier_ct), 
            weather_ct = sum(weather_ct), nas_ct = sum(nas_ct), late_aircraft_ct = sum(late_aircraft_ct), 
            arr_delay = sum(arr_delay), security_ct=sum(security_ct)) %>% 
  na.omit()

# Calculate the min, max, median, and mean.
describe(clean_data)

# Total flights by airport
## All airports
## Sort by descending number of flights
all_airport = clean_data %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  arrange(desc(sum_arr_flight))

ggplot(all_airport, aes(x = airport, y = sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Top 50
top_50 = clean_data %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  filter(rank(desc(sum_arr_flight)) <= 50)

ggplot(top_50, aes(x = reorder(airport, -sum_arr_flight), y = sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 50 Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 0.5))

# Top 10
top_10 = clean_data %>% 
  group_by(airport) %>%
  summarise(sum_arr_flight = sum(arr_flights)) %>%
  filter(rank(desc(sum_arr_flight)) <= 10)

ggplot(top_10, aes(x = reorder(airport, -sum_arr_flight), y = sum_arr_flight)) + geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Frequently Delayed Airports") + 
  xlab("Airports") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
  

# Correlation Plot 
correlation <- cor(clean_data[c('arr_flights', 'arr_del15', 'carrier_ct',
                                'weather_ct', 'nas_ct', 'late_aircraft_ct', 'arr_delay')])
corrplot.mixed(correlation)

# Regression
## Select only the top 10 airport
## Turn the year and month into dummy variables
## Use dummy variable on year, 0 represents 2017, 1 for 2018
top_10_name <- as.vector(top_10$airport)
top_data <- clean_data %>% 
  group_by(airport) %>%
  subset(airport %in% top_10_name) %>% 
  mutate(year = as.numeric((year == 2018)))

write.csv(top_data, file="top_data.csv")

## A general linear regression model for all the airports combined
## With all variables considered
all_lm <- lm(arr_delay ~ year + month + arr_flights + carrier_ct + weather_ct + nas_ct +
               late_aircraft_ct, data = top_data)
summary(all_lm)
plot(all_lm)

## Now consider AIC for variable selection
## Use backward selection
step <- stepAIC(all_lm, direction = "backward")
step$anova

## Now for each of the top 3 airports
ATL_data <- top_data %>% 
  subset(airport == 'ATL')
ATL_lm <- lm(arr_delay ~ year + month + arr_flights + carrier_ct + weather_ct + nas_ct +
               late_aircraft_ct, data = ATL_data)
summary(ATL_lm)

ORD_data <- top_data %>% 
  subset(airport == 'ORD')
ORD_lm <- lm(arr_delay ~ year + month + arr_flights + carrier_ct + weather_ct + nas_ct +
               late_aircraft_ct, data = ORD_data)
summary(ORD_lm)

DFW_data <- top_data %>% 
  subset(airport == 'DFW')
DFW_lm <- lm(arr_delay ~ year + month + arr_flights + carrier_ct + weather_ct + nas_ct +
               late_aircraft_ct, data = DFW_data)
summary(DFW_lm)
