library(dplyr)

top.data <- read.csv("top_data.csv")

top.list <- unique(top.data$airport)

selected.data <- top.data %>% 
  subset(airport == "SFO")

selected.lm <- lm(arr_delay ~ year + month + carrier_ct + weather_ct + nas_ct +
                    late_aircraft_ct + security_ct, data = selected.data)
summary(selected.lm)
