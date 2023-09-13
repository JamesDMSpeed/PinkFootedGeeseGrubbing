library(tidyverse)


### Environmental data

PFGDays_01062023 <- read_delim("data/PFGDays_01062023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

may_temp <- PFGDays_01062023 %>% 
  mutate(temp_positive = ifelse((mean_temp > 0), 1, 0)) %>% 
  mutate(station_name= case_when(station == "SN99840" ~ "Longyearbyen",
                                 station == "SN99910" ~ "New_Aalesund")) %>% 
  group_by(station_name, year) %>% 
  summarise(positive_days = sum(temp_positive, na.rm = TRUE))


Location_Valley<-c("Adventdalen", "Alkhornet", "Bohemanflya", "Colesdalen", "Sassendalen", "Semmeldalen", "Brogger")
station_name<-c("Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "Longyearbyen", "New_Aalesund")

weather_match<-as.data.frame(cbind(Location_Valley, station_name))

may_positive_days<-left_join(may_temp, weather_match)

write_csv(may_positive_days, file = "data/may_positive_days.csv")
