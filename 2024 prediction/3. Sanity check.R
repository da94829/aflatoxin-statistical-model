setwd("C:/Users/dayoung.kim/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/Data/AWIS_weather_data")


  testing <- read_csv("BEN HILL.csv")
testing <- testing %>% 
  mutate(DATE = as.Date(date, format = "%m/%d/%Y")) %>%
  filter(between(DATE, as.Date("2019-01-01"), as.Date("2019-12-01"))) %>% 
  rename(TMAX = `max temperature`, 
         TMIN = `min temperature`, 
         PRCP = `total precipitation`) %>%
  mutate(TMAX = round((TMAX-32) * 5/9,2),
         TMIN = round((TMIN-32) * 5/9,2),
         PRCP = PRCP * 25.7) %>%
  select(DATE, TMAX, TMIN, PRCP) 


z <-process_single_data(testing , equation)

