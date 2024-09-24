# Premium Peanut Weather data Cleaning 

# required packages
library(readr)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)

# import weather data
original_data <- read_csv("Weather data 8.15 to 9.23.csv")

# transform data
original_data <- original_data %>%  
  mutate(DATE = as.Date(Date, format = "%m/%d/%Y")) %>% 
  rename(TMAX = `High`, 
         TMIN = `Low`, 
         PRCP = `Total Precipitation`) %>%
  mutate(TMAX = round((TMAX-32) * 5/9, 2),
         TMIN = round((TMIN-32) * 5/9, 2),
         PRCP = PRCP * 25.7) %>% 
  select(Name, DATE, TMAX, TMIN, PRCP) %>% 
  na.omit() %>%
  distinct(Name, DATE, TMAX, TMIN, PRCP, .keep_all = TRUE)


# PGT20 (percentage greater than 20ppb of aflatoxin) prediction 

# best-performing model 
equation <- "PGT20 = -114.45 +  MinT4wk*-10.26 + MinT2wk*8.04 + MaxT5wk*26.26 + Rain4wk*-0.86 +  MaxT4wk*-20.87"

# Define the functions
# the function for calculating PGT20 value 
PGT20_prop_ossrr <- function(df, equation) {
  # Extract the response variable from the equation
  response_var <- strsplit(equation, "=")[[1]][1]
  
  # Extract the predictor variables from the equation
  predictor_vars <- strsplit(equation, "=")[[1]][2]
  predictor_varq <- gsub("[/^]", "", predictor_vars)
  predictor_vart <- unlist(strsplit(predictor_varq, "[+_\\*]"))
  predictor_final<- gsub("\\s+", "", predictor_vart)
  
  # Check if each element starts with a number
  is_numeric <- grepl("^[0-9-]", predictor_final)
  
  # Convert numeric elements to numeric
  char_coff<- as.numeric(predictor_final[is_numeric])
  
  # Replace non-numeric elements with variable names
  pr_vrbl <- gsub("[^[:alpha:][:digit:]]", "", 
                  predictor_final[!is_numeric])
  
  # Initialize variables
  coef_first <- char_coff[1] 
  coef_list <- char_coff[-1] 
  pred_vars <- pr_vrbl
  predicted <- char_coff[1] 

  
  # Loop to calculate the predicted values
  for (i in 1:length(coef_list)) {
    predicted <- predicted + coef_list[i] * df[, pred_vars[i]] 
  }
  
  return(predicted)
}

# the function for calculating metrics based on data 
calculate_metrics <- function(df) {
  df <- df %>%
    group_by(Name) %>% 
    mutate(
      MinT4wk = rollapply(TMIN, width = 28, FUN = mean, fill = NA, align = "right"),
      MinT2wk = rollapply(TMIN, width = 14, FUN = mean, fill = NA, align = "right"),
      MaxT5wk = rollapply(TMAX, width = 35, FUN = mean, fill = NA, align = "right"),
      Rain4wk = rollapply(PRCP, width = 28, FUN = sum, fill = NA, align = "right"),
      MaxT4wk = rollapply(TMAX, width = 28, FUN = mean, fill = NA, align = "right")
    )
  return(df)
}


# the function for adding the PGT20 to weather dataset
process_single_data <- function(df, equation) {
  df <- calculate_metrics(df)
  df$PGT20 <- PGT20_prop_ossrr(df, equation)
  df$PGT20 <- ifelse(df$PGT20 < 0, 0, df$PGT20)
  return(df)
}


# generate the predicted PGT20 values using the functions
dataset <- process_single_data(original_data , equation)

new <- dataset %>% 
  na.omit() 
new$PGT20 <- as.numeric(new$PGT20)

timestamp <- format(Sys.time(), "%Y%m%d")
dataname <- paste0("data_", timestamp, ".csv")

# save the clean PGT20 data
write_csv(new, dataname) 

# Clean up county names in dataset 
data <- new %>% 
  mutate(Name = tolower(gsub(" (GA|SC)$", "", Name))) # Remove " GA" and convert to lowercase 

library(sf)
library(maps)

# Get the county map data for Georgia and South Carolina
county_map <- map_data("county")
# Filter for Georgia and South Carolina 
ga_sc_counties <- county_map[county_map$region %in% c("georgia", "south carolina"), ]

data <- data %>% rename(subregion = Name) 

map_data <- ga_sc_counties %>% 
  left_join(data, by = "subregion") %>% 
  na.omit()

## plotting
graph <- ggplot() +
  geom_polygon(data = ga_sc_counties, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black")+
  geom_polygon(data =map_data, aes(x =long, y = lat, group = group, fill = PGT20), color = "black") + 
  coord_fixed(1.3) + 
  scale_fill_gradientn(colors = c("yellow", "blue"), limits = c(0, 100)) + 
  theme_void() + 
  labs(title = "Daily PGT20 Prediction by county",
       fill = "PGT20 (%)") +
  facet_wrap(~DATE)

graph  

timestamp <- format(Sys.time(), "%Y%m%d")
filename <- paste0("plot_", timestamp, ".png")

# Save the plot with the generated filename
ggsave(filename = filename, plot = graph, 
       width = 170, height = 150, units = "mm")
