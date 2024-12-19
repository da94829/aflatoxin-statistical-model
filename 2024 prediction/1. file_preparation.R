
# Load necessary libraries
library(readr)
library(dplyr)
library(purrr)
library(zoo)

# Define the directories
directories <- c("ALdata", "GAdata", "FLdata", "MSdata")
base_dir <- "C:/Users/dayoung.kim/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/AuburnModel/2024 prediction/" 

#base_dir <- "C:/Users/da94_/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/AuburnModel/2024 prediction/" 


# Function to read all CSV files in a directory
read_all_csvs <- function(sub_dir) { 
  full_path <- file.path(base_dir, sub_dir) 
  files <- list.files(path = full_path, pattern = "*.csv", full.names = TRUE) 
  data_list <- lapply(files, read_csv) 
  names(data_list) <- basename(files) 
  # Optional: assign file names as names in the list 
  return(data_list) 
  }


# Read data from each directory and combine into a named list
data <- directories %>% 
  set_names() %>%
  map(read_all_csvs)



# Georgia 
georgia <- function(data_list){
  data_list %>% 
    map(~ .x %>% 
          mutate(DATE = as.Date(Julian.Day - 1, origin = paste0(Year, "-01-01"))) %>% 
          filter(between(DATE, as.Date("2024-01-01"), Sys.Date())) %>% 
          rename(TMAX = Max.Air.Temperature.C., 
                 TMIN = Min.Air.Temperature.C., 
                 PRCP = Total.Rain.mm.) %>% 
          select(DATE, TMAX, TMIN, PRCP) %>% 
          mutate(TMAX = na.locf(TMAX, na.rm = FALSE),
                 TMIN = na.locf(TMIN, na.rm = FALSE),
                 PRCP = na.locf(PRCP, na.rm = FALSE))
    )
}


# AL 

alabama <- function(data_list){
  data_list %>% 
  map(~ .x %>% 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>% 
    filter(between(DATE, as.Date("2024-01-01"), Sys.Date())) %>% 
    mutate(TMAX = TMAX * 0.1, 
           TMIN = TMIN * 0.1, 
           PRCP = PRCP * 0.1) %>% 
    select(DATE, TMAX, TMIN, PRCP) %>% 
    mutate(TMAX = na.locf(TMAX, na.rm = FALSE),
           TMIN = na.locf(TMIN, na.rm = FALSE),
           PRCP = na.locf(PRCP, na.rm = FALSE))
  )
}

# FL 
florida <- function(data_list){
  data_list %>% 
  map(~ .x %>% 
        mutate(DATE = as.Date(Period, format = "%d-%b-%y")) %>% 
        filter(between(DATE, as.Date("2024-01-01"), Sys.Date())) %>% 
        rename(TMAX = `60cm T max (F)`, 
               TMIN = `60cm T min (F)`, 
               PRCP = `2m Rain tot (in)`) %>%
        mutate(TMAX = round((TMAX-32) * 5/9,2),
               TMIN = round((TMIN-32) * 5/9,2),
               PRCP = PRCP * 25.7) %>%
        select(DATE, TMAX, TMIN, PRCP) %>% 
        mutate(TMAX = na.locf(TMAX, na.rm = FALSE),
               TMIN = na.locf(TMIN, na.rm = FALSE),
               PRCP = na.locf(PRCP, na.rm = FALSE))
  )
}



# MS 
mississippi <- function(data_list){
  data_list %>% 
    map(~ .x %>% 
          mutate(DATE = as.Date(`Record Date  MM/DD/YYYY`, format = "%m/%d/%Y")) %>%
          filter(between(DATE, as.Date("2024-01-01"), Sys.Date())) %>% 
          rename(TMAX = `Air Temperature Max (Degrees Fahrenheit F)`, 
                 TMIN = `Air Temperature Min (Degrees Fahrenheit F)`, 
                 PRCP = `Precipitation (Inches n.nn)  total rain fall that occurred for the day.`) %>%
          mutate(TMAX = round((TMAX-32) * 5/9,2),
                 TMIN = round((TMIN-32) * 5/9,2),
                 PRCP = PRCP * 25.7) %>%
          select(DATE, TMAX, TMIN, PRCP) %>% 
          mutate(TMAX = na.locf(TMAX, na.rm = FALSE),
                 TMIN = na.locf(TMIN, na.rm = FALSE),
                 PRCP = na.locf(PRCP, na.rm = FALSE))
    )
}

al <- alabama(data$ALdata)
ga <- georgia(data$GAdata)
fl <- florida(data$FLdata)
mi <- mississippi(data$MSdata)

consolidated_data_list <- list(
  Alabama = al,
  Georgia = ga,
  Florida = fl,
  Mississippi = mi
)
