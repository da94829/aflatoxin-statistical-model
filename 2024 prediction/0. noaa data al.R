library(readr)
# NOAA data extraction for AL

Station <- c("USC00013251", "USW00013839", "USW00063872")


for(p in 1:length(Station)){
  url<-paste0("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",Station[p],".csv")
  destfile<-paste0("C:/Users/dayoung.kim/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/AuburnModel/2024 prediction/ALdata/",Station[p],".csv")
  
  res<-try(download.file(url,destfile))
  if(inherits(res,"try-error")){
    next
  } else{
    download.file(url,destfile)
  }
}

# Specify the path to the directory containing the Excel files
path <- "C:/Users/dayoung.kim/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/AuburnModel/2024 prediction/ALdata/"

# List all files in the directory
files <- list.files(path = path, pattern = ".csv", full.names = TRUE)

# Read each CSV file and store them in a list
data_list <- lapply(files, read_csv)

View(data_list[[1]])

names(data_list[[1]])



