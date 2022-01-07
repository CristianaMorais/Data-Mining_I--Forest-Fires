library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
library(parzer)
library('rnoaa')
library(base)
require(devtools)
require(graphics)

clean_pre_processing_data <- function(file) {
  
  file <- file %>% select(-c("extinction_date","extinction_hour", "firstInterv_date", "firstInterv_hour", "alert_source"))
  
  # Fill the missing values
  file <- file %>% arrange(region) %>% group_by(district) %>% fill(region)
  
  # Normalize the areas values
  #file$village_area <- file %>% mutate(village_area = as.numeric(transform(village_area, method = "minmax"))) %>% pull(village_area)
  #file$vegetation_area <- file %>% mutate(vegetation_area = as.numeric(transform(vegetation_area, method = "minmax"))) %>% pull(vegetation_area)
  #file$farming_area <- file %>% mutate(farming_area = as.numeric(transform(farming_area, method = "minmax"))) %>% pull(farming_area)
  #file$village_veget_area <- file %>% mutate(village_veget_area = as.numeric(transform(village_veget_area, method = "minmax"))) %>% pull(village_veget_area)
  file$total_area <- file %>% mutate(total_area = as.numeric(transform(total_area, method = "minmax"))) %>% pull(total_area)
  
  # Format the latitude and longitude
  file <- file %>% mutate(lat=parse_lat(lat), lon=parse_lon(lon))
  file <- file %>% mutate(lat = na_if(lat, "NaN"), lon = na_if(lon, "NaN")) # Converts latitude and longitude to decimal format using the function of the package parzer
  
  # Convert the alert_date for the correct format
  file <- file %>% mutate(alert_date=format(alert_date,format="%Y-%m-%d"))
  
  # Return the file "clean"
  return (file)
  
}


# This function was given in the statement and is in the getTemperatureNOAA.R file
# It has been modified to get the maximum temperatures from the station closest to each location
# The location is sent in the form of latitude and longitude, which we converted earlier
# Get the maximum temperature and insert it in the table in the column "tmax"

get_temperature <- function(tempdata){
  
  options(noaakey = "mqEuOSuAUjyuGlTjVjxxCpzRlbrooRnr")
  load("station_data.Rdata")
  
  for(i in 1: length(tempdata$id)){
    df <- data.frame(
      id = c(tempdata$district[i]), 
      latitude = c(tempdata$lat[i]),
      longitude = c(tempdata$lon[i]),
      stringsAsFactors = FALSE
    )
    
    #Get nearby stations that can provide the mean average temperature (TAVG)
    nearby_stations <-  meteo_nearby_stations(lat_lon_df = df,
                                              station_data = station_data, radius = 1000, 
                                              var = c("TMAX"),
                                              year_min = 2014, year_max = 2015)
    
    #Get TMAX data
    weather_data <- ghcnd_search(nearby_stations[[1]]$id[3], var = c("TMAX") , date_min = tempdata$alert_date[i] , date_max = tempdata$alert_date[i])
    
    temp <- do.call(rbind.data.frame, weather_data['tmax'])
    #print(tempdata$id[i])
    tempdata$tmax[i] <- temp$tmax[1]
    
  }
  return(tempdata)
}


# Task 1: Data importation, clean-up and pre-processing
# Data clean-up and pre-processing steps.

fires_train <- read_csv("fires_train.csv", na= c("NA","", "-"), col_names = TRUE)
# fires_test <- read_csv("fires_test.csv", na= c("NA","", "-"), col_names = TRUE)

# Preparing the files for Task 2
fires_train <- clean_pre_processing_data(fires_train)

fires_train <- fires_train[-c(10:10309), ]

fires_train <- get_temperature(fires_train)

max1 <- max(fires_train$tmax, na.rm = TRUE)
min1 <- min(fires_train$tmax, na.rm = TRUE)

med <- min1:max1
mean(med)
fires_train$tmax <- fires_train %>% imputate_na(tmax,method = "mean")


# Task 2: Data exploratory analysis


# Task 3: Predictive modelling
