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
  #
  #file$village_area <- file %>% mutate(village_area = as.numeric(transform(village_area, method = "minmax"))) %>% pull(village_area)
  #file$vegetation_area <- file %>% mutate(vegetation_area = as.numeric(transform(vegetation_area, method = "minmax"))) %>% pull(vegetation_area)
  #file$farming_area <- file %>% mutate(farming_area = as.numeric(transform(farming_area, method = "minmax"))) %>% pull(farming_area)
  #file$village_veget_area <- file %>% mutate(village_veget_area = as.numeric(transform(village_veget_area, method = "minmax"))) %>% pull(village_veget_area)
  file$total_area <- file %>% mutate(total_area = as.numeric(transform(total_area, method = "minmax"))) %>% pull(total_area)
  
  # Format the latitude and longitude
  file <- file %>% mutate(lat=parse_lat(lat), lon=parse_lon(lon))
  file <- file %>% mutate(lat = na_if(lat, "NaN"), lon = na_if(lon, "NaN"))
  
  # Convert the alert_date for the correct format
  file <- file %>% mutate(alert_date=format(alert_date,format="%Y-%m-%d"))
  
  # Return the file "clean"
  return (file)
  
}

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
    print(tempdata$id[i])
    tempdata$tmax[i] <- temp$tmax[1]
    
  }
  return(tempdata)
}


# Task 1: Data importation, clean-up and pre-processing
# Data clean-up and pre-processing steps.

fires_train <- read_csv("fires_train.csv", na= c("NA","", "-"), col_names = TRUE)

fires_train$lat <- gsub('1900-01-01','',fires_train$lat)

fires_train$lat <- str_replace(fires_train$lat,',','.')
fires_train$lon <- str_replace(fires_train$lon,',','.')
fires_train$lat <- strtrim(fires_train$lat, 9)
fires_train$lon <- strtrim(fires_train$lon, 9)

# Preparing the files for Task 2
fires_train <- clean_pre_processing_data(fires_train)

fires_train$timePeriod <- NA

for(x in 1:length(fires_train$id)) {
  if(isTRUE(fires_train$alert_hour[x]<as.difftime("06:00:00"))){
    fires_train$timePeriod[x] = "Madrugada"
  } 
  else if(isTRUE(fires_train$alert_hour[x]<as.difftime("12:00:00"))){
    fires_train$timePeriod[x] ="Manhã"
  } 
  else if(isTRUE(fires_train$alert_hour[x]<as.difftime("18:00:00"))){
    fires_train$timePeriod[x] ="Tarde"
  } 
  else if(isTRUE(fires_train$alert_hour[x]<=as.difftime("23:59:59"))){
    fires_train$timePeriod[x] = "Noite"
  } 
}

# fires_train <- fires_train[-c(10:10309), ]

# Initialize the column of the temperatures
#fires_train$tmax <- NA

#fires_train <- get_temperature(fires_train)

# Task 2: Data exploratory analysis

# IMP
ggplot(fires_train,aes(x=timePeriod)) + geom_bar() + facet_wrap(~origin) +
  ggtitle("Relation between the day period and the origin of the fires.") + xlab("Period of the day") + ylab("Number of fires")

print(ggplot(fires_train, aes(x=total_area, y=region)) + geom_bar(stat = "identity"))

# Task 3: Predictive modeling

aux <- fires_train %>% select(c(2,3,6,7,8,9,11,12,13,16,17))
aux <- aux %>% mutate_if(is.character,as.factor)
# aux <- fires_train %>% select(c(2,3,6,7,8,9,11,12,13,16,17,18))
aux2 <- fires_train %>% select(c(2,3,6,7,8,9,11,12,13,16,17))
spec(aux2)
summary(aux2)
count(unique(aux2$district))
# Task 4: Kaggle Competition

fires_test <- read_csv("fires_test.csv", na= c("NA","", "-"), col_names = TRUE)
fires_test$intentional_cause <- 0
# fires_test <- fires_test %>% select(-c(2:20))

submission <- data.frame(matrix(ncol=0, nrow=length(fires_test$id)))
submission$id <- fires_test$id
submission$intentional_cause <- 0
write.csv(submission , "submission.csv", row.names=FALSE)