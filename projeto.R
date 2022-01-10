library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
library(parzer)
library('rnoaa')
library(base)
require(devtools)
require(graphics)
library(rpart)
library(rpart.plot)
require(randomForest)  
library(randomForest)
library(caret)
library(naivebayes)



clean_pre_processing_data <- function(file) {
  
  file <- file %>% select(-c("extinction_date","extinction_hour", "firstInterv_date", "firstInterv_hour", "alert_source"))
  
  # Fill the missing values
  file <- file %>% arrange(region) %>% group_by(district) %>% fill(region)
  
  # Normalize the areas values
  file$total_area <- file %>% mutate(total_area = as.numeric(transform(total_area, method = "minmax"))) %>% pull(total_area)
  
  file$timePeriod <- NA
  
  for(x in 1:length(file$id)) {
    if(isTRUE(file$alert_hour[x]<as.difftime("06:00:00"))){
      file$timePeriod[x] = "Madrugada"
    } 
    else if(isTRUE(file$alert_hour[x]<as.difftime("12:00:00"))){
      file$timePeriod[x] ="Manhã"
    } 
    else if(isTRUE(file$alert_hour[x]<as.difftime("18:00:00"))){
      file$timePeriod[x] ="Tarde"
    } 
    else if(isTRUE(file$alert_hour[x]<=as.difftime("23:59:59"))){
      file$timePeriod[x] = "Noite"
    } 
  }
  
  file$lat <- gsub('1900-01-01','',file$lat)
  
  file$lat <- str_replace(file$lat,',','.')
  file$lon <- str_replace(file$lon,',','.')
  file$lat <- strtrim(file$lat, 9)
  file$lon <- strtrim(file$lon, 9)
  
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


############ Task 1: Data importation, clean-up and pre-processing ############
# Data clean-up and pre-processing steps.

fires_train <- read_csv("fires_train.csv", na= c("NA","", "-"), col_names = TRUE)
fires_test <- read_csv("fires_test.csv", na= c("NA","", "-"), col_names = TRUE)

#fires_train <- fires_train[-c(50:10309), ]

# Preparing the files for Task 2
fires_train <- clean_pre_processing_data(fires_train)
fires_test <- clean_pre_processing_data(fires_test)



# Initialize the column of the temperatures
fires_train$tmax <- NA
fires_test$tmax <- NA

# Get the temperatures
fires_train <- get_temperature(fires_train)
fires_test <- get_temperature(fires_test)

fires_train$tmax <- fires_train %>%
  imputate_na(tmax,method = "mean")

fires_test$tmax <- fires_test %>%
  imputate_na(tmax,method = "mean")

write.csv(fires_train , "fires_train2.csv",row.names = FALSE)
write.csv(fires_test , "fires_test2.csv",row.names = FALSE)

######################### Task 2: Data exploratory analysis ###################

# IMP
ggplot(fires_train,aes(x=timePeriod)) + geom_bar() + facet_wrap(~origin) +
ggtitle("Relation between the day period and the origin of the fires.") + xlab("Period of the day") + ylab("Number of fires")

print(ggplot(fires_train, aes(x=total_area, y=region)) + geom_bar(stat = "identity"))

######################### Task 3: Predictive modeling #########################

aux <- fires_train2 %>% select(c(2,6,7,8,9,11,12,13,16,18)) # com lat e lon

aux.knn <- fires_train2 %>% select(c(6,7,11,12,13,16,18))
aux.knn <- aux.knn %>% mutate_if(is.character,as.factor)

aux.bay <- fires_train2 %>% select(c(6,7,11,12,13,16,18))
aux.bay <- aux.bay %>% mutate_if(is.character,as.factor)

aux2 <- fires_test2 %>% select(c(2,6,7,8,9,11,12,13,14,17)) #com lat e lon VER ISTO 14 TEM DE SAIR

aux2.knn <- fires_test2 %>% select(c(6,7,11,12,13,17)) #com lat e lon
aux2.knn <- aux2.knn %>% mutate_if(is.character,as.factor)

aux2.bay <- fires_test2 %>% select(c(6,7,11,12,13,17)) #com lat e lon
aux2.bay <- aux2.bay %>% mutate_if(is.character,as.factor)


modelo <- randomForest(intentional_cause ~.,data=aux,ntree=1000,importance=TRUE)
knn.model <- knn3(intentional_cause ~., data = aux.knn, k = 100)
nb.model <- naive_bayes(intentional_cause ~., data = aux.bay)


pred <- predict(modelo,aux2,type="class")
predknn <- predict(knn.model,aux2.knn,type="class")


######################### Task 4: Kaggle Competition ##########################

submission <- data.frame(matrix(ncol=0, nrow=length(fires_test2$id)))
submission$id <- fires_test2$id
submission$intentional_cause <- 0
submission$intentional_cause <- pred
write.csv(submission , "submission.csv", row.names=FALSE)
