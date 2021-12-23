library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
library(parzer)
fires_train <- read_csv("fires_train.csv")

# Task 1
View(fires_train)
fires_train <- fires_train %>% select(-c(id, alert_source))
fires_train <- fires_train %>% mutate(region = na_if(region, "-"))
fires_train <- fires_train %>% mutate(lat=parse_lat(lat), lon=parse_lon(lon))
fires_train <- fires_train %>% mutate(lat = na_if(lat, "NaN"), lon = na_if(lon, "NaN"))

#See if there´s any NA
sum(is.na(fires_train$alert_date))
fires_train <- fires_train %>% mutate(alert_date=format(alert_date,format="%Y/%m/%d"))

fires_train <- fires_train %>% mutate(extinction_date=format(extinction_date,format="%Y/%m/%d"))
fires_train <- fires_train %>% mutate(firstInterv_date=format(firstInterv_date,format="%Y/%m/%d"))
sum(is.na(fires_train$firstInterv_date))