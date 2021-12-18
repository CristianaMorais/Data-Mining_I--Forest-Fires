library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
fires_train <- read_csv("fires_train.csv")

# Task 1
View(fires_train)
fires_train <- fires_train %>% select(-c(id, region, alert_source))
# fires_train <- fires_train %>% mutate(region = na_if(region, "-"))

#See if there´s any NA
sum(is.na(fires_train$alert_date))
fires_train <- fires_train %>% mutate(alert_date=format(alert_date,format="%Y/%m/%d"))