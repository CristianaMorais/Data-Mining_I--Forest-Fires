library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
fires_train <- read_csv("fires_train.csv")
View(fires_train)

# Task 1
fires_train <- fires_train %>% select(-c(id, alert_source))
fires_train <- fires_train %>% mutate(region = na_if(region, "-"))
View(fires_train)
