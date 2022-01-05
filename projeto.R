library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
library(na.tools)

fires_train <- read_csv("fires_train.csv", na= c("NA","", "-"), col_names = TRUE)

# Task 1: Data importation, clean-up and pre-processing
# Data clean-up and pre-processing steps.

# summary(fires_train)
fires_train <- fires_train %>% select(-c("id", "extinction_date","extinction_hour", "firstInterv_date", "firstInterv_hour", "alert_source"))
fires_train <- fires_train %>% arrange(region) %>% group_by(district) %>% fill(region)


# Task 2: Data exploratory analysis


# Task 3: Predictive modelling

# fires_test <- read_csv("fires_test.csv", na= c("NA","", "-"), col_names = TRUE)

