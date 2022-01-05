library(readr)
library(dplyr)
library(tidyverse)
library(dlookr)
library(na.tools)

# flag = 1 -> fires_train ; flag = 2 -> fires_test
clean_pre_processing_data <- function(file, flag) {
  
  # We need id for fires_test
  if(flag == 1)
    file <- file %>% select(-c("id", "extinction_date","extinction_hour", "firstInterv_date", "firstInterv_hour", "alert_source"))
  
  else
    file <- file %>% select(-c("extinction_date","extinction_hour", "firstInterv_date", "firstInterv_hour", "alert_source"))
  
  # Fill the missing values
  file <- file %>% arrange(region) %>% group_by(district) %>% fill(region)
  
  # Convert the alert_date for the correct format
  file <- file %>% mutate(alert_date=format(alert_date,format="%Y-%m-%d"))
  
  
  # Return the file "clean"
  return (file)
}

# Task 1: Data importation, clean-up and pre-processing
# Data clean-up and pre-processing steps.

fires_train <- read_csv("fires_train.csv", na= c("NA","", "-"), col_names = TRUE)
fires_test <- read_csv("fires_test.csv", na= c("NA","", "-"), col_names = TRUE)

# summary(fires_train)
# Preparing the files for Task 2
fires_train <- clean_pre_processing_data(fires_train, 1)
fires_test <- clean_pre_processing_data(fires_test, 2)


# Task 2: Data exploratory analysis


# Task 3: Predictive modelling