library(tidyverse)
library(caret)
library(dplyr)
library(na.tools)
library(caret)
library(lubridate)

fires_train2 <- read_csv("fires_train2.csv", na= c("NA","", "-"), col_names = TRUE)
fires_test2 <- read_csv("fires_test2.csv", na= c("NA","", "-"), col_names = TRUE)


######################### Task 3: Predictive modeling #########################
aux.knn <- fires_train2
aux2.knn <- fires_test2

# aux.knn$lat <- as.data.frame(scale(aux.knn[,c(6)])) #normalizar lat e lon
# aux.knn$lon <- as.data.frame(scale(aux.knn[,c(7)]))



aux.knn$region <- factor(aux.knn$region )
aux.knn$district <- factor(aux.knn$district )
aux.knn$municipality <- factor(aux.knn$municipality )
aux.knn$parish <- factor(aux.knn$parish )
aux.knn$origin <- factor(aux.knn$origin )
aux.knn$intentional_cause <- factor(aux.knn$intentional_cause )
aux.knn$lat <- factor(aux.knn$lat )
aux.knn$lon <- factor(aux.knn$lon )
#aux.knn$tmax <- factor(aux.knn$tmax)


aux2.knn$region <- factor(aux2.knn$region )
aux2.knn$district <- factor(aux2.knn$district )
aux2.knn$municipality <- factor(aux2.knn$municipality )
aux2.knn$parish <- factor(aux2.knn$parish )
aux2.knn$origin <- factor(aux2.knn$origin )
aux2.knn$lat <- factor(aux2.knn$lat )
aux2.knn$lon <- factor(aux2.knn$lon )
#aux2.knn$tmax <- factor(aux2.knn$tmax)

aux.knn <- subset(aux.knn, select=-c(id, parish, municipality, lat, lon))


knn.model <- knn3(intentional_cause ~., data = aux.knn, k = 7)

preds.knn <- predict(knn.model,aux2.knn,type= "class")



######################### Task 4: Kaggle Competition ##########################

submission <- data.frame(matrix(ncol=0, nrow=length(fires_test2$id)))
submission$id <- fires_test2$id
submission$intentional_cause <- 0
submission$intentional_cause <- preds.knn
#sumbmissionKNN <- submission %>% select(-c("intentional_cause.0"))



write.csv(submission , "submissionKNN.csv",row.names = FALSE)
