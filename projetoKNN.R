
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

#aux.knn <- fires_train2 %>% mutate_if(is.character,as.factor)
#aux.knn <- fires_train2 %>% mutate_if(is.numeric,as.factor)

aux.knn$region <- factor(fires_train2$region, ordered=TRUE)
aux.knn$district <- factor(fires_train2$district, ordered=TRUE)
aux.knn$municipality <- factor(fires_train2$municipality, ordered=TRUE)
aux.knn$parish <- factor(fires_train2$parish, ordered=TRUE)
aux.knn$origin <- factor(fires_train2$origin, ordered=TRUE)
aux.knn$intentional_cause <- factor(fires_train2$intentional_cause, ordered=TRUE)
aux.knn$lat <- factor(fires_train2$lat, ordered=TRUE)
aux.knn$lon <- factor(fires_train2$lon, ordered=TRUE)


#aux2.knn <- fires_test2 %>% mutate_if(is.character,as.factor)
#aux2.knn <- fires_test2 %>% mutate_if(is.numeric,as.factor)

aux2.knn$region <- factor(fires_test2$region, ordered=TRUE)
aux2.knn$district <- factor(fires_test2$district, ordered=TRUE)
aux2.knn$municipality <- factor(fires_test2$municipality, ordered=TRUE)
aux2.knn$parish <- factor(fires_test2$parish, ordered=TRUE)
aux2.knn$origin <- factor(fires_test2$origin, ordered=TRUE)
aux2.knn$lat <- factor(fires_test2$lat, ordered=TRUE)
aux2.knn$lon <- factor(fires_test2$lon, ordered=TRUE)


set.seed(123)
input_train <- createDataPartition(y=fires_train2$intentional_cause, p=1, list=FALSE)
aux.knn <- fires_train2 %>% slice(input_train)
aux.knn <- subset(fires_train2, select=-c(id, parish, municipality, lat, lon))





knn.model <- knn3(intentional_cause ~., data = aux.knn, k = 7)

preds.knn <- predict(knn.model,aux2.knn)



######################### Task 4: Kaggle Competition ##########################

submission <- data.frame(matrix(ncol=0, nrow=length(fires_test2$id)))
submission$id <- fires_test2$id
submission$intentional_cause <- 0
submission$intentional_cause <- preds.knn
# submissionKNN <- submission[-c(3)]


write.csv(submission , "submissionKNN2.csv",row.names = FALSE)
