library(tidyverse)
library(caret)
library(dplyr)
library(na.tools)
library(caret)
library(lubridate)
library(naivebayes)

fires_train2 <- read_csv("fires_train2.csv", na= c("NA","", "-"), col_names = TRUE)
fires_test2 <- read_csv("fires_test2.csv", na= c("NA","", "-"), col_names = TRUE)


######################### Task 3: Predictive modeling #########################
aux.bay <- fires_train2
aux2.bay <- fires_test2


aux.bay <- aux.bay %>% select(-c("id","total_area","lat", "lon", "alert_date", "alert_hour", "village_area", "village_veget_area"))
aux.bay$region <- factor(aux.bay$region, ordered=TRUE )
aux.bay$district <- factor(aux.bay$district, ordered=TRUE )
aux.bay$municipality <- factor(aux.bay$municipality , ordered=TRUE)
aux.bay$parish <- factor(aux.bay$parish, ordered=TRUE )
aux.bay$origin <- factor(aux.bay$origin, ordered=TRUE )
aux.bay$intentional_cause <- factor(aux.bay$intentional_cause , ordered=TRUE)
aux.bay$tmax <- factor(aux.bay$tmax, ordered=TRUE )
aux.bay$timePeriod <- factor(aux.bay$timePeriod, ordered=TRUE )

# sapply(aux2.bay$region, typeof)
# class(aux2.bay$region)
# typeof(aux2.bay$region)


aux2.bay$region <- factor(aux2.bay$region ,ordered=TRUE)
aux2.bay$district <- factor(aux2.bay$district ,ordered=TRUE)
aux2.bay$municipality <- factor(aux2.bay$municipality ,ordered=TRUE)
aux2.bay$parish <- factor(aux2.bay$parish ,ordered=TRUE)
aux2.bay$lat <- factor(aux2.bay$lat ,ordered=TRUE)
aux2.bay$lon <- factor(aux2.bay$lon ,ordered=TRUE)
aux2.bay$origin <- factor(aux2.bay$origin ,ordered=TRUE)
aux2.bay$alert_date <- ymd(aux2.bay$alert_date)
aux2.bay$alert_hour <- hms(aux2.bay$alert_hour)
aux2.bay$tmax <- factor(aux2.bay$tmax,ordered=TRUE )
aux2.bay$timePeriod <- factor(aux2.bay$timePeriod, ordered=TRUE )


nb.model <- naive_bayes(intentional_cause ~., data = aux.bay, laplace=1)

preds.nb <- predict(nb.model,aux2.bay, type= "class")




######################### Task 4: Kaggle Competition ##########################

submission <- data.frame(matrix(ncol=0, nrow=length(fires_test2$id)))
submission$id <- fires_test2$id
submission$intentional_cause <- 0
submission$intentional_cause <- preds.nb


write.csv(submission , "submissionBAYES.csv",row.names = FALSE)

