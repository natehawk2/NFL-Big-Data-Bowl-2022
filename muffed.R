library(dplyr)
library(caret)

data <- read.csv("punts_plays_fastr.csv")

fair_catch <- data[data$event == "fair_catch",] 
punt_received <- data[data$event == "punt_received",]
muffed <- data[data$event == "punt_muffed",]

fair_catch <- fair_catch %>% select(event, playDirection, kickLength, kickReturnYardage)
punt_received <- punt_received %>% select(event, playDirection, kickLength, kickReturnYardage)
muffed <- muffed %>% select(event, playDirection, kickLength, kickReturnYardage)

combined <- rbind(fair_catch, punt_received)

combined$kickReturnYardage <- ifelse(is.na(combined$kickReturnYardage), 0, combined$kickReturnYardage)

muffed$kickReturnYardage <- ifelse(is.na(muffed$kickReturnYardage), 0, muffed$kickReturnYardage)

log <- train(
  event ~ .,
  data = combined,
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    classProbs = TRUE
  )
)

muffed$event <- NA

pred_probs <- predict(log, newdata = muffed, type = "prob")

preds <- predict(log, newdata = muffed)

muffed$event <- preds
