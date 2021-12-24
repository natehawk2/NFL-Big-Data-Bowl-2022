# Pretty much every method has the same estimates. It splits it 44-107. 
# I think I should add defender distance into the model and then run it again.
# Maybe this will increase accuracy above 77%

library(dplyr)
library(caret)
data = read.csv("https://raw.githubusercontent.com/natehawk2/NFL-Big-Data-Bowl-2022/main/allpunts_plays_fastr.csv")
data$half_seconds_remaining <- ifelse(data$half_seconds_remaining < 90, 1, 0)
data$wind = ifelse(is.na(data$wind), 0, data$wind)

fair_catch <- data[data$event == "fair_catch",] 
punt_received <- data[data$event == "punt_received",]
muffed <- data[data$event == "punt_muffed",]



fair_catch <- fair_catch %>% select(event, playDirection, kickLength, wind, half_seconds_remaining, yardLine)
punt_received <- punt_received %>% select(event, playDirection, kickLength, wind, half_seconds_remaining, yardLine)
#muffed <- muffed %>% select(event, playDirection, kickLength, wind, half_seconds_remaining, yardLine)

combined <- rbind(fair_catch, punt_received)

apply(apply(combined, 2, is.na), 2, mean)
combined = combined[!is.na(combined$kickLength),]

# Logistic Regression
# 0.77 Accuracy

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

summary(log)
confusionMatrix(log)

muffed$event <- NA

pred_probs <- predict(log, newdata = muffed, type = "prob")

preds <- predict(log, newdata = muffed)

muffed$event <- preds

table(muffed$event)




# Random Forest
# 0.777 accuracy
rf <- train(
  event ~ .,
  data = combined,
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    classProbs = TRUE
  )
)

confusionMatrix(rf)

muffed$event <- NA

pred_probs <- predict(log, newdata = muffed, type = "prob")

preds <- predict(log, newdata = muffed)

muffed$event <- preds

table(muffed$event)




# Neural Net
# 0.77 accuracy

nnet <- train(
  event ~ .,
  data = combined,
  method = "nnet",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    classProbs = TRUE
  )
)

confusionMatrix(nnet)

muffed$event <- NA

pred_probs <- predict(log, newdata = muffed, type = "prob")

preds <- predict(log, newdata = muffed)

muffed$event <- preds

table(muffed$event)


# GBM
# 0.78
gbm <- train(
  event ~ .,
  data = combined,
  method = "gbm",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    classProbs = TRUE
  )
)

confusionMatrix(gbm)

muffed$event <- NA

pred_probs <- predict(log, newdata = muffed, type = "prob")

preds <- predict(log, newdata = muffed)

muffed$event <- preds

table(muffed$event)



