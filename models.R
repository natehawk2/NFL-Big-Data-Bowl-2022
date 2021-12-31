library(caret)

hist(binned_data$mean_defender_distance)
hist(binned_data$yardLine)
table(binned_data$event.y)

binned_data$half_seconds_remaining

binned_data = binned_data %>% mutate(score_difference = 
                                       case_when(
                                         team.x == "home" ~ preSnapHomeScore.x - preSnapVisitorScore.x,
                                         team.x == "away" ~ preSnapVisitorScore.x - preSnapHomeScore.x
                                       )
)

binned_data1 = binned_data %>% filter(abs(epa) < 7)

samp = sample(1:nrow(binned_data1), nrow(binned_data1)*0.8)
train = binned_data1[samp,]
test = binned_data1[-samp,]



model <- train(
  epa ~ as.factor(event.y) + yardLine + mean_defender_distance + kickLength.x + quarter.x + half_seconds_remaining + 
    score_difference,
  data = train,
  method = "glm",
  family = "gaussian",
  trControl = trainControl(
    method = "cv", 
    number = 5
  )
)

summary(model)

preds = predict(model, test)

sqrt(mean((test$epa - preds)^2))

plot(preds - test$epa)



model <- train(
  epa ~  as.factor(event.y) * yardLine + as.factor(event.y) * mean_defender_distance + as.factor(event.y) *kickLength.x +
    as.factor(event.y) *quarter.x + as.factor(event.y) *half_seconds_remaining + 
    as.factor(event.y) * score_difference,
  data = train,
  method = "glm",
  family = "gaussian",
  trControl = trainControl(
    method = "cv", 
    number = 5
  )
)

summary(model)

preds = predict(model, test)

sqrt(mean((test$epa - preds)^2))

plot(preds - test$epa)


a = matrix(data = c(1,0,0,10,20,45,4,180,14))
b = matrix(data = c(1,1,0,10,20,45,4,180,14))
c = matrix(data = c(1,0,1,10,20,45,4,180,14))

df = data.frame("event.y" = c("punt_received", "fair_catch", "punt_land"), "mean_defender_distance" = c(3,3,3),
           "half_seconds_remaining" = c(180,180,180), "quarter.x" = c(2,2,2), "yardLine" = c(20,20,20), 
           "kickLength.x" = c(45,45,45), "score_difference" = c(40,40,40))

predict(model, df)

hist(fitted.values(model))
plot(fitted.values(model), residuals(model))



rf <- train(
  epa ~  as.factor(event.y) * yardLine + as.factor(event.y) * mean_defender_distance + as.factor(event.y) *kickLength.x +
    as.factor(event.y) *quarter.x + as.factor(event.y) *half_seconds_remaining + 
    as.factor(event.y) * score_difference,
  data = train,
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5
  )
)

summary(rf)

preds = predict(rf, test)

sqrt(mean((test$epa - preds)^2))

plot(preds - test$epa)


a = matrix(data = c(1,0,0,10,20,45,4,180,14))
b = matrix(data = c(1,1,0,10,20,45,4,180,14))
c = matrix(data = c(1,0,1,10,20,45,4,180,14))

n = 60
df = data.frame("event.y" = c("punt_received", "fair_catch", "punt_land"), "mean_defender_distance" = rep(c(1,),180),
                "half_seconds_remaining" = c(180,180,180), "quarter.x" = c(4,4,4), "yardLine" = rep(seq(1:20),3), 
                "kickLength.x" = c(45,45,45), "score_difference" = c(10,10,10))

predict(model, df)




lm1 = lm(epa ~  as.factor(event.y) * yardLine + as.factor(event.y) * mean_defender_distance + as.factor(event.y) *kickLength.x +
           as.factor(event.y) *quarter.x + as.factor(event.y) *half_seconds_remaining + 
           as.factor(event.y) * score_difference, data = binned_data1)

lm2 = lm(epa ~  as.factor(event.y) * yardLine  + as.factor(event.y) *kickLength.x +
           as.factor(event.y) *quarter.x + as.factor(event.y) *half_seconds_remaining + 
           as.factor(event.y) * score_difference, data = binned_data1)

anova(lm1, lm2)
