library(caret)

hist(binned_data$wpa)
hist(binned_data$yardLine)
table(binned_data$event.y)

binned_data$half_seconds_remaining

binned_data = binned_data %>% mutate(score_difference = 
                                       case_when(
                                         team.x == "home" ~ preSnapHomeScore.x - preSnapVisitorScore.x,
                                         team.x == "away" ~ preSnapVisitorScore.x - preSnapHomeScore.x
                                       )
)

binned_data1 = binned_data %>% filter(abs(epa) < 8)

samp = sample(1:nrow(binned_data1), nrow(binned_data1)*0.8)
train = binned_data1[samp,]
test = binned_data1[-samp,]

binned_data$kickLength.x = 100 -(binned_data$yardLine + binned_data$yardlineNumber.x)

write.csv(binned_data, "play_summaries.csv")

model <- train(
  wpa ~ as.factor(event.y) + yardLine + mean_defender_distance + kickLength.x + quarter.x + half_seconds_remaining + 
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


library(caret)

plays = read.csv("play_summaries.csv")
model <- train(
  epa ~  as.factor(event.y) * yardLine + as.factor(event.y) * mean_defender_distance + as.factor(event.y) *kickLength.x +
    as.factor(event.y) * as.factor(quarter.x) + as.factor(event.y) *half_seconds_remaining + 
    as.factor(event.y) * score_difference ,
  data = plays,
  method = "glm",
  family = "gaussian",
  trControl = trainControl(
    method = "cv", 
    number = 5
  )
)

summary(model)

saveRDS(model, "appdata/epa_model.rds")

summary(model)

preds = predict(model, test)

sqrt(mean((test$epa - preds)^2))

plot(preds - test$epa)

plays %>% filter(is.na(plays$returnerId.x.x)) %>% View()

# Make prediction martrix
yardline_grid <- seq(1,20, by=1) ## make a yardline vector
def_dist_grid <- seq(10,35, by=1) ## make a distance vector
event_grid <- c("punt_land", "fair_catch", "punt_received")
score_diff_grid = -7
seconds_grid = 5
scrimmage_line = 35
quarter_grid = 2



pred_grid <- expand.grid(yardLine = yardline_grid, mean_defender_distance = def_dist_grid, event.y = event_grid, 
                         score_difference = score_diff_grid, half_seconds_remaining = seconds_grid,
                         kickLength.x = kicklength_grid, quarter.x = quarter_grid)

pred_grid = pred_grid %>%
  mutate(kickLength.x = 100 - (yardLine + scrimmage_line)) %>%
  mutate(half_seconds_remaining =
           case_when(
             (quarter.x == 4 | quarter.x == 2) ~ 60*seconds_grid,
             (quarter.x == 3 | quarter.x == 1) ~ 60*seconds_grid + 60*15
           )
  )


matrix.preds = predict(model, pred_grid)


preds_data = cbind(pred_grid, matrix.preds) # %>% filter(yardLine == 1, mean_defender_distance == 20)

ggplot(data = preds_data, mapping = aes(x = yardLine, y = mean_defender_distance, color = matrix.preds)) + 
  geom_point() + 
  facet_grid(~event.y) + 
  labs(title = "EPA for each decision and yardline/defender distance", 
       subtitle = "Up by 10, 3 minutes left in 2nd quarter, 45 yard kick")





###############################################
# Now bin them and get the binned estimates ###
###############################################

# make yardline bins
preds_data_bin <- preds_data %>% mutate(yardlinebin = 
                                                    case_when(
                                                      yardLine < 2 ~ 1,
                                                      yardLine < 4 ~ 2,
                                                      yardLine < 6 ~ 3,
                                                      yardLine < 8 ~ 4,
                                                      yardLine < 10 ~ 5,
                                                      yardLine < 12 ~ 6,
                                                      yardLine < 14~ 7,
                                                      yardLine < 16 ~ 8,
                                                      yardLine < 18 ~ 9,
                                                      yardLine < 21 ~ 10
                                                    )) 




# Make defender distances bins
preds_data_bin <- preds_data_bin %>% mutate(def_distance_bin = 
                                              case_when(
                                                mean_defender_distance < 15 ~ 0,
                                                mean_defender_distance < 20 ~ 1,
                                                mean_defender_distance < 25 ~ 2,
                                                mean_defender_distance < 30 ~ 3,
                                                mean_defender_distance < 64 ~ 4
                                              ))

epa_tbl <- preds_data_bin %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-matrix.preds)) %>% 
  arrange(median_epa, .by_group = TRUE)

# epa_tbl %>% arrange(median_epa, .by_group = TRUE) %>% 
#   dplyr::select(mean_defender_distance, yardLine, playDescription.x, epa, event.y, median_epa) %>%
#   group_by(yardlinebin, def_distance_bin)



# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin)

result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)

# Get second highest in each group
second = epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(2, median_epa) %>% arrange(-median_epa, .by_group = TRUE) %>% 
  slice(2) %>% 
  dplyr::select(yardlinebin, def_distance_bin, median_epa)
names(second)[3] = "median_2"
result_table_epa = result_table_epa %>% left_join(second)
result_table_epa = result_table_epa %>% mutate(difference = median_epa - median_2)
result_table_epa

ggplot(data = result_table_epa %>% filter(!is.na(difference)), mapping = aes(x = yardlinebin, y = def_distance_bin, 
                                                                             fill = event.y, 
                                                                             alpha = difference)) + 
  geom_raster() + labs(title = "What should returners be doing based on EPA",
                       subtitle = "Difference between first and second best choice \
                       Up by 40, 10 seconds left in 4th, 30 yard punt",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned at quantiles")




########## Make plot #############

result_table_epa = result_table_epa %>%   mutate(event.y = case_when(
  event.y == "fair_catch" ~ "Fair Catch",
  event.y == "punt_land" ~ "Let Bounce",
  event.y == "punt_received" ~ "Return Punt")
)
group.colors <- c(`Return Punt` = "#333BFF", `Let Bounce` = "#11D879", `Fair Catch` = "891322")

plot1 = ggplot(
  data = result_table_epa,
  mapping = aes(
    x = yardlinebin,
    y = def_distance_bin,
    fill = event.y,
    alpha = difference
  )
) +
  geom_tile(color = "white") +
  labs(
    title = "What *should* returners be doing?",
    subtitle = "Down by 7, 5 minutes left in 2nd quarter, opposing team punting from 35 yardline\nGradient represents how much better the optimal choice is than the next best choice",
    x = "Yardline \n (binned every 2 yards)",
    y = "Closest Defender Distance\n(2 seconds until ball lands)",
    fill = NULL,
    #caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 14,
                                      face = "bold")) +
  scale_fill_manual(values = group.colors) +
  guides(alpha = FALSE)

plot1
ggsave("plot1.png", plot1, width = 8, height = 4)

# Make prediction martrix
yardline_grid <- seq(1,20, by=1) ## make a yardline vector
def_dist_grid <- seq(10,35, by=1) ## make a distance vector
event_grid <- c("punt_land", "fair_catch", "punt_received")
score_diff_grid = 18
seconds_grid = 15
scrimmage_line = 50
quarter_grid = 4



pred_grid <- expand.grid(yardLine = yardline_grid, mean_defender_distance = def_dist_grid, event.y = event_grid, 
                         score_difference = score_diff_grid, half_seconds_remaining = seconds_grid,
                         kickLength.x = kicklength_grid, quarter.x = quarter_grid)

pred_grid = pred_grid %>%
  mutate(kickLength.x = 100 - (yardLine + scrimmage_line)) %>%
  mutate(half_seconds_remaining =
           case_when(
             (quarter.x == 4 | quarter.x == 2) ~ 60*seconds_grid,
             (quarter.x == 3 | quarter.x == 1) ~ 60*seconds_grid + 60*15
           )
  )


matrix.preds = predict(model, pred_grid)


preds_data = cbind(pred_grid, matrix.preds) # %>% filter(yardLine == 1, mean_defender_distance == 20)

ggplot(data = preds_data, mapping = aes(x = yardLine, y = mean_defender_distance, color = matrix.preds)) + 
  geom_point() + 
  facet_grid(~event.y) + 
  labs(title = "EPA for each decision and yardline/defender distance", 
       subtitle = "Up by 10, 3 minutes left in 2nd quarter, 45 yard kick")



# make yardline bins
preds_data_bin <- preds_data %>% mutate(yardlinebin = 
                                          case_when(
                                            yardLine < 2 ~ 1,
                                            yardLine < 4 ~ 2,
                                            yardLine < 6 ~ 3,
                                            yardLine < 8 ~ 4,
                                            yardLine < 10 ~ 5,
                                            yardLine < 12 ~ 6,
                                            yardLine < 14~ 7,
                                            yardLine < 16 ~ 8,
                                            yardLine < 18 ~ 9,
                                            yardLine < 21 ~ 10
                                          )) 




# Make defender distances bins
preds_data_bin <- preds_data_bin %>% mutate(def_distance_bin = 
                                              case_when(
                                                mean_defender_distance < 15 ~ 0,
                                                mean_defender_distance < 20 ~ 1,
                                                mean_defender_distance < 25 ~ 2,
                                                mean_defender_distance < 30 ~ 3,
                                                mean_defender_distance < 64 ~ 4
                                              ))

epa_tbl <- preds_data_bin %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-matrix.preds)) %>% 
  arrange(median_epa, .by_group = TRUE)

# epa_tbl %>% arrange(median_epa, .by_group = TRUE) %>% 
#   dplyr::select(mean_defender_distance, yardLine, playDescription.x, epa, event.y, median_epa) %>%
#   group_by(yardlinebin, def_distance_bin)



# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin)

result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)

# Get second highest in each group
second = epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(2, median_epa) %>% arrange(-median_epa, .by_group = TRUE) %>% 
  slice(2) %>% 
  dplyr::select(yardlinebin, def_distance_bin, median_epa)
names(second)[3] = "median_2"
result_table_epa = result_table_epa %>% left_join(second)
result_table_epa = result_table_epa %>% mutate(difference = median_epa - median_2)
result_table_epa

ggplot(data = result_table_epa %>% filter(!is.na(difference)), mapping = aes(x = yardlinebin, y = def_distance_bin, 
                                                                             fill = event.y, 
                                                                             alpha = difference)) + 
  geom_raster() + labs(title = "What should returners be doing based on EPA",
                       subtitle = "Difference between first and second best choice \
                       Up by 40, 10 seconds left in 4th, 30 yard punt",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned at quantiles")




########## Make plot #############

result_table_epa = result_table_epa %>%   mutate(event.y = case_when(
  event.y == "fair_catch" ~ "Fair Catch",
  event.y == "punt_land" ~ "Let Bounce",
  event.y == "punt_received" ~ "Return Punt")
)
group.colors <- c(`Return Punt` = "#333BFF", `Let Bounce` = "#11D879", `Fair Catch` = "891322")

plot2 = ggplot(
  data = result_table_epa,
  mapping = aes(
    x = yardlinebin,
    y = def_distance_bin,
    fill = event.y,
    alpha = difference
  )
) +
  geom_tile(color = "white") +
  labs(
    title = "What *should* returners be doing?",
    subtitle = "Up by 18, start of the 4th quarter, opposing team punting from 50 yardline\nGradient represents how much better the optimal choice is than the next best choice",
    x = "Yardline \n (binned every 2 yards)",
    y = "Closest Defender Distance\n(2 seconds until ball lands)",
    fill = NULL,
    #caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 14,
                                      face = "bold")) +
  scale_fill_manual(values = group.colors) +
  guides(alpha = FALSE)

plot2
ggsave("plot2.png", plot2, width = 8, height = 4)




epa_tbl <- plays %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-epa)) %>% 
  arrange(median_epa, .by_group = TRUE)

epa_tbl_only = epa_tbl %>% count(yardlinebin, def_distance_bin, event.y) %>% unique()

result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, n)

second = epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(2, n) %>% arrange(-n, .by_group = TRUE) %>% 
  slice(2) %>% 
  dplyr::select(yardlinebin, def_distance_bin, n)
names(second)[3] = "n_2"
result_table_epa = result_table_epa %>% left_join(second)
result_table_epa = result_table_epa %>% mutate(difference = n - n_2)
result_table_epa

result_table_epa = result_table_epa %>%   mutate(event.y = case_when(
  event.y == "fair_catch" ~ "Fair Catch",
  event.y == "punt_land" ~ "Let Bounce",
  event.y == "punt_received" ~ "Return Punt")
)
group.colors <- c(`Return Punt` = "#333BFF", `Let Bounce` = "#11D879", `Fair Catch` = "891322")


plot3 = ggplot(
  data = result_table_epa,
  mapping = aes(
    x = yardlinebin,
    y = def_distance_bin,
    fill = event.y,
    alpha = difference
  )
) +
  geom_tile(color = "white") +
  labs(
    title = "What *are* returners doing?",
    subtitle = "Gradient represents the relative frequency of the decision",
    x = "Yardline \n (binned every 2 yards)",
    y = "Closest Defender Distance\n(2 seconds until ball lands)",
    fill = NULL,
    #caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 14,
                                      face = "bold")) +
  scale_fill_manual(values = group.colors) +
  guides(alpha = FALSE)

plot3
ggsave("plot3.png", plot3, width = 8, height = 4)




###################
# Jacob Model Code#
###################
library(tidymodels)
# Split into training and test sets
bdb_split <- initial_split(binned_data,
                           prop = 9/10)
train_tbl <- training(bdb_split)
test_tbl <- testing(bdb_split)
# Make folds for cross validation
bdb_folds <- vfold_cv(train_tbl)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
# In-sample cross validation
lm_cv <- fit_resamples(
  object = lm_spec,
  preprocessor = epa ~  event.y*(yardLine + mean_defender_distance +
                                   kickLength.x + as.factor(quarter.x) +
                                   half_seconds_remaining + score_difference),
  resamples = bdb_folds,
  metrics = metric_set(rmse, rsq),
  save_pred = TRUE
)
bdb_folds


lm_cv %>% collect_metrics()
lm_fit <- lm_spec %>%
  fit(epa ~  event.y*(yardLine + mean_defender_distance +
                        kickLength.x + as.factor(quarter.x) +
                        half_seconds_remaining + score_difference),
      data = binned_data)

saveRDS(lm_fit, "appdata/epa_model.rds")

tidy(lm_fit) %>% View()

augment(MODEL, new_data = NEWDATA)










##################################
## Quantitative regression code###
######################################

library(quantreg)
multi_rqfit <- rq(epa ~as.factor(event.y) * yardLine + as.factor(event.y) * mean_defender_distance + as.factor(event.y) *kickLength.x +
                    as.factor(event.y) *quarter.x + as.factor(event.y) * half_seconds_remaining + 
                    as.factor(event.y) * score_difference,
                  data = binned_data, tau = 0.9)





# Create a new situation

# Make prediction martrix
yardline_grid <- seq(1,20, by=1) ## make a yardline vector
def_dist_grid <- seq(1,60, by=1) ## make a distance vector
event_grid <- c("punt_land", "fair_catch", "punt_received")
score_diff_grid = 21
seconds_grid = 60
kicklength_grid = 50
quarter_grid = 4



pred_grid <- expand.grid(yardLine = yardline_grid, mean_defender_distance = def_dist_grid, event.y = event_grid, 
                         score_difference = score_diff_grid, half_seconds_remaining = seconds_grid,
                         kickLength.x = kicklength_grid, quarter.x = quarter_grid)
table(pred_grid$score_difference)


matrix.preds = predict(model, pred_grid)


preds_data = cbind(pred_grid, matrix.preds) # %>% filter(yardLine == 1, mean_defender_distance == 20)

ggplot(data = preds_data, mapping = aes(x = yardLine, y = mean_defender_distance, color = matrix.preds)) + 
  geom_point() + 
  facet_grid(~event.y) + 
  labs(title = "EPA for each decision and yardline/defender distance", 
       subtitle = "Down by 21, 10 minutes left in 4th quarter, 50 yard punt")


preds_data_bin <- preds_data %>% mutate(yardlinebin = 
                                          case_when(
                                            yardLine < 2 ~ 1,
                                            yardLine < 4 ~ 2,
                                            yardLine < 6 ~ 3,
                                            yardLine < 8 ~ 4,
                                            yardLine < 10 ~ 5,
                                            yardLine < 12 ~ 6,
                                            yardLine < 14~ 7,
                                            yardLine < 16 ~ 8,
                                            yardLine < 18 ~ 9,
                                            yardLine < 21 ~ 10
                                          )) 




# Make defender distances bins
preds_data_bin <- preds_data_bin %>% mutate(def_distance_bin = 
                                              case_when(
                                                mean_defender_distance < 15 ~ 0,
                                                mean_defender_distance < 20 ~ 1,
                                                mean_defender_distance < 25 ~ 2,
                                                mean_defender_distance < 30 ~ 3,
                                                mean_defender_distance < 64 ~ 4
                                              ))

epa_tbl <- preds_data_bin %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-matrix.preds)) %>% 
  arrange(median_epa, .by_group = TRUE)

# epa_tbl %>% arrange(median_epa, .by_group = TRUE) %>% 
#   dplyr::select(mean_defender_distance, yardLine, playDescription.x, epa, event.y, median_epa) %>%
#   group_by(yardlinebin, def_distance_bin)



# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin)

result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)

# Get second highest in each group
second = epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(2, median_epa) %>% arrange(-median_epa, .by_group = TRUE) %>% 
  slice(2) %>% 
  dplyr::select(yardlinebin, def_distance_bin, median_epa)
names(second)[3] = "median_2"
result_table_epa = result_table_epa %>% left_join(second)
result_table_epa = result_table_epa %>% mutate(difference = median_epa - median_2)
result_table_epa

ggplot(data = result_table_epa %>% filter(!is.na(difference)), mapping = aes(x = yardlinebin, y = def_distance_bin, 
                                                                             fill = event.y, 
                                                                             alpha = difference)) + 
  geom_raster() + labs(title = "What should returners be doing based on EPA",
                       subtitle = "Difference between first and second best choice \
                       Up by 40, 10 seconds left in 4th, 30 yard punt",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned at quantiles")
