#### Directions for this script:
#### Run `Distance Data Nate 1.R` file down to line 120

library(tidyverse)

# Get the mean of the two closest defenders.
def_distances_tbl <- tst2 %>% 
  group_by(gameId, playId, Punt_Return_Team)  %>% 
  filter(Punt_Return_Team == "Punt Team") %>% 
  arrange(returner_distance, .by_group = TRUE)  %>% 
  #dplyr::select(gameId, playId, Punt_Return_Team, returner_distance) %>% 
  slice(1) %>% 
  mutate(mean_defender_distance = mean(returner_distance)) %>% 
  slice(1)

def_distances_tbl <- def_distances_tbl %>% mutate(yardlinebin = 
                                                    case_when(
                                                      yardLine < 2 ~ 1,
                                                      yardLine < 4 ~ 2,
                                                      yardLine < 6 ~ 3,
                                                      yardLine < 8 ~ 4,
                                                      yardLine < 10 ~ 5,
                                                      yardLine < 12 ~ 6,
                                                      yardLine < 14 ~ 7,
                                                      yardLine < 16 ~ 8,
                                                      yardLine < 18 ~ 9,
                                                      yardLine < 21 ~ 10
                                                    ))

summary(def_distances_tbl$mean_defender_distance)

# Make defender distances bins
binned_data <- def_distances_tbl %>% mutate(def_distance_bin = 
                                              case_when(
                                                mean_defender_distance < 15 ~ 0,
                                                mean_defender_distance < 20 ~ 1,
                                                mean_defender_distance < 25 ~ 2,
                                                mean_defender_distance < 30 ~ 3,
                                                mean_defender_distance < 64 ~ 4
                                              ))

table(paste0(binned_data$def_distance_bin, "-", binned_data$yardlinebin))

table(binned_data$def_distance_bin)

# reclassify muffs as either fair catches or punt recieved
binned_data$event.y <- ifelse(binned_data$event.y == "punt_muffed", 
                              "fair_catch", 
                              binned_data$event.y)

# Get mean epa for each group
epa_tbl <- binned_data %>%
  group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-epa)) %>% ## This is what we can change
  arrange(median_epa, .by_group = TRUE)
## Options: quantile(-epa, *choose a quantile*) or mean(-epa)

# Find which one is best in each case
epa_tbl_only <- epa_tbl %>% 
  dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% 
  group_by(yardlinebin, def_distance_bin)

result_table_epa <- epa_tbl_only %>% 
  filter(event.y != "punt_muffed") %>% 
  group_by(yardlinebin, def_distance_bin) %>% 
  top_n(1, median_epa) ## Choose the top value

# Plot showing what returners should be doing based on EPA and
# percentile of risk preference
result_table_epa %>% 
  ggplot(mapping = aes(x = yardlinebin, y = def_distance_bin, fill = event.y)) + 
  geom_tile(color = "white") + 
  labs(title = "What should returners be doing based on EPA",
       subtitle = "Mean of Returns",
       x = "Yardline \n (binned every 2 yards)",
       y = "Closest Defender Distance \n (2 seconds until ball lands)",
       fill = NULL,
       caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.text = element_text(size = 12,
                                 face = "bold")) +
  scale_fill_discrete(name = NULL, 
                      labels = c("Fair Catch", "Let Bounce", "Return Punt"))

##### Same as above, but for WPA

wpa_tbl <- binned_data %>%
  group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_wpa = mean(-wpa)) %>% ## This is what we can change
  arrange(median_wpa, .by_group = TRUE)
## Options: quantile(-epa, *choose a quantile*) or mean(-epa)

# Find which one is best in each case
wpa_tbl_only <- wpa_tbl %>% 
  dplyr::select(median_wpa, yardlinebin, def_distance_bin, event.y)
wpa_tbl_only <- unique(wpa_tbl_only)
wpa_tbl_only %>% 
  group_by(yardlinebin, def_distance_bin)

result_table_wpa <- wpa_tbl_only %>% 
  filter(event.y != "punt_muffed") %>% 
  group_by(yardlinebin, def_distance_bin) %>% 
  top_n(1, median_wpa) ## Choose the top value

# Plot showing what returners should be doing based on EPA and
# percentile of risk preference
result_table_wpa %>% 
  ggplot(mapping = aes(x = yardlinebin, y = def_distance_bin, fill = event.y)) + 
  geom_tile(color = "white") + 
  labs(title = "What should returners be doing based on WPA",
       subtitle = "Mean of Returns",
       x = "Yardline \n (binned every 2 yards)",
       y = "Closest Defender Distance \n (2 seconds until ball lands)",
       fill = NULL,
       caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top") +
  scale_fill_discrete(name = NULL, 
                      labels = c("Fair Catch", "Let Bounce", "Return Punt"))

################################
## Model the data to predict epa
################################

library(tidymodels)
library(discrim)

binned_data <- binned_data %>%
  mutate(
    score_difference = case_when(
      team.x == "home" ~ preSnapHomeScore.x - preSnapVisitorScore.x,
      team.x == "away" ~ preSnapVisitorScore.x - preSnapHomeScore.x
    )
  ) %>% 
  mutate(
    event.y = as.factor(event.y),
    quarter.x = as.factor(quarter.x)
  )

# Split into training and test sets
bdb_split <- initial_split(binned_data,
                           prop = 9/10)

train_tbl <- training(bdb_split)
test_tbl <- testing(bdb_split)

# Make folds for cross validation
bdb_folds <- vfold_cv(train_tbl)

xgboost_spec <- boost_tree(
  mtry = tune(),
  trees = 10000,
  min_n = tune(),
  tree_depth = tune(),
) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

xgboost_tune <- tune_grid(
  xgboost_spec,
  epa ~  yardLine + mean_defender_distance + kickLength.x + 
    quarter.x + half_seconds_remaining + score_difference + event.y,
  resamples = bdb_folds,
  metrics = metric_set(rmse, rsq),
  grid = 10
)

xgboost_tune %>% collect_metrics()

xgboost_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  dplyr::select(mean, min_n, mtry, tree_depth) %>% 
  pivot_longer(min_n:tree_depth,
               values_to = "value",
               names_to = "parameter") %>% 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) + 
  facet_wrap(~ parameter, scales = "free_x") +
  labs(title = "Hyperparameter Tuning for XGBoost Model",
       y = "RMSE")

# try min_n between 15 and 20
# try mtry between 5 and 7
# try tree_depth between 4 and 10

xgboost_grid <- grid_regular(mtry(range = c(5,7)),
                             min_n(range = c(15,20)),
                             tree_depth(range = c(4,10)),
                             levels = 3)

xgboost_tune_adj <- tune_grid(
  xgboost_spec,
  epa ~  yardLine + mean_defender_distance + kickLength.x + 
    quarter.x + half_seconds_remaining + score_difference + event.y,
  resamples = bdb_folds,
  metrics = metric_set(rmse, rsq),
  grid = xgboost_grid
)

xgboost_tune_adj %>% show_best(metric = "rsq")
xgboost_tune_adj %>% select_best(metric = "rmse")

xgboost_tune_adj %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_point(show.legend = FALSE) +
  geom_line(alpha = 0.5, 
            size = 1.5) +
  labs(y = "RMSE")

xgboost_tune_adj %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(min_n = factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_point() +
  geom_line(alpha = 0.5, 
            size = 1.5) +
  labs(y = "R Squared")

# Set best xgboost model

xgboost_spec_final <- boost_tree(
  mtry = 5,
  trees = 10000,
  min_n = 20,
  tree_depth = 10,
) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

xgboost_fit <- xgboost_spec_final %>% 
  fit(epa ~  yardLine + mean_defender_distance + kickLength.x + 
        quarter.x + half_seconds_remaining + score_difference + event.y,
      data = binned_data)

#saveRDS(xgboost_fit, "xgboost_model.rds")

# Now try a basic linear model

lm_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# In-sample cross validation
lm_cv <- fit_resamples(
  object = lm_spec,
  preprocessor = epa ~  event.y*(yardLine + mean_defender_distance + 
                                   kickLength.x + quarter.x + 
                                   half_seconds_remaining + score_difference),
  resamples = bdb_folds,
  metrics = metric_set(rmse, rsq),
  save_pred = TRUE
)

lm_cv %>% collect_metrics()

lm_fit <- lm_spec %>% 
  fit(epa ~  event.y*(yardLine + mean_defender_distance + 
                        kickLength.x + quarter.x + 
                        half_seconds_remaining + score_difference),
      data = binned_data)

tidy(lm_fit) %>% View()

#saveRDS(lm_fit, "lm_model.rds")

#### Test xgboost model in the app ####

yardline_grid <- seq(1,20, by=1) ## make a yardline vector
def_dist_grid <- seq(1,60, by=1) ## make a distance vector
event_grid <- c("punt_land", "fair_catch", "punt_received")
score_diff_grid <- 0
seconds_grid <- 180
kicklength_grid <- 50
quarter_grid <- 2

pred_grid <-
  expand.grid(
    yardLine = yardline_grid,
    mean_defender_distance = def_dist_grid,
    event.y = event_grid,
    score_difference = score_diff_grid,
    half_seconds_remaining = seconds_grid,
    kickLength.x = kicklength_grid,
    quarter.x = quarter_grid
  ) %>% 
  mutate(quarter.x = as.factor(quarter.x))

preds_data <- augment(model1,
                      new_data = pred_grid,
                      interval = "prediction")

preds_data_bin <- preds_data %>%
  mutate(
    yardlinebin =
      case_when(
        yardLine < 2 ~ 1,
        yardLine < 4 ~ 2,
        yardLine < 6 ~ 3,
        yardLine < 8 ~ 4,
        yardLine < 10 ~ 5,
        yardLine < 12 ~ 6,
        yardLine < 14 ~ 7,
        yardLine < 16 ~ 8,
        yardLine < 18 ~ 9,
        yardLine < 21 ~ 10
      )
  )




# Make defender distances bins
preds_data_bin1 <- preds_data_bin %>%
  mutate(
    def_distance_bin =
      case_when(
        mean_defender_distance < 15 ~ 0,
        mean_defender_distance < 20 ~ 1,
        mean_defender_distance < 25 ~ 2,
        mean_defender_distance < 30 ~ 3,
        mean_defender_distance < 64 ~ 4
      )
  )

epa_tbl <- preds_data_bin1 %>%
  group_by(yardlinebin, def_distance_bin, event.y) %>%
  mutate(median_epa = mean(-.pred)) %>%
  arrange(median_epa, .by_group = TRUE)

# Find which one is best in each case
epa_tbl_only <- epa_tbl %>%
  dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)

epa_tbl_only1 <- unique(epa_tbl_only)

result_table_epa <- epa_tbl_only1 %>% 
  group_by(yardlinebin, def_distance_bin) %>% 
  top_n(1, median_epa)

# Get second highest in each group
second <- epa_tbl_only1  %>% 
  group_by(yardlinebin, def_distance_bin) %>% 
  top_n(2, median_epa) %>% 
  arrange(-median_epa, .by_group = TRUE) %>% 
  dplyr::slice(2) %>% 
  dplyr::select(yardlinebin, def_distance_bin, median_epa)

# Stopped fixing the app here; we're good to go

#### Test linear model in the app ####

pred_grid <-
  expand.grid(
    yardLine = yardline_grid,
    mean_defender_distance = def_dist_grid,
    event.y = event_grid,
    score_difference = score_diff_grid,
    half_seconds_remaining = seconds_grid,
    kickLength.x = kicklength_grid,
    quarter.x = quarter_grid
  ) %>% 
  mutate(quarter.x = as.factor(quarter.x))

scrimmage_line <- 40
minutes_remaining <- 4

pred_grid1 <- pred_grid %>%
  mutate(kickLength.x = 100 - (yardLine + scrimmage_line)) %>%
  mutate(half_seconds_remaining =
           case_when(
             (quarter.x == 4 | quarter.x == 2) ~ 60 * minutes_remaining,
             (quarter.x == 3 |
                quarter.x == 1) ~ 60 * minutes_remaining + 60 * 15
             
           ))

# Make Predictions
#matrix.preds = reactive(predict(model, pred_grid()))

# Bind them together
preds_data <- augment(model1,
                      new_data = pred_grid1,
                      interval = "prediction")

# make yardline bins
preds_data_bin <- preds_data %>%
  mutate(
    yardlinebin =
      case_when(
        yardLine < 2 ~ 1,
        yardLine < 4 ~ 2,
        yardLine < 6 ~ 3,
        yardLine < 8 ~ 4,
        yardLine < 10 ~ 5,
        yardLine < 12 ~ 6,
        yardLine < 14 ~ 7,
        yardLine < 16 ~ 8,
        yardLine < 18 ~ 9,
        yardLine < 21 ~ 10
      )
  )



# Make defender distances bins
preds_data_bin1 <- preds_data_bin %>%
  mutate(
    def_distance_bin =
      case_when(
        mean_defender_distance < 15 ~ 0,
        mean_defender_distance < 20 ~ 1,
        mean_defender_distance < 25 ~ 2,
        mean_defender_distance < 30 ~ 3,
        mean_defender_distance < 64 ~ 4
      )
  )

epa_tbl <- preds_data_bin1 %>%
  group_by(yardlinebin, def_distance_bin, event.y) %>%
  mutate(median_epa = mean(-.pred)) %>%
  arrange(median_epa, .by_group = TRUE)

# Find which one is best in each case
epa_tbl_only <- epa_tbl %>%
  dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)

epa_tbl_only1 <- unique(epa_tbl_only)

result_table_epa <- epa_tbl_only1  %>%
  group_by(yardlinebin, def_distance_bin) %>%
  top_n(1, median_epa)

# Get second highest in each group
second <- epa_tbl_only1  %>%
  group_by(yardlinebin, def_distance_bin) %>%
  top_n(2, median_epa) %>%
  arrange(-median_epa, .by_group = TRUE) %>%
  dplyr::slice(2) %>%
  dplyr::select(yardlinebin, def_distance_bin, median_epa)

second1 <- second %>% 
  rename(median_2 = median_epa)

result_table_epa1 <- result_table_epa %>% 
  left_join(second1)

result_table_epa2 <- result_table_epa1 %>% 
  mutate(difference = median_epa - median_2)

ggplot(
  data = result_table_epa2,
  mapping = aes(
    x = yardlinebin,
    y = def_distance_bin,
    fill = event.y,
    alpha = difference
  )
) +
  geom_raster() + labs(
    title = "What should returners be doing based on EPA",
    subtitle = "Difference between first and second best choice",
    x = "Yardline binned every 2 yards",
    y = "Closest Defender Distance binned at quantiles"
  )


#### Look at more interesting summary details ####

# for the `binned_data`
# returner.x is the returner based on punt_received or fair_catch
# returner.y is the returner based on punt_land that Nate calculated
# use returner.x.x because that is the combined grouping of returner IDs

summary_analysis_tbl <- binned_data %>%
  ungroup() %>% 
  dplyr::select(gameId,
                playId,
                returnerId.x.x,
                possessionTeam.x,
                homeTeamAbbr,
                visitorTeamAbbr,
                yardLine,
                mean_defender_distance,
                kickLength.x,
                quarter.x,
                half_seconds_remaining,
                score_difference,
                event.y,
                epa
  ) %>%
  mutate(returnTeam = case_when(
    possessionTeam.x == homeTeamAbbr ~ visitorTeamAbbr,
    TRUE ~ homeTeamAbbr
  )) %>% 
  mutate(returnTeam = case_when(
    returnTeam == "OAK" ~ "LV",
    TRUE ~ returnTeam
  ))

# Predicted EPA based on how good the returner/team is based on what they chose
returner_preds <- augment(lm_fit,
                          new_data = summary_analysis_tbl,
                          interval = "prediction") %>% 
  mutate(returner_decision_EPAdifference = -.resid)

returner_preds %>% 
  # filter by certain teams to see them easier
  filter(returnTeam == "PHI" |
           returnTeam == "SEA" |
           returnTeam == "ATL") %>% 
  ggplot(aes(y = returner_decision_EPAdifference)) +
  geom_boxplot() +
  facet_wrap(~ returnTeam)

returner_preds %>% 
  group_by(returnTeam) %>% 
  summarise(mean(returner_decision_EPAdifference)) %>% 
  arrange(desc(`mean(returner_decision_EPAdifference)`), .by_group = TRUE) %>% 
  ggplot(aes(x = returnTeam, y = `mean(returner_decision_EPAdifference)`)) +
  geom_col() +
  #coord_flip() +
  labs(title = "Based on the decision made, how did the team's returner do?",
       subtitle = "",
       x = "Team",
       y = "EPA over Expected")

# make 3 datasets each with its own of the three events and then row_bind
returner_preds_puntland <- returner_preds %>% 
  rename("event.actual" = event.y) %>% 
  mutate(event.y = "punt_land")

returner_preds_puntreceived <- returner_preds %>% 
  rename("event.actual" = event.y) %>% 
  mutate(event.y = "punt_received")

returner_preds_faircatch <- returner_preds %>% 
  rename("event.actual" = event.y) %>% 
  mutate(event.y = "fair_catch")

returner_preds_full <- bind_rows(returner_preds_puntland,
                                 returner_preds_puntreceived,
                                 returner_preds_faircatch) %>% 
  rename("event.actual.pred" = .pred,
         "event.actual.resid" = .resid)

preds_full <- augment(lm_fit,
                      new_data = returner_preds_full,
                      interval = "prediction")

preds_full <- preds_full %>% 
  group_by(gameId, playId) %>% 
  arrange(.pred, .by_group = TRUE) %>% 
  slice_min(.pred) %>% 
  mutate(.pred = -.pred,
         epa = -epa,
         event.actual.pred = -event.actual.pred) %>% 
  mutate(
         # How much better our model does than they did 
         model_minus_actual = .pred - epa,
         # How much better our model does than we expected them to do
         model_minus_expectation = .pred - event.actual.pred
         )

preds_full %>% 
  ggplot(aes(x = model_minus_expectation, 
             y = model_minus_actual, 
             color = event.actual,
             shape = event.y)) +
  geom_point(alpha = 0.4, size = 2)

preds_full %>% 
  filter(model_minus_expectation == 0) %>% 
  ggplot(aes(x = model_minus_expectation, 
             y = model_minus_actual, 
             color = event.actual,
             shape = event.y)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_jitter()

### Look at some plots that explain more what's going on for teams and returners
library(ggimage)
library(scales)
library(patchwork)
asp_ratio <- 1.618

df.logos <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv") %>% 
  rename("returnTeam" = team_code)


## for teams
team_preds <- preds_full %>% 
  ungroup() %>% 
  group_by(returnTeam) %>% 
  dplyr::summarise(mean = mean(model_minus_actual),
                   n = n(),
                   median = median(model_minus_actual)) %>% 
  ungroup()

team_preds %>% 
  arrange(desc(median)) %>% 
  mutate(returnTeam = fct_reorder(returnTeam, median)) %>%
  ggplot(aes(x = median, y = returnTeam)) +
  geom_col()

# Look at the previous plot in the opposite way--how well each team does
# compared to our model's prediction of how well they would do if they
# made the most optimal decisions
team_preds %>% 
  mutate(median = -median) %>% 
  arrange(desc(median)) %>% 
  mutate(returnTeam = fct_reorder(returnTeam, median)) %>%
  ggplot(aes(x = median, y = returnTeam)) +
  geom_col()

######
# WE WILL USE THIS ONE
######

team_preds %>% 
  mutate(mean = -mean) %>% 
  arrange(desc(mean)) %>% 
  mutate(returnTeam = fct_reorder(returnTeam, mean)) %>%
  ggplot(aes(x = mean, y = returnTeam)) +
  geom_col(fill = "steelblue4",
           color = "white") +
  labs(title = "Seven teams outperform the model on average",
       subtitle = NULL,
       x = "Mean actual EPA minus BDEPA",
       y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 14,
                                      face = "bold")) +
  geom_vline(xintercept = 0,
             size = 1.25,
             color = "grey43")

######

# Looking at the above two plots, it's interesting that CHI, IND, DEN, BUF, MIA
# have had more "luck" in their punt returns than we would expect

# Similarly, CIN has had "bad luck" based on their punt returns

# This is showing what percentage of time the teams are choosing the correct 
# decision
preds_full %>% 
  ungroup() %>% 
  group_by(returnTeam) %>% 
  dplyr::summarise(perc_right = mean(event.y == event.actual),
                   n = n()) %>% 
  arrange(desc(perc_right)) %>% 
  mutate(returnTeam = fct_reorder(returnTeam, perc_right)) %>%
  ggplot(aes(x = perc_right, y = returnTeam)) +
  geom_col()
  

## for players

players <- read_csv("players.csv")

player_preds <- preds_full %>%
  ungroup() %>%
  group_by(returnerId.x.x) %>%
  dplyr::summarise(mean = mean(model_minus_actual),
                   n = n(),
                   median = median(model_minus_actual)) %>% 
  ungroup() %>% 
  rename("nflId" = returnerId.x.x) %>% 
  left_join(players, 
            by = "nflId")


players %>% 
  filter(nflId == 52628) %>% View()

preds_full %>% 
  filter(returnerId.x.x == 53044) %>% 
  ggplot(aes(x = model_minus_actual)) +
  geom_histogram()

######
# WE WILL USE THIS ONE
######

# This plot shows the players that the model could help the most,
# based on their median punt returning outcomes
bottomplayer_median <- player_preds %>% 
  # returned more than 15 punts which is the 45 most frequent punt returners
  # in our time frame of 2018 - 2020
  filter(n > 15) %>% 
  arrange(desc(median)) %>% 
  mutate(displayName = fct_reorder(displayName, desc(median))) %>% 
  slice_head(n = 8) %>% 
  mutate(displayName = fct_reorder(displayName, median)) %>% 
  ggplot(aes(x = -median, y = displayName)) +
  geom_col(fill = "firebrick",
           color = "white") +
  labs(x = "Median EPA difference",
       y = NULL,
       subtitle = NULL,
       title = "Eight returners who are consistently the worst") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 12,
                                      face = "bold")) +
  geom_vline(xintercept = 0,
             size = 1.25,
             color = "grey43")


# This plot shows the players that the model could help the most,
# based on their mean punt returning outcomes
bottomplayer_mean <- player_preds %>% 
  # returned more than 15 punts which is the 45 most frequent punt returners
  # in our time frame of 2018 - 2020
  filter(n > 15) %>% 
  arrange(desc(mean)) %>% 
  mutate(displayName = fct_reorder(displayName, desc(mean))) %>% 
  slice_head(n = 8) %>% 
  mutate(displayName = fct_reorder(displayName, mean)) %>% 
  ggplot(aes(x = -mean, y = displayName)) +
  geom_col(fill = "firebrick",
           color = "white") +
  labs(x = "Mean EPA difference",
       y = NULL,
       subtitle = NULL,
       title = "Eight returners who are either unlucky or bad (or both)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 12,
                                      face = "bold")) +
  geom_vline(xintercept = 0,
             size = 1.25,
             color = "grey43")

bottomplayer_median + bottomplayer_mean

######

######
# WE WILL USE THIS ONE
######

# This plot shows the players that beat the model,
# based on their median punt returning outcomes
topplayer_median <- player_preds %>% 
  # returned more than 15 punts which is the 45 most frequent punt returners
  # in our time frame of 2018 - 2020 (leaves some space for top back-ups)
  filter(n > 15) %>% 
  arrange(median) %>% 
  mutate(displayName = fct_reorder(displayName, median)) %>% 
  # there are only 8 punt returners that out perform our model
  slice_head(n = 8) %>% 
  mutate(displayName = fct_reorder(displayName, desc(median))) %>% 
  ggplot(aes(x = -median, y = displayName)) +
  geom_col(fill = "steelblue4",
           color = "white") +
  labs(x = "Median EPA difference",
       y = NULL,
       subtitle = NULL,
       title = "Eight players have consistently out-performed the model") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 12,
                                      face = "bold")) +
  geom_vline(xintercept = 0,
             size = 1.25,
             color = "grey43")

# This plot shows the players that beat the model,
# based on their mean punt returning outcomes
topplayer_mean <- player_preds %>% 
  # returned more than 15 punts which is the 45 most frequent punt returners
  # in our time frame of 2018 - 2020 (leaves some space for top back-ups)
  filter(n > 15) %>% 
  arrange(mean) %>% 
  mutate(displayName = fct_reorder(displayName, mean)) %>% 
  # there are only 8 punt returners that out perform our model
  slice_head(n = 10) %>% 
  mutate(displayName = fct_reorder(displayName, desc(mean))) %>% 
  ggplot(aes(x = -mean, y = displayName)) +
  geom_col(fill = "steelblue4",
           color = "white") +
  labs(x = "Mean EPA difference",
       y = NULL,
       subtitle = NULL,
       title = "Ten players have out-performed the model by being lucky or great") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10,
                                 face = "bold"),
        axis.title = element_text(size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.title = element_markdown(size = 12,
                                      face = "bold")) +
  geom_vline(xintercept = 0,
             size = 1.25,
             color = "grey43")

(topplayer_mean + topplayer_median) / (bottomplayer_mean + bottomplayer_median) +
  plot_annotation(title = "Best and worst individual punt returners",
                  subtitle = "EPA over BDEPA",
                  theme = theme(plot.title = element_text(size = 18,
                                                          face = "bold.italic",
                                                          hjust = 0),
                                plot.subtitle = element_text(face = "bold.italic")))

######

## Some combined plots

mean(preds_full$event.y == preds_full$event.actual)

preds_full %>% 
  ungroup() %>% 
  left_join(df.logos, by = "returnTeam") %>% 
  group_by(returnTeam) %>% 
  dplyr::summarise(perc_right = mean(event.y == event.actual),
                   n = n(),
                   median = -median(model_minus_actual),
                   url = url) %>% 
  ungroup() %>% 
  ggplot(aes(x = perc_right, y = median)) +
  geom_image(aes(image = url), size = 0.05,
             by = "width", asp = asp_ratio) +
  geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = mean(preds_full$event.y == preds_full$event.actual), 
             color = "red") +
  theme(aspect.ratio = 1/asp_ratio)

######
# WE WILL USE THIS ONE
######

q <- preds_full %>% 
  ungroup() %>% 
  left_join(df.logos, by = "returnTeam") %>% 
  group_by(returnTeam) %>% 
  dplyr::summarise(perc_right = mean(event.y == event.actual),
                   n = n(),
                   mean = -mean(model_minus_actual),
                   url = url) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(!(returnTeam %in% c("IND", "LV")))

quadrant_plot <- preds_full %>% 
  ungroup() %>% 
  left_join(df.logos, by = "returnTeam") %>% 
  group_by(returnTeam) %>% 
  dplyr::summarise(perc_right = mean(event.y == event.actual),
                   n = n(),
                   mean = -mean(model_minus_actual),
                   url = url) %>% 
  ungroup() %>% 
  ggplot(aes(x = perc_right, y = mean)) +
  geom_hline(yintercept = 0, 
             color = "grey",
             size = 1.25,
             linetype = "solid") +
  geom_vline(xintercept = mean(preds_full$event.y == preds_full$event.actual), 
             color = "grey",
             size = 1.25,
             linetype = "solid") +
  geom_image(aes(image = url), size = 0.05,
             by = "width", asp = asp_ratio) +
  theme_minimal() +
  labs(title = "How do teams perform compared to the model?",
       subtitle = NULL,
       x = "Frequency of choosing the most optimal decision",
       y = "Mean actual EPA minus BDEPA") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(label = "Worse Decisions\nBetter Performance",
            x = 0.525, y = 0.1,
            size = 2,
            color = "grey43") +
  geom_text(label = "Better Decisions\nBetter Performance",
            x = 0.7, y = 0.15,
            size = 2,
            color = "blue") +
  geom_text(label = "Worse Decisions\nWorse Performance",
            x = 0.5, y = -0.325,
            size = 2,
            color = "red") +
  geom_text(label = "Better Decisions\nWorse Performance",
            x = 0.675, y = -0.3,
            size = 2,
            color = "grey43") +
  theme(aspect.ratio = 1/asp_ratio,
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold"),
        plot.title = element_markdown(face = "bold"))

#######

ggsave("playerbar_plot.png",
       width = 12,
       height = 7)
