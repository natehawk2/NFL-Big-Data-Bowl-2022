# File to deal with distance data

library(tidyverse)
library(janitor)

## Read in and combine action datasets
# Read in action dataframes
action2018_tbl <- read_csv("action2018.csv") %>% 
  mutate(returnerId.x = as.double(returnerId.x),
         returnerId = as.double(returnerId)) %>% 
  dplyr::select(-...1)
action2019_tbl <- read_csv("action2019.csv") %>% 
  mutate(returnerId.x = as.double(returnerId.x),
         returnerId = as.double(returnerId)) %>% 
  dplyr::select(-...1)
action2020_tbl <- read_csv("action2020.csv") %>% 
  mutate(returnerId.x = as.double(returnerId.x),
         returnerId = as.double(returnerId)) %>% 
  dplyr::select(-...1)


# Combine all the locations
action_tbl <- bind_rows(action2018_tbl, action2019_tbl, action2020_tbl)

# Take out the ones where there is no returner
unique.games <- action_tbl %>% 
  filter((position == "P" & returner_distance == 0)) %>% 
  dplyr:: select(game_play) %>% unique()

mean(action_tbl$game_play %in% unique.games$game_play)
action_tbl <- action_tbl[!(action_tbl$game_play %in% unique.games$game_play),]
table(action_tbl$game_play)

# I think the data for this game is wrong so I am taking it out.
action_tbl <- action_tbl[!(action_tbl$gameId == 2019120110 & action_tbl$playId == 3456),]

table(table(action_tbl$game_play))

# Read in punts_plays_fastr data
punts_plays_fastr <- read_csv("https://raw.githubusercontent.com/natehawk2/NFL-Big-Data-Bowl-2022/main/punts_plays_fastr.csv")

# Look at one specific play
action_puntteam_tbl %>% 
  filter(playId == 3456 & gameId == 2019120110) %>% 
  ggplot(aes(x = x, y = y, color = Punt_Return_Team)) +
  geom_point(size = 3) +
  geom_text(aes(label = ifelse(position == "P", position, "")),
            nudge_y = 2,
            nudge_x = 2)

## Label the players for each team on each punt
return_punt_team_reference <- action_tbl %>%
  select(position, team, gameId, playId) %>% 
  group_by(gameId, playId) %>% 
  mutate(Punt_Return_Team = case_when(
    position == "P" ~ "Punt Team",
    is.na(position) ~ "Football",
    TRUE ~ "Return Team"
  )) %>% 
  filter(position == "P" | Punt_Return_Team == "Football") %>% 
  dplyr::select(-position) %>% 
  ungroup()

action_puntteam_tbl <- return_punt_team_reference %>% 
  right_join(action_tbl)

# Look at the distances to make sure they make sense
action_puntteam_tbl %>% 
  filter(Punt_Return_Team == "Punt Team") %>% 
  group_by(gameId, playId) %>% 
  arrange(returner_distance) %>% 
  dplyr::select(gameId, playId, Punt_Return_Team, x, y, returner_distance) %>% 
  View()


#### Below is the tst data set that we were working on that we're trying
#### to make sure it's a working data set

# Combine action_puntteam_tbl with punts_plays_fastr data
tst <- action_puntteam_tbl %>% 
  left_join(punts_plays_fastr,
            by = c("gameId", "playId")) %>%
  filter(!is.na(yardLine))

table(table(tst$game_play))
max(table(tst$game_play))

tst %>% 
  filter(Punt_Return_Team == "Punt Team") %>% 
  arrange(returner_distance) %>%
  dplyr::select(x.y,  y.y, gameId, playId, yardLine, Punt_Return_Team, returner_distance, playDescription.x) %>% View()



# Get rid of the plays that have more than 23 rows
require(data.table) ## 1.9.2
tst1 <- tst[!(tst$game_play %in% setDT(tst)[, .N, by=game_play][N > 23L]$game_play),]
table(table(tst1$game_play))

table(tst1$event.y)/23


# I think that tst1 has everything that I want
# It's possible that I took out too much
tst1 %>% 
  arrange(returner_distance) %>%
  dplyr::select(x.y,  y.y, gameId, playId, yardLine, Punt_Return_Team, returner_distance, playDescription.x) %>% View()

table(tst1$event.y)

tst1$Punt_Return_Team <- ifelse(is.na(tst1$Punt_Return_Team), "Return Team", tst1$Punt_Return_Team)
hist(tst1$kickLength.x)

# Filter to take out the super short punts.
# Try and change the number
tst2 <- tst1 %>% filter(kickLength.x > 40)
#tst1 %>% filter(kickLength.x < 20) %>% dplyr::select(playDescription.x, yardLine, kickLength.x, yardlineNumber.x) %>% view 


# Get the mean of the  closest defender.
def_distances_tbl <- tst2 %>% group_by(gameId, playId, Punt_Return_Team)  %>% 
  filter(Punt_Return_Team == "Punt Team") %>% 
  arrange(returner_distance, .by_group = TRUE)  %>% 
  #dplyr::select(gameId, playId, Punt_Return_Team, returner_distance) %>% 
  slice(1) %>% 
  mutate(mean_defender_distance = mean(returner_distance)) %>% 
  slice(1)

table(def_distances_tbl$event.y)
hist(def_distances_tbl$yardLine)
hist(def_distances_tbl$mean_defender_distance)
summary(def_distances_tbl$mean_defender_distance)

library(ggplot2)
ggplot(data = def_distances_tbl, mapping = aes(x = mean_defender_distance, fill = event.y, alpha = 0.3)) + 
  geom_density() 

# make yardline bins
def_distances_tbl <- def_distances_tbl %>% mutate(yardlinebin = 
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

ggplot(data = def_distances_tbl %>% filter(event.y != "punt_muffed"),
       mapping = aes(x = mean_defender_distance, fill = event.y, alpha = 0.3)) + 
  geom_density() +
  facet_wrap(~yardlinebin)


# Make defender distances bins
binned_data <- def_distances_tbl %>% mutate(def_distance_bin = 
                                                    case_when(
                                                      mean_defender_distance < 15 ~ 0,
                                                      mean_defender_distance < 20 ~ 1,
                                                      mean_defender_distance < 25 ~ 2,
                                                      mean_defender_distance < 30 ~ 3,
                                                      mean_defender_distance < 64 ~ 4
                                                    )) 

binned_data %>% dplyr::select(def_distance_bin, mean_defender_distance)

### reclassify muffs as either fair catches or punt recieves
# summary(log)
# preds <- predict(log, newdata = binned_data %>% filter(event.y == "punt_muffed"))
binned_data$event.y = ifelse(binned_data$event.y == "punt_muffed", "fair_catch", binned_data$event.y)



# Get mean epa for each group
epa_tbl <- binned_data %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-epa)) %>% 
  arrange(median_epa, .by_group = TRUE)

epa_tbl %>% arrange(median_epa, .by_group = TRUE) %>% 
  dplyr::select(mean_defender_distance, yardLine, playDescription.x, epa, event.y, median_epa) %>%
  group_by(yardlinebin, def_distance_bin)

# Plot epa by group
ggplot(data = epa_tbl %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = median_epa)) + 
  geom_raster() + 
  facet_wrap(~event.y)

# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin)

result_table_epa <- epa_tbl_only %>% filter(event.y != "punt_muffed") %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)

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
                       subtitle = "Difference between first and second best choice",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned at quantiles")

ggplot(
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
    title = "Best decision (all punts)",
    subtitle = "Gradient represents how much better that choice is than the next best choice",
    x = "Yardline \n (binned every 2 yards)",
    y = "Closest Defender Distance\n(2 seconds until ball lands)",
    fill = NULL,
    caption = "Plot: Nate Hawkins & Jacob Miller \n Data: NFL Big Data Bowl & nflfastr"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0.5, 3, 5.5, 8, 10.5),
                     labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("< 15", "15 - 20", "20 - 25", "25 - 30", "> 30")) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14,
                                 face = "bold"),
        axis.title = element_text(size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.title = element_markdown(size = 20,
                                      face = "bold"))

# Make plot to show what returners ARE doing

epa_tbl <- binned_data %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
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

ggplot(data = result_table_epa, mapping = aes(x = yardlinebin, y = def_distance_bin,
                                              fill = event.y, 
                                              alpha = difference)) + 
  geom_raster() + labs(title = "What are returners doing?",
                       subtitle = "Most frequent choices",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned every 3 yards")







##############################
# Trying to bin another way###
##############################
library(mltools)

# Resulting bins have an equal number of observations in each group
binned_data1 <- binned_data
binned_data1$yardlinebin <- bin_data(c(binned_data$yardLine), bins=10, binType = "quantile")
binned_data1$def_distance_bin <- bin_data(c(binned_data$mean_defender_distance), bins=10, binType = "quantile")


# Get mean epa for each group
epa_tbl <- binned_data1 %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-epa)) %>% 
  arrange(median_epa, .by_group = TRUE)

epa_tbl %>% arrange(median_epa) %>% 
  dplyr::select(mean_defender_distance, yardLine, playDescription.x, epa, event.y, median_epa) %>% View()

# Plot epa by group
ggplot(data = epa_tbl %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = median_epa)) + 
  geom_raster() + 
  facet_wrap(~event.y)

# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin) 

result_table_epa <- epa_tbl_only %>% filter(event.y != "punt_muffed") %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_epa)
result_table_epa
ggplot(data = result_table_epa %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = event.y)) + 
  geom_raster() + labs(title = "Best Choice for punt returners based on EPA", 
                       x = "Yardline",
                       y = "Closest Defender Distance")

wpa_tbl <- binned_data1 %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_wpa = quantile(-wpa, 0.5)) %>% 
  arrange(median_wpa, .by_group = TRUE)

wpa_tbl %>% arrange(median_wpa) %>% 
  dplyr::select(mean_defender_distance, yardLine, playDescription.x, wpa, event.y, median_wpa)

# Plot wpa by group
ggplot(data = wpa_tbl %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = median_wpa)) + 
  geom_raster() + 
  facet_wrap(~event.y)

# Find which one is best in each case
wpa_tbl_only = wpa_tbl %>% dplyr::select(median_wpa, yardlinebin, def_distance_bin, event.y)
wpa_tbl_only <- unique(wpa_tbl_only)
wpa_tbl_only %>% group_by(yardlinebin, def_distance_bin)

result_table <- wpa_tbl_only %>% filter(event.y != "punt_muffed") %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, median_wpa)
result_table
ggplot(data = result_table %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = event.y)) + 
  geom_raster() + 
  labs(title = "Best Choice for punt returners based on WPA", 
       x = "Yardline binned every 2 yards",
       y = "Closest Defender Distance binned every 3 yards")




####################
# Kick length bins #
####################

binned_data2 <- binned_data
hist(binned_data$yardlineNumber.x)

binned_data2 <- binned_data %>% mutate(kick_yardline_bin = 
                                              case_when(
                                                yardlineNumber.x < 30 ~ 3,
                                                yardlineNumber.x < 40 ~ 4,
                                                yardlineNumber.x < 50 ~ 5,
                                                yardlineNumber.x < 100 ~ 6
                                              )) 
table(binned_data2$kick_yardline_bin)

epa_tbl <- binned_data2 %>% group_by(yardlinebin, def_distance_bin, event.y, kick_yardline_bin) %>% 
  mutate(median_epa = mean(-epa)) %>% 
  arrange(median_epa, .by_group = TRUE)


# Find which one is best in each case
epa_tbl_only = epa_tbl %>% dplyr::select(median_epa, yardlinebin, def_distance_bin, kick_yardline_bin, event.y)
epa_tbl_only <- unique(epa_tbl_only)
epa_tbl_only %>% group_by(yardlinebin, def_distance_bin, kick_yardline_bin) %>% view


result_table <- epa_tbl_only %>% filter(event.y != "punt_muffed") %>%  group_by(yardlinebin, def_distance_bin, kick_yardline_bin) %>% top_n(1, median_epa)
result_table
ggplot(data = result_table %>% filter(event.y != "punt_muffed"), mapping = aes(x = yardlinebin, y = def_distance_bin, fill = event.y)) + 
  geom_raster() + 
  labs(title = "Best Choice for punt returners based on WPA", 
       x = "Yardline binned every 2 yards",
       y = "Closest Defender Distance binned every 3 yards") + 
  facet_wrap(~kick_yardline_bin)



########################
# Individual Returners #
########################

# make yardline bins
def_distances_tbl <- def_distances_tbl %>% mutate(yardlinebin = 
                                                    case_when(
                                                      yardLine < 5 ~ 1,
                                                      yardLine < 10 ~ 2,
                                                      yardLine < 15 ~ 3,
                                                      yardLine < 21 ~ 4
                                                    
                                                    )) 


# Make defender distances bins
binned_data <- def_distances_tbl %>% mutate(def_distance_bin = 
                                              case_when(
                                                mean_defender_distance < 15 ~ 0,
                                                mean_defender_distance < 20 ~ 1,
                                                mean_defender_distance < 25 ~ 2,
                                                mean_defender_distance < 30 ~ 3,
                                                mean_defender_distance < 64 ~ 4
                                              )) 

table(binned_data$returnerId.x.x)
mean(is.na(binned_data$returnerId.x.x))

binned_data3 <- binned_data
returner = binned_data3 %>% filter(returnerId.x.x == "43556")

for(i in 1:length(unique(binned_data3$returnerId.x.x))){
  returner = binned_data3 %>% filter(returnerId.x.x == unique(binned_data3$returnerId.x.x)[i])
  
  epa_tbl <- returner %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
    mutate(median_epa = mean(-epa)) %>% 
    arrange(median_epa, .by_group = TRUE)
  
  epa_tbl_only = epa_tbl %>% count(yardlinebin, def_distance_bin, event.y) %>% unique()
  
  result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, n)
  
  ggplot(data = result_table_epa, mapping = aes(x = yardlinebin, y = def_distance_bin,
                                                fill = event.y)) + 
    geom_raster() + labs(title = "What is a given reciever doing?",
                         #subtitle = "Most frequent choices",
                         x = "Yardline binned every 2 yards",
                         y = "Closest Defender Distance binned every 3 yards")
}

epa_tbl <- returner %>% group_by(yardlinebin, def_distance_bin, event.y) %>% 
  mutate(median_epa = mean(-epa)) %>% 
  arrange(median_epa, .by_group = TRUE)

epa_tbl_only = epa_tbl %>% count(yardlinebin, def_distance_bin, event.y) %>% unique()

result_table_epa <- epa_tbl_only  %>%  group_by(yardlinebin, def_distance_bin) %>% top_n(1, n)

ggplot(data = result_table_epa, mapping = aes(x = yardlinebin, y = def_distance_bin,
                                              fill = event.y)) + 
  geom_raster() + labs(title = "What is a given reciever doing?",
                       #subtitle = "Most frequent choices",
                       x = "Yardline binned every 2 yards",
                       y = "Closest Defender Distance binned every 3 yards")

