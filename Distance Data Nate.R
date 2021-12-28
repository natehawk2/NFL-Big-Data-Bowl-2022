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


# I think that tst1 has everything that I want
# It's possible that I took out too much
tst1 %>% 
  arrange(returner_distance) %>%
  dplyr::select(x.y,  y.y, gameId, playId, yardLine, Punt_Return_Team, returner_distance, playDescription.x) %>% View()
