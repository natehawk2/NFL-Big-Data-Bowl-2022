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

# Read in punts_plays_fastr data
punts_plays_fastr <- read_csv("punts_plays_fastr.csv")

# Look at one specific play
tst %>% 
  filter(playId == 2788 & gameId == 2018100710) %>% 
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

tst %>% 
  filter(Punt_Return_Team == "Punt Team") %>% 
  arrange(returner_distance) %>% View()