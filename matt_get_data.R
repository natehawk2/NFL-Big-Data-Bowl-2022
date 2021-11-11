rm(list = ls())
library(vroom)
library(tidyverse)
track18 <- "nfl-big-data-bowl-2022/tracking2018.csv"
tracking.example <- read_csv(track18)

file.game <- "nfl-big-data-bowl-2022/games.csv"
games.sum <- read.csv(file.game) 

file.plays <- "nfl-big-data-bowl-2022/plays.csv"
plays.sum <- read.csv(file.plays)
games_vec <- unique(games.sum$gameId)

pff <- "nfl-big-data-bowl-2022/PFFScoutingData.csv"
pff <- read.csv(pff)

players <- "nfl-big-data-bowl-2022/players.csv"
players <- read.csv(players)



#tracking.example <- tracking.example[1:10000,]

tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum)

all_data <- tracking.example.merged %>% filter(specialTeamsPlayType == "Punt")


# This one doesn't have the football location
withpff <- all_data %>% inner_join(pff) %>% inner_join(players)


# I would take a subset of this to write the code with otherwise it's going to take forever to test anything.