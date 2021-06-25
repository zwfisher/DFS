library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(lubridate)
library(slider)

setwd("~/Desktop/data/dfs/")

## read data
# pbp <- readRDS("pbp_20152020.rds")
schedule <- readRDS("sched_20152020.rds")
roster <- readRDS("rosters_20152020.rds")
dfspoints <- readRDS("dfspoints_20152020.rds")

## trim dataframes
schedule <- schedule %>% select(game_id, season, week, gameday, home_team, away_team)
roster <- roster %>% select(season, team, position, jersey_number, full_name, gsis_id)

## tidy up schedule
schedule <- schedule %>% pivot_longer(cols = 5:6, names_to = "home_or_away", values_to = "team")

## create schedule with roster details
schedule_roster <- left_join(schedule, roster, by=c("season", "team"))

## join dfspoints w/ schedule_roster to get player and game details
dfs_details <- left_join(schedule_roster, dfspoints, by=c("gsis_id" = "player_id", "game_id" = "game_id"))

## trim dfs_details to only include QB, RB, WR, TE
dfs_details <- dfs_details %>% filter(position %in% c("QB", "RB", "WR", "TE"))

## free up memory by cleaning up unneccessary tables
rm(schedule_roster)
rm(dfspoints)
gc()

dfs_details <- dfs_details %>% mutate(total_points = select(.,passing_points, 
                                             rushing_points, 
                                             receiving_points,
                                             fumble_points,
                                             koret_points,
                                             fgret_points,
                                             pass_2ptconv_points,
                                             rush_2ptconv_points,
                                             rec_2ptconv_points) %>% rowSums(na.rm = TRUE))

dfs_details$gameday <- ymd(dfs_details$gameday)

dfs_details_by_player <- as_tibble(dfs_details)

dfs_details_by_player <- dfs_details_by_player %>%
  arrange(gsis_id) %>%
  group_by(gsis_id) %>%
  mutate(points.lag = dplyr::lag(total_points, n=1, default = 0)) %>%
  mutate(l3points = zoo::rollapplyr(points.lag, width=3, mean, fill=NA) %>% round(digits = 2)) %>%
  mutate(lifetime_avg = cumsum(points.lag)/seq_along(points.lag)) %>%
  mutate(lifetime_avg = round(lifetime_avg, digits=2)) %>%
  ungroup()

rm(dfs_details)
gc()

fl_ownership<- readRDS("fantasylabs20152020_ownership.rds")

# clean up roosevelt nix
fl_ownership <- fl_ownership %>% mutate(Properties.Player_Name = ifelse(Properties.Player_Name == " Roosevelt Nix", "Roosevelt Nix", Properties.Player_Name))

# standardize full name formats
roster$full_name <- tolower(roster$full_name)
roster$full_name <- gsub('[[:punct:]]',"",roster$full_name)

fl_ownership$Properties.Player_Name <- tolower(fl_ownership$Properties.Player_Name)
fl_ownership$Properties.Player_Name <- gsub('[[:punct:]]', "", fl_ownership$Properties.Player_Name)

# format dates for joins
schedule$gameday <- as.Date(schedule$gameday)
fl_ownership$season <- year(fl_ownership$Date-60)

# add player id and team b/c fl_ownership team is not complete
fl_ownership <- left_join(fl_ownership, select(roster, season, position, team, full_name, gsis_id), by=c("Properties.Player_Name" = "full_name", "Properties.Position" = "position", "season"))
missing_players <- fl_ownership %>% filter(!str_detect(Properties.Player_Name, 'defense') & is.na(gsis_id)) %>% group_by(Properties.Player_Name) %>% arrange(Properties.Player_Name)
missing_players_names <- fl_ownership %>% filter(!str_detect(Properties.Player_Name, 'defense') & is.na(gsis_id)) %>% group_by(Properties.Player_Name) %>% summarise()

# export and clean up data, reimport
missing_players_manual_edits <- read.csv("fl_ownership_players_missing_from_roster.csv", stringsAsFactors = FALSE, )
missing_players_manual_edits <- missing_players_manual_edits[,-1]
fl_ownership <- left_join(fl_ownership, select(missing_players_manual_edits, Properties.Player_Name, alternate_name, gsis_id_manual), by=c("Properties.Player_Name"))

# use fl_ownership team data for defenses, add in manual gsis_ids
fl_ownership <- fl_ownership %>% mutate(team = ifelse(str_detect(Properties.Player_Name, 'defense'), Properties.Team, team))
fl_ownership <- fl_ownership %>% mutate(gsis_id = ifelse(str_detect(Properties.Player_Name, 'defense'), "defense", gsis_id))
fl_ownership <- fl_ownership %>% mutate(team = ifelse(is.na(team), Properties.Team, team))
fl_ownership <- fl_ownership %>% mutate(gsis_id = ifelse(is.na(gsis_id), gsis_id_manual, gsis_id))

# round dates from schedule to match fl_ownership
schedule$rounded_gameday <- round_date(schedule$gameday, "weeks")

# clean up historic team names
fl_ownership <- fl_ownership[!is.na(fl_ownership$team),]
library(data.table)
fl_ownership <- as.data.table(fl_ownership)
fl_ownership[season == 2016 & team == "LAC", team := "SD"]
fl_ownership[season < 2020 & team == "LV", team := "OAK"]
fl_ownership[team == "LAR", team := "LA"]
fl_ownership <- as.tibble(fl_ownership)

# add game id 
fl_ownership$Date <- as.Date(fl_ownership$Date)
fl_ownership$Date <- round_date(fl_ownership$Date, "weeks")
fl_ownership <- left_join(fl_ownership, select(schedule, rounded_gameday, team, game_id), by=c("Date"="rounded_gameday", "team"="team"))

# clean to remove no salary, no 
fl_ownership_clean <- drop_na(fl_ownership, Properties.Salary)
fl_ownership_clean <- drop_na(fl_ownership_clean, game_id)
fl_ownership_clean <- drop_na(fl_ownership_clean, gsis_id)

rm(fl_ownership)
rm(missing_players)
rm(missing_players_manual_edits)
rm(missing_players_names)
gc()

names(fl_ownership_clean) <- c("player_name", "position", "team", "salary", "average_ownership", "groupid", "date", "season", "team", "gsis_id", "alternate_name", "gsis_id_manual","game_id")  

# look at offense first
offensive_full_data <- left_join(dfs_details_by_player, select(fl_ownership_clean, salary, average_ownership, gsis_id, game_id), by=c("gsis_id", "game_id")) 

