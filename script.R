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
schedule <- schedule %>% select(game_id, season, week, weekday, gameday, home_team, away_team)
roster <- roster %>% select(season, team, position, jersey_number, full_name, gsis_id)

## tidy up schedule
schedule <- schedule %>% pivot_longer(cols = 6:7, names_to = "home_or_away", values_to = "team")

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
  mutate(l3points = zoo::rollapplyr(points.lag, width=3, mean, fill=NA) %>% 
           round(digits = 2)) %>%
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

names(fl_ownership_clean) <- c("player_name", "position", "team", "salary", "average_ownership", "groupid", "date", "season", "clean_team", "gsis_id", "alternate_name", "gsis_id_manual","game_id")  

# look at offense first
offensive_full_data <- left_join(
  select(fl_ownership_clean, player_name, position, clean_team, salary, average_ownership, gsis_id, game_id, season, date), 
  select(dfs_details_by_player, gsis_id, game_id, total_points, points.lag, l3points, lifetime_avg, home_or_away), 
  by=c("gsis_id", "game_id")) 

write.csv(offensive_full_data, file = "offensive_full_data.csv", row.names = F)


offensive_full_data$total_points[offensive_full_data$total_points < 0] <- 0
offensive_full_data$l3points[offensive_full_data$l3points < 0] <- 0
offensive_full_data$lifetime_avg[offensive_full_data$lifetime_avg < 0] <- 0
offensive_full_data$points.lag[offensive_full_data$points.lag < 0] <- 0

offensive_full_data <- offensive_full_data %>% filter(!offensive_full_data$total_points == 0)
offensive_full_data <- offensive_full_data %>% filter(!offensive_full_data$l3points == 0)
offensive_full_data <- offensive_full_data %>% filter(!offensive_full_data$lifetime_avg == 0)
offensive_full_data <- offensive_full_data %>% filter(!offensive_full_data$points.lag == 0)

# plot to see shape of ownership distribution
offensive_full_data %>% group_by(player_name) %>% summarise(avg_ownership = mean(average_ownership)) %>% ggplot(aes(avg_ownership)) + geom_histogram()

# too many unowned players so remove those
owned_players_only <- offensive_full_data[offensive_full_data$average_ownership > 0,]
player_ownership <- owned_players_only %>% group_by(player_name) %>% summarise(avg_ownership = mean(average_ownership))
# check shape of this data
mean(player_ownership$avg_ownership)
sd(player_ownership$avg_ownership)
player_ownership %>% arrange(desc(avg_ownership))

## player data exploration
# how many unique players are in the dataset
owned_players_only %>% group_by(gsis_id) %>% summarise() %>% nrow()
# how many of each position
owned_players_only %>% group_by(position) %>% summarise(count = n_distinct(gsis_id))
# see if some teams are over represented
owned_players_only %>% group_by(clean_team) %>% summarise(count = n_distinct(gsis_id)) %>% arrange(desc(count))
owned_players_only %>% group_by(clean_team, season) %>% summarise(count = n_distinct(gsis_id)) %>% arrange(desc(count))
# find players with highest lifetime avg points
owned_players_only %>% group_by(player_name) %>% summarise(avg_points = mean(lifetime_avg)) %>% arrange(desc(avg_points))
# which positions score the most points
owned_players_only %>% group_by(position) %>% summarise(avg_points = mean(lifetime_avg)) %>% arrange(desc(avg_points))
# what shape do points scored have
owned_players_only %>% ggplot(aes(total_points)) + geom_histogram()
# looks log-normal so check that shape
owned_players_only %>% filter(total_points >= 1) %>% ggplot(aes(log(total_points))) + geom_histogram()

## ownership exploration
# see how salary affects ownership
owned_players_only %>% group_by(salary %/% 1000) %>% summarise(avg_ownership = mean(average_ownership)) %>% arrange(desc(avg_ownership))
# see how value (expected points/(salary/1000)) affects ownership
owned_players_only <- owned_players_only %>% mutate(lifetime_value = lifetime_avg/(salary/1000), l3_value = l3points/(salary/1000))
owned_players_only %>% group_by(lifetime_value %/% 1) %>% summarise(avg_ownership = mean(average_ownership)) %>% arrange(desc(avg_ownership))
# plot last game points vs ownership
owned_players_only %>% ggplot(aes(x=points.lag, y=average_ownership)) + geom_point()
# plot last 3 average vs ownership
owned_players_only %>% ggplot(aes(x=l3points, y=average_ownership)) + geom_point()
owned_players_only %>% filter(total_points > 1) %>% ggplot(aes(x=l3points, y=average_ownership)) + geom_point()
# plot lifetime avg vs ownership
owned_players_only %>% ggplot(aes(x=lifetime_avg, y=average_ownership, col=position)) + 
  geom_point() +
  geom_smooth(aes(color=position))
# see which players are consistently delivering value
owned_players_only %>% group_by(player_name) %>% summarise(avg_value = mean(lifetime_value)) %>% arrange(desc(avg_value))
# plot expected value
owned_players_only %>% ggplot(aes(x=points.lag, y=salary)) + geom_point() + geom_smooth()
owned_players_only %>% ggplot(aes(x=l3points, y=salary)) + geom_point() + geom_smooth()
owned_players_only %>% ggplot(aes(x=lifetime_avg, y=salary)) + geom_point() + geom_smooth()
# plot actual value
owned_players_only %>% ggplot(aes(x=total_points, y=salary)) + geom_point() + geom_smooth()

# see which positions deliver the most value
owned_players_only %>% group_by(position) %>% summarise(avg_value = mean(lifetime_value)) %>% arrange(desc(avg_value))

# see if salary affects average value
owned_players_only %>% group_by(salary %/% 1000) %>% summarise(avg_value = mean(lifetime_value)) %>% arrange(desc(avg_value))

# pull in vegas o/u data to see how total expected points 
vegas_ou <- readRDS("vegas_overunder.rds")

owned_players_only <- left_join(owned_players_only, vegas_ou)

# does vegas o/u affect ownership?
owned_players_only %>% mutate(season = as.factor(season)) %>% ggplot(aes(x=max_ou, y=average_ownership, col=season)) + geom_point() + geom_smooth()
owned_players_only %>% mutate(season = as.factor(position)) %>% ggplot(aes(x=max_ou, y=average_ownership, col=position)) + geom_point() + geom_smooth()

# determine defending team for each player
unmelted_schedule <- readRDS("sched_20152020.rds")
unmelted_schedule <- unmelted_schedule %>% 
  select(game_id, home_team, away_team) %>% 
  slice(rep(1:n(), each = 2))

for(i in 1:length(unmelted_schedule$game_id)){
  if(i %% 2 == 0){
    tmp <- unmelted_schedule[i,2]
    unmelted_schedule[i,2] <- unmelted_schedule[i,3]
    unmelted_schedule[i,3] <- tmp
  }
}

names(unmelted_schedule) <- c("game_id", "team", "defteam")

# join in defending team
owned_players_only <- left_join(owned_players_only, unmelted_schedule, by=c('game_id', 'clean_team'='team'))

# calculate positional defense index
def_points <- owned_players_only %>% group_by(defteam, season, position) %>% summarise(avg_points_given_up = mean(total_points))
def_points <- def_points %>% group_by(season, position) %>% mutate(season_def_points_avg = mean(avg_points_given_up)) %>% ungroup()
def_points <- def_points %>% mutate(season_def_index = avg_points_given_up/season_def_points_avg)
def_points <- def_points %>% select(-c("avg_points_given_up", "season_def_points_avg"))

# join back in the def index
owned_players_only <- left_join(owned_players_only, def_points, by=c("defteam", "season", "position"))

# see if defense strength against position affects ownership
owned_players_only %>% mutate(season = as.factor(season)) %>% ggplot(aes(x=season_def_index, y=average_ownership, col=season)) + geom_point() + geom_smooth()

# try projecting points
owned_players_only$position <- as.factor(owned_players_only$position)
owned_players_only$home_or_away <- as.factor(owned_players_only$home_or_away)

opo_train <- owned_players_only %>% filter(season != 2020)
opo_test <- owned_players_only %>% filter(season == 2020)

fit2 <- glm(total_points ~ position + salary + points.lag + l3points + lifetime_avg + home_or_away + max_ou + season_def_index, opo_train, family = Gamma('log'))
summary(fit2)
fit2.diag <- glm.diag(fit2)
glm.diag.plots(fit2, fit2.diag)

# try predicting 2020
predictions_2020 <- predict(fit2, opo_test, type = "response")
opo_test$predicted_points <- predictions_2020
opo_test %>% ggplot(aes(x=predicted_points, y=total_points)) + geom_point() + geom_abline()

# add projections for the full dataset
full_projected_points <- predict(fit2, owned_players_only, type="response")
owned_players_only$projected_points <- full_projected_points

# try predicting 2020 ownership
games_per_week <- schedule %>% group_by(rounded_gameday) %>% summarise(num_games = n_distinct(game_id))
owned_players_only <- left_join(owned_players_only, games_per_week, by=c("date" = "rounded_gameday"))
# because ownership is between 0 and 1, try beta regression
library(betareg)
# convert ownership to between 0 and 1, reset training/test data
owned_players_only$average_ownership <- owned_players_only$average_ownership/100
opo_train <- owned_players_only %>% filter(season != 2020)
opo_test <- owned_players_only %>% filter(season == 2020)

betaModel <- betareg(average_ownership ~ position + salary + points.lag + l3points + lifetime_avg + home_or_away + max_ou + season_def_index + projected_points + num_games, data=opo_train)
summary(betaModel)
ownership_pred_2020 <- predict(betaModel, opo_test)

opo_test$projected_ownership <- ownership_pred_2020
opo_test %>% ggplot(aes(x=average_ownership, y=projected_ownership)) + geom_point() + geom_abline()
plot(betaModel)

opo_test$ownership_projection_error <- opo_test$average_ownership - opo_test$projected_ownership
hist(opo_test$ownership_projection_error)

