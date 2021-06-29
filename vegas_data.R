library(readxl)

setwd("~/Desktop/data/dfs/")

# load seasons
season_2016 <- read_xlsx("nfl odds 2016-17.xlsx")
season_2017 <- read_xlsx("nfl odds 2017-18.xlsx")
season_2018 <- read_xlsx("nfl odds 2018-19.xlsx")
season_2019 <- read_xlsx("nfl odds 2019-20.xlsx")
season_2020 <- read_xlsx("nfl odds 2020-21.xlsx")

# set up date for each season
season_2016 <- season_2016 %>% mutate(fullDate = ifelse(Date < 300, paste0(Date,"2017") , paste0(Date, "2016")))
season_2017 <- season_2017 %>% mutate(fullDate = ifelse(Date < 300, paste0(Date,"2018") , paste0(Date, "2017")))
season_2018 <- season_2018 %>% mutate(fullDate = ifelse(Date < 300, paste0(Date,"2019") , paste0(Date, "2018")))
season_2019 <- season_2019 %>% mutate(fullDate = ifelse(Date < 300, paste0(Date,"2020") , paste0(Date, "2019")))
season_2020 <- season_2020 %>% mutate(fullDate = ifelse(Date < 300, paste0(Date,"2021") , paste0(Date, "2020")))

# combine into one dataframe
vegas <- rbind(season_2016, season_2017, season_2018, season_2019, season_2020)

# free up memory
rm(season_2016)
rm(season_2017)
rm(season_2018)
rm(season_2019)
rm(season_2020)
gc()

# clean up date into POSIXct
vegas$formattedDate <- as.character(vegas$formattedDate)
for(i in 1:length(vegas$fullDate)){
  d <- as.character(vegas$fullDate[i])
  if(str_length(d) < 8){
    d <- paste0("0", d)
  }
  vegas$formattedDate[i] <- d
}
vegas$Date <- mdy(vegas$formattedDate)
vegas <- vegas %>% select(-c("formattedDate", "fullDate"))

# manually set up team abbreviations
teams <- vegas %>% distinct(Team)
print(teams, n=40)
abbr <- c("CAR", "DEN", "TB", "ATL", "MIN", "TEN", "CLE", "PHI", "CIN", "NYJ", "OAK",
          "NO", "SD", "KC", "BUF", "BAL", "CHI", "HOU", "GB", "JAX", "MIA", "SEA", "NYG", 
          "DAL", "DET", "IND", "NE", "ARI", "PIT", "WAS", "LA", "SF", "LA", "LAC", "LV", "LV", "KC", "KC", "TB", "WAS")
teams$abbr <- abbr
# join in the abbreviations
vegas <- left_join(vegas, teams, by="Team")

# join in the game_id from the schedule data
schedule <- readRDS("sched_20152020.rds")
schedule <- schedule %>% pivot_longer(cols = 8:9, names_to = "home_or_away", values_to = "team")
schedule$gameday <- as.Date(schedule$gameday)
# check to see if there are missing data
schedule %>% filter(season != 2015) %>% anti_join(., vegas, by=c("gameday"="Date", "team"="abbr"))
# none so join
vegas <- left_join(vegas, schedule, by=c("Date"="gameday", "abbr"="team"))

# select just what we need and clean it up, get rid of pickem lines
over_under_only <- vegas %>% select(game_id, abbr, Open, Close)
over_under_only <- over_under_only %>% pivot_longer(cols = 3:4)
over_under_only$value <- as.numeric(over_under_only$value)
over_under_only$value[is.na(over_under_only$value)] <- 0

# get the max ou for the game
over_under_only <- over_under_only %>% group_by(game_id) %>% summarise(max_ou = max(value))
saveRDS(over_under_only, "vegas_overunder.rds")
