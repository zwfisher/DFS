library(httr)
library(jsonlite)
library(lubridate)

nfl_weeks <- schedule$gameday
nfl_weeks <- str_glue("{month(nfl_weeks)}_{day(nfl_weeks)}_{year(nfl_weeks)}")
nfl_weeks <- unique(nfl_weeks)
## get sourceid & draftgroup id from fantasylabs api
# fl_cgroups <- data.frame()
# for(i in 1:length(nfl_weeks)){
#   fl_cgroups <- rbind(fl_cgroups, jsonlite::fromJSON(url(paste0("https://www.fantasylabs.com/api/ownership-contestgroups/1/4/",nfl_weeks[i]))))
#   if((i %% 5) == 0){
#     print(paste0(i, " of 339"))
#   }
# }
# saveRDS(fl_cgroups, file = "fantasylabs_20152020_draftgroupids.rds" )

# fl_cgroups <- readRDS("fantasylabs_20152020_draftgroupids.rds")
# draftGroupIdsAndDates <- fl_cgroups %>% select(DraftGroupId, ContestStartDate)
# draftGroupIdsAndDates$ContestStartDate <- as.Date(draftGroupIdsAndDates$ContestStartDate)
# draftGroupIdsAndDates <- unique(draftGroupIdsAndDates)
# row.names(draftGroupIdsAndDates) <- NULL
# 
# ## get ownership data for given date/sourceid/draftgroupid/0
# fl_ownership <- data.frame()
# for(i in 1:length(draftGroupIdsAndDates$ContestStartDate)){
#   ## build the URL
#   url <- paste0("https://www.fantasylabs.com/api/contest-ownership/1/", draftGroupIdsAndDates[i,2], "/4/", draftGroupIdsAndDates[i,1], "/0/")
#   ## fetch the data, put into df
#   dat <- as.data.frame(jsonlite::fromJSON(url, flatten = TRUE), row.names = NULL)
#   ## check if empty (in case a date has no data)
#   if(nrow(dat) == 0){ next }
#   ## select the columns we want b/c FL will put variable column names in there
#   dat <- dat %>% select(Properties.Player_Name, Properties.Position, Properties.Team, Properties.Salary, Properties.Average)
#   dat$GroupId <- draftGroupIdsAndDates[i,1]
#   dat$Date <- draftGroupIdsAndDates[i,2]
#   ## append
#   fl_ownership <- rbind(fl_ownership, dat)
#   ## print progress
#   if((i %% 5) == 0){
#     print(paste0(i, " of 103"))
#   }
# }
## saveRDS(fl_ownership, file="fantasylabs20152020_ownership.rds")

fl_ownership <- readRDS("fantasylabs20152020_ownership.rds")

roster$last_name <- str_extract(roster$full_name, '[^ ]+$')
roster$first_name <- sapply(strsplit(roster$full_name, " "), `[`, 1)
roster$first_initial <- substring(roster$first_name, 1, 1)
roster$standardized_name <- paste0(roster$first_initial,".", roster$last_name)

fl_ownership$last_name <- str_extract(fl_ownership$Properties.Player_Name, '[^ ]+$')
fl_ownership$first_name <- sapply(strsplit(fl_ownership$Properties.Player_Name, " "), `[`, 1)
fl_ownership$first_initial <- substring(fl_ownership$Properties.Player_Name,1,1)
fl_ownership$standardized_name <- paste0(fl_ownership$first_initial,".", fl_ownership$last_name)

missing_in_fl <- anti_join(roster,fl_ownership, by="standardized_name")
missing_in_fl <- missing_in_fl %>% filter(position %in% c("QB", "RB", "WR", "TE"))
missing_in_fl <- missing_in_fl %>% filter(season != 2015)

missing_in_roster <- anti_join(fl_ownership, roster, by="standardized_name")
missing_in_roster <- missing_in_roster %>% filter(last_name != "Defense")
                                          