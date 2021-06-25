## function to calculate DFS points
# Game ID: 2015_01_BAL_DEN
# Fumbled Game ID: 2015_01_CAR_JAX
# Passer P.Manning : 00-0010346
# Rusher C.Anderson: 00-0029854
# Receiver D.Thomas: 00-0027874
# Fumbled A.Hearns: 00-0030821
# Special Teams

get_dfs_points <- function(player_id) {
  # find total passing points
  passing_points <- pbp %>% 
    filter(passer_player_id == player_id, play_type == "pass") %>%
    group_by(game_id) %>%
    summarise(passing_points = 
                sum(pass_touchdown)*4 + 
                sum(yards_gained)*0.04 + 
                ifelse(sum(yards_gained) > 299,3,0) -
                sum(interception))
  
  # find total rushing points
  rushing_points <- pbp %>% 
    filter( 
      rusher_player_id == player_id, 
      play_type == "run") %>%
    group_by(game_id) %>%
    summarise(rushing_points = sum(rush_touchdown)*6 + 
                sum(yards_gained)*0.1 + 
                ifelse(sum(yards_gained)>=100,3,0) )
  
  # find total receiving points
  receiving_points <- pbp %>% 
    filter( 
      receiver_player_id == player_id, 
      play_type == "pass") %>%
    group_by(game_id) %>%
    summarise(receiving_points = sum(pass_touchdown)*6 + 
                sum(yards_gained)*0.1 +
                sum(+(!incomplete_pass)) +
                ifelse(sum(yards_gained)>=100,3,0) )
  
  # find total fumbling points
  fumbling_points <- pbp %>% 
    filter( 
      fumbled_1_player_id == player_id) %>%
    group_by(game_id) %>%
    summarise(fumble_points = -sum(fumble_lost) )
  
  
  # kickoff return TD points
  ko_ret_points <- pbp %>% 
    filter( 
      kickoff_returner_player_id == player_id, 
      play_type == "kickoff") %>%
    group_by(game_id) %>%
    summarise(koret_points = sum(return_touchdown)*6 )
  
  # punt return TD points
  punt_ret_points <- pbp %>% 
    filter( 
      punt_returner_player_id == player_id, 
      play_type == "punt") %>%
    group_by(game_id) %>%
    summarise(puret_points = sum(return_touchdown)*6 )
  
  # blocked FG return TD points
  punt_ret_points <- pbp %>% 
    filter( 
      td_player_id == player_id, 
      play_type == "field_goal") %>%
    group_by(game_id) %>%
    summarise(fgret_points = sum(touchdown)*6 )
  
  # two point conversion player points
  two_point_conv_passer_points <- pbp %>%
    filter(
      passer_player_id == player_id,
      two_point_conv_result == "success") %>%
    group_by(game_id) %>%
    summarise(pass_2ptconv_points = sum(two_point_attempt)*2 )
  
  # two point conversion rushing points
  two_point_conv_rusher_points <- pbp %>%
    filter(
      rusher_player_id == player_id,
      two_point_conv_result == "success") %>%
    group_by(game_id) %>%
    summarise(rush_2ptconv_points = sum(two_point_attempt)*2 )
  
  # two point conversion receiving points
  two_point_conv_receiver_points <- pbp %>%
    filter(
      receiver_player_id == player_id,
      two_point_conv_result == "success") %>%
    group_by(game_id) %>%
    summarise(rec_2ptconv_points = sum(two_point_attempt)*2 )
  
  # join all points into table
  total_points <- full_join(passing_points, rushing_points, by="game_id") %>%
    full_join(., receiving_points, by="game_id") %>%
    full_join(., fumbling_points, by="game_id") %>%
    full_join(., ko_ret_points, by="game_id") %>%
    full_join(., punt_ret_points, by="game_id") %>%
    full_join(., two_point_conv_passer_points, by="game_id") %>%
    full_join(., two_point_conv_receiver_points, by="game_id") %>%
    full_join(., two_point_conv_rusher_points, by="game_id")
  
  # add in the player_id
  total_points$player_id <- player_id
  
  return(total_points)
}

unique_players <- unique(roster$gsis_id)

dfs_points <- data.frame()
for(i in 1:length(unique_players)){
  dfs_points <- rbind(dfs_points, get_dfs_points(unique_players[i]))
}

saveRDS(dfs_points, "dfspoints20152020.rds")