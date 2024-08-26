#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(zoo)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats <-
  read_rds("Data/afl_fantasy_2015_2023_data.rds")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob_season <- function(player_full_name, line, stat) {
  
  # Get name of last season
  last_season = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1)
  
  # Filter for player
  player_stats <-
    combined_stats |> 
    tidytable::filter(season_name == last_season) |>
    tidytable::filter(player_full_name == !!player_full_name) |> 
    tidytable::arrange(tidytable::desc(start_time_utc))
  
  # Ensure 'stat' column exists
  if(!stat %in% names(player_stats)) {
    stop("Stat column does not exist in the dataset")
  }
  
  # Calculate proportion of games above 'line' for season
  last_season_stats <-
    player_stats |> 
    tidytable::mutate(
      above_line = as.numeric(!!sym(stat) > line)) |> 
    summarise(
      n_games_2023 = n(),
      emp_prob_2023 = mean(above_line, na.rm = TRUE)
    )
  
  return(last_season_stats)
}
