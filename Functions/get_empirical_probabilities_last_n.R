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

current_season_stats <- read_rds("Data/afl_fantasy_2024_data.rds")

combined_stats <- bind_rows(combined_stats, current_season_stats)

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_full_name, line, stat) {
  
  # Get name of last season
  last_season = as.character(as.numeric(format(Sys.Date(), "%Y")) - 1)
  
  # Filter for player
  player_stats <-
    combined_stats |> 
    tidytable::filter(player_full_name == !!player_full_name) |> 
    tidytable::arrange(tidytable::desc(start_time_utc))
  
  # Ensure 'stat' column exists
  if(!stat %in% names(player_stats)) {
    stop("Stat column does not exist in the dataset")
  }
  
  # Calculate proportion of games above 'line' for last 3, 5, 7, and 10 games
  last_games_stats <-
    player_stats |> 
    tidytable::mutate(
      above_line = as.numeric(!!sym(stat) > line), # Convert logical to numeric for summing
      emp_prob_last_3 = rollapply(above_line, width = 3, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "left"),
      emp_prob_last_5 = rollapply(above_line, width = 5, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "left"),
      emp_prob_last_7 = rollapply(above_line, width = 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "left"),
      emp_prob_last_10 = rollapply(above_line, width = 10, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, align = "left")
    ) |> 
    mutate(sample_size = n()) |>
    tidytable::mutate(
      emp_prob_last_3 = ifelse(sample_size < 3, NA, emp_prob_last_3),
      emp_prob_last_5 = ifelse(sample_size < 5, NA, emp_prob_last_5),
      emp_prob_last_7 = ifelse(sample_size < 7, NA, emp_prob_last_7),
      emp_prob_last_10 = ifelse(sample_size < 10, NA, emp_prob_last_10)
    ) |>
    tidytable::select(-sample_size) |>
    tidytable::slice_head(n = 1) |> 
    tidytable::mutate(line = line) |> 
    tidytable::select(
      player_full_name,
      line,
      emp_prob_last_3,
      emp_prob_last_5,
      emp_prob_last_7,
      emp_prob_last_10
    )
  
  return(last_games_stats)
}
