#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Data
#===============================================================================

combined_stats <-
  read_rds("Data/afl_fantasy_2015_2023_data.rds") |> 
  bind_rows(read_rds("Data/afl_fantasy_2024_data.rds")) |>
  mutate(home_away = if_else(home_team == player_team, "home", "away"))

combined_stats_2023 <-
  combined_stats |> 
  tidytable::filter(season_name %in% c("2023", "2024"))

#===============================================================================
# Function to compare home vs away performance
#===============================================================================

compare_home_vs_away_performance <- function(player, stat) {
  # Get all of player's games last season
  player_stats <-
    combined_stats_2023 |> 
    tidytable::filter(player_full_name == player) |> 
    tidytable::arrange(tidytable::desc(start_time_utc)) |> 
    tidytable::select(match_name, round, season_name, start_time_utc, home_away, player_name = player_full_name, player_stat = !!sym(stat), home_team, away_team)

  # Get median of player's stat grouped by home / away status
  home_away_medians <-
  player_stats |> 
    tidytable::group_by(player_name, home_away) |> 
    tidytable::summarise(median_stat = median(player_stat, na.rm = TRUE))
  
  # Get difference between home and away median
    home_away_medians |> 
      tidytable::pivot_wider(names_from = home_away, values_from = median_stat) |> 
      tidytable::mutate(diff = home - away) |> 
      tidytable::mutate(stat = stat)
}