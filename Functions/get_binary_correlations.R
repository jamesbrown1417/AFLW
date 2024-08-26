#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Data
#===============================================================================

combined_stats <-
  read_rds("Data/afl_fantasy_2015_2023_data.rds")

combined_stats_2023 <-
  combined_stats |> 
  tidytable::filter(season_name == "2023" | season_name == "2024")

#===============================================================================
# Create Function
#===============================================================================

get_binary_correlations <- function(player_a, player_b, stat, line_a, line_b) {
  # Get all of player A's games last season
  player_a_stats <-
    combined_stats_2023 |> 
    tidytable::filter(player_full_name == player_a) |> 
    tidytable::arrange(tidytable::desc(start_time_utc)) |> 
    tidytable::select(match_name, round, season_name, start_time_utc, player_a_name = player_full_name, player_a_stat = !!sym(stat))
  
  # Get all of player B's games last season
  player_b_stats <-
    combined_stats_2023 |> 
    tidytable::filter(player_full_name == player_b) |> 
    tidytable::arrange(tidytable::desc(start_time_utc)) |> 
    tidytable::select(match_name, round, season_name, start_time_utc, player_b_name = player_full_name, player_b_stat = !!sym(stat))
  
  # Perform inner join
  player_a_b_stats <-
    player_a_stats |> 
    tidytable::inner_join(player_b_stats) |> 
    tidytable::select(match_name, round, player_a_name, player_a_stat, player_b_name, player_b_stat)
  
  # Create variable for whether stat is over line
  player_a_b_stats <-
    player_a_b_stats |> 
    tidytable::mutate(player_a_above_line = player_a_stat > line_a) |> 
    tidytable::mutate(player_b_above_line = player_b_stat > line_b)
  
  # Convert logical to numeric (binary) if needed
  player_a_b_stats$player_a_above_line <- as.numeric(player_a_b_stats$player_a_above_line)
  player_a_b_stats$player_b_above_line <- as.numeric(player_a_b_stats$player_b_above_line)
  
  # Create a contingency table
  table <- table(player_a_b_stats$player_a_above_line, player_a_b_stats$player_b_above_line)
  
  # Calculate the Phi coefficient
  phi <- function(table) {
    n11 <- table[2, 2]
    n00 <- table[1, 1]
    n10 <- table[2, 1]
    n01 <- table[1, 2]
    sqrt_n <- sqrt(sum(table[2, ]) * sum(table[1, ]) * sum(table[, 2]) * sum(table[, 1]))
    phi <- (n11 * n00 - n10 * n01) / sqrt_n
    return(phi)
  }
  
  phi_coefficient <- phi(table)
  
  # Create a tibble for output
  tibble(
    player_a = player_a,
    line_a = line_a,
    player_b = player_b,
    line_b = line_b,
    stat = stat,
    n_games = nrow(player_a_b_stats),
    correlation = phi_coefficient
  )
}
