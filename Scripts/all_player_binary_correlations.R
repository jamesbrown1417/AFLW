#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(future)
library(furrr)
`%notin%` <- Negate(`%in%`)

# Set up parallel processing
plan(multisession)

#===============================================================================
# Load in function
#===============================================================================

source("Functions/get_binary_correlations.R")

# Read in Current Season Data
afl_fantasy_2015_2023_data <- read_rds("../Data/afl_fantasy_2015_2023_data.rds")
afl_fantasy_2024_data <- read_rds("../Data/afl_fantasy_2024_data.rds")

# Combine
all_data <- bind_rows(afl_fantasy_2015_2023_data, afl_fantasy_2024_data)

#===============================================================================
# Get pairwise tibble of players and their medians
#===============================================================================

# Disposals---------------------------------------------------------------------
all_medians_disposals <-
combined_stats_2023 |>
  group_by(player_id, player_full_name, player_team) |>
  summarise(games_played = n(),
            median_disposals = median(disposals, na.rm = TRUE)) |> 
  filter(games_played >= 5) |> 
  ungroup() |> 
  select(-player_id) |> 
  mutate(median_disposals = if_else(median_disposals %% 1 == 0, median_disposals - 0.5, median_disposals))
  
# Get all pairwise combinations of two players on the same team (order doesn't matter)
pairwise_medians_disposals <-
  all_medians_disposals |> 
  inner_join(all_medians_disposals, by = "player_team", relationship = "many-to-many") |> 
  filter(player_full_name.x != player_full_name.y) |> 
  select(player_full_name.x, player_full_name.y, median_disposals.x, median_disposals.y) |> 
  rename(player_a = player_full_name.x,
         player_b = player_full_name.y,
         line_a = median_disposals.x,
         line_b = median_disposals.y) |> 
  filter(player_a < player_b) |> 
  mutate(stat = "disposals")

# Fantasy-----------------------------------------------------------------------
all_medians_fantasy_points <-
  combined_stats_2023 |>
  group_by(player_id, player_full_name, player_team) |>
  summarise(games_played = n(),
            median_fantasy_points = median(fantasy_points, na.rm = TRUE)) |> 
  filter(games_played >= 5) |> 
  ungroup() |> 
  select(-player_id) |> 
  mutate(median_fantasy_points = if_else(median_fantasy_points %% 1 == 0, median_fantasy_points - 0.5, median_fantasy_points))

# Get all pairwise combinations of two players on the same team (order doesn't matter)
pairwise_medians_fantasy_points <-
  all_medians_fantasy_points |> 
  inner_join(all_medians_fantasy_points, by = "player_team", relationship = "many-to-many") |> 
  filter(player_full_name.x != player_full_name.y) |> 
  select(player_full_name.x, player_full_name.y, median_fantasy_points.x, median_fantasy_points.y) |> 
  rename(player_a = player_full_name.x,
         player_b = player_full_name.y,
         line_a = median_fantasy_points.x,
         line_b = median_fantasy_points.y) |> 
  filter(player_a < player_b) |> 
  mutate(stat = "fantasy_points")

#===============================================================================
# Apply function to get binary correlations table
#===============================================================================

# Create safe version of function
get_binary_correlations <- safely(get_binary_correlations)

# Disposals---------------------------------------------------------------------
disposal_correlations <-
future_pmap(pairwise_medians_disposals, get_binary_correlations, .progress = TRUE) |> 
  map_dfr("result") |> 
  mutate(across(where(is.numeric), ~round(., 3))) |> 
  arrange(desc(correlation)) |> 
  filter(n_games >= 10)

# Fantasy-----------------------------------------------------------------------
fantasy_correlations <-
future_pmap(pairwise_medians_fantasy_points, get_binary_correlations, .progress = TRUE) |> 
  map_dfr("result") |> 
  mutate(across(where(is.numeric), ~round(., 3))) |> 
  arrange(desc(correlation)) |> 
  filter(n_games >= 10)
