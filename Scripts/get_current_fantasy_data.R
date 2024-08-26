# Get all afl fantasy in current season and save as an RDS file:

# Libraries and functions
library(purrr)
library(tidyverse)

# Function to fix team names
source("Functions/fix_team_names.R")

# Source functions
source("Functions/data_processing_functions.R")

# Apply function to years
afl_fantasy_2024_data <- get_fantasy_data(season = 2024)

# Fix team names
afl_fantasy_2024_data <-
  afl_fantasy_2024_data |> 
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste0(home_team, " Vs ", away_team))

# Output as an RDS object
saveRDS(afl_fantasy_2024_data, "Data/afl_fantasy_2024_data.rds")