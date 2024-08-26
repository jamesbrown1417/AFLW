# Get all aflw data since 2023 and save as an RDS file:

# Libraries and functions
library(purrr)
library(tidyverse)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Function to fix team names
source("Functions/fix_team_names.R")

# Source functions
source("Functions/data_processing_functions.R")

# Vector of years
years = 2023:2023

# Apply function to years
afl_fantasy_2023_2023_data <-
  future_map(years, get_fantasy_data, .progress = TRUE)

# Bind together the tibbles
afl_fantasy_2023_2023_data <-
  afl_fantasy_2023_2023_data |> 
  reduce(dplyr::bind_rows)

# Fix team names
afl_fantasy_2023_2023_data <-
  afl_fantasy_2023_2023_data |> 
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match = paste0(home_team, " Vs ", away_team))

# Output as an RDS object
saveRDS(afl_fantasy_2023_2023_data, "Data/afl_fantasy_2023_2023_data.rds")
