#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
library(uwot)  # Changed from umap to uwot
library(dbscan)
`%notin%` <- Negate(`%in%`)

# Source fix team names function
source("Functions/fix_team_names.R")

#===============================================================================
# Read in Position Data
#===============================================================================

# Get Player Attributes
player_attributes_2024 <-
fetch_player_details_afl(season = 2024, comp = "AFLW")

# Get Player Positions
player_positions <-
  player_attributes_2024 |> 
  transmute(player_full_name = paste(firstName, surname, sep = " "),
            player_team = fix_team_names(team),
            position)