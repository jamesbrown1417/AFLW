#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

source("Functions/fix_team_names.R")

#===============================================================================
# Get fixture for current season
#===============================================================================

# Get fixtures
current_fixture <- fetch_fixture_afl(comp = "AFLW")

# Tidy dataset
current_fixture <-
  current_fixture |>
  mutate(
    home.team.name = fix_team_names(home.team.name),
    away.team.name = fix_team_names(away.team.name)
  ) |>
  transmute(
    match = paste(home.team.name, "v", away.team.name),
    round = round.name,
    home_team = home.team.name,
    away_team = away.team.name,
    start_time = ymd_hms(utcStartTime),
    venue = venue.name
  )

# Convert start_time to adelaide time
current_fixture$start_time <- with_tz(current_fixture$start_time, tzone = "Australia/Adelaide")

#===============================================================================
# Save as RDS File
#===============================================================================

# Save as RDS file
write_rds(current_fixture, "Data/current_fixture.rds")
