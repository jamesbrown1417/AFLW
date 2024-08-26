#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(forecast)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Fantasy Data
#===============================================================================

# Read in all data and relevant variables
combined_stats <-
  read_rds("Data/afl_fantasy_2015_2023_data.rds") |> 
  bind_rows(read_rds("Data/afl_fantasy_2024_data.rds")) |>
  filter(tog_percentage >= 60) |> 
  mutate(home_away = if_else(home_team == player_team, "home", "away")) |> 
  select(player_full_name, home_away, fantasy_points, start_time_utc,tog_percentage)

#===============================================================================
# Create Simple ETS Forecast model
#===============================================================================

# Create a simple ETS model on Lachie Neale's data
lachie_neale_data <-
  combined_stats |> 
  filter(player_full_name == "Lachie Neale") |> 
  select(start_time_utc,home_away, fantasy_points) |> 
  arrange(start_time_utc) |> 
  filter(start_time_utc >= "2023-01-01") |> 
  filter(home_away == "home")

# Create a time series object
lachie_neale_ts <- ts(lachie_neale_data$fantasy_points, frequency = 1)

# Plot the time series
autoplot(lachie_neale_ts)

# Fit an ETS model
lachie_neale_ets <- ets(lachie_neale_ts)

# Forecast next game and provide prediction intervals
lachie_neale_forecast <- forecast(lachie_neale_ets, h = 1)

# Plot the forecast
autoplot(lachie_neale_forecast)

# Get probability of exceeding 100 points
lachie_neale_forecast$mean > 100

# Get prediction intervals
lachie_neale_forecast$lower

lachie_neale_forecast$upper

