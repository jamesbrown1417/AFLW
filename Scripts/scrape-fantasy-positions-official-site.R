##%######################################################%##
#                                                          #
####          Scrape the afl player positions           ####
####              from the afl fantasy API              ####
#                                                          #
##%######################################################%##

# libraries and functions
library(tidyverse)
library(request)

# Source fix team names function
source("Functions/fix_team_names.R")

# Get URL of data from API
url = "https://aflwfantasy.afl/json/fantasy/players.json"

# Get Data from API
scraped_fantasy_data <-
  api(url) %>%
  http()

##%######################################################%##
#                                                          #
####     Create a function to extract relevant data     ####
#                                                          #
##%######################################################%##

get_afl_api_player_data <- function(player_data){
  # Name and price data
  first_name = player_data$firstName
  last_name = player_data$lastName
  team_id = player_data$squadId
  team_name = player_data$squad$name
  price = player_data$cost

  # Position Boolean Variables
  forward_status = "FWD" %in% player_data$position
  ruck_status =  "RUC" %in% player_data$position
  midfield_status = "MID" %in% player_data$position
  defender_status = "DEF" %in% player_data$position
  
  # Return Dataframe with each derived variable as a column
  tibble(
    first_name,
    last_name,
    team_id,
    team_name,
    price,
    forward_status,
    ruck_status,
    midfield_status,
    defender_status
  )
}

##%######################################################%##
#                                                          #
####                  Get output data                   ####
#                                                          #
##%######################################################%##

afl_player_api_data <-
  scraped_fantasy_data |> 
  map(get_afl_api_player_data) |> 
  reduce(bind_rows) |>
  mutate(player_full_name = paste(first_name, last_name)) |> 
  mutate(team_name = fix_team_names(team_name)) |>
  relocate(player_full_name, team_name, .after = last_name)

# Write out as starting data for 2024 season
write_rds(afl_player_api_data, "Data/2024_start_positions_and_prices.rds")

afl_player_api_data |> openxlsx::write.xlsx("players.xlsx")
