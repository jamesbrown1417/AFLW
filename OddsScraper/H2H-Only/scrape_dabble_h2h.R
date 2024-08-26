# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
competitions_api_url = "https://api.dabble.com.au/competitions/ad4c78ec-e39d-45ee-8cec-ff5d485a3205/sport-fixtures"

# Function to fix team names
source("Functions/fix_team_names.R")

# Make request and get response
dabble_response <-
  request(competitions_api_url) |>
  req_perform() |>
  resp_body_json()

# Function to get fixture details
get_fixture_details <- function(data) {
  match <- data$name
  match_id <- data$id
  
  # Get Prices Information
  all_prices <-
    map_dfr(data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })
  
  # Get Markets Information
  all_markets <-
    map_dfr(data$markets, ~{
      tibble(
        market_name = .x$name,
        marketId = .x$id
      )
    })
  
  # Get Selections Information
  all_selections <-
    map_dfr(data$selections, ~{
      tibble(
        selection_name = .x$name,
        selectionId = .x$id
      )
    })
  
  # Return tibble
  all_prices |>
    left_join(all_markets) |> 
    left_join(all_selections) |> 
    mutate(match = match, id = match_id)
  
}

# Map over data
data_list <- data <- dabble_response$data
fixture_details <- map_dfr(data_list, get_fixture_details)

#===============================================================================
# Get H2H Data
#===============================================================================

all_h2h <-
  fixture_details |> 
  filter(market_name == "Head to Head") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(selection_name = fix_team_names(selection_name))

# Home Teams
home_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(home_team == selection_name) |>
  rename(home_win = price) |>
  select(match, home_team, away_team, market_name, home_win)

# Away teams
away_teams <-
  all_h2h |>
  mutate(market_name = "Head To Head") |> 
  filter(away_team == selection_name) |>
  rename(away_win = price) |>
  select(match, home_team, away_team, market_name, away_win)

# Combine
dabble_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Dabble")

# Write to csv
write_csv(dabble_head_to_head_markets, "Data/scraped_odds/dabble_h2h.csv")