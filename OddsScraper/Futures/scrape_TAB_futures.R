# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/AFL%20Football/featured?jurisdiction=SA"

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

main_tab <- function() {

# Make request and get response
tab_response <-
    request(tab_url) |>
    req_perform() |> 
    resp_body_json()

tab_response <- tab_response$competitions[[2]]

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# Map functions to data
all_tab_markets <-
    map(tab_response$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match",
           "start_time",
           "market_name")),
           prop_name = propositions_name,
           prop_id = propositions_id,
           price = propositions_returnWin)

#===============================================================================
# Premiership Winner
#===============================================================================

# Home teams
premiership_winner <-
    all_tab_markets |>
    filter(market_name == "Winner") |>
    transmute(market_name = "Premiership Winner",
              selection = prop_name,
              price) |> 
  arrange(price)

#===============================================================================
# To Make Grand Final
#===============================================================================

# Home teams
grand_finalist <-
  all_tab_markets |>
  filter(market_name == "To Make Grand Final") |>
  transmute(market_name = "Grand Finalist",
            selection = prop_name,
            price) |> 
  arrange(price)

#===============================================================================
# Minor Premiership
#===============================================================================

# Home teams
minor_premiership <-
  all_tab_markets |>
  filter(market_name == "Minor Premiership") |>
  transmute(market_name = "Minor Premiership",
            selection = prop_name,
            price) |> 
  arrange(price)

#===============================================================================
# Top 8
#===============================================================================

# Home teams
top_8 <-
  all_tab_markets |>
  filter(market_name == "Final 8") |>
  transmute(market_name = "Top 8",
            selection = prop_name,
            price) |> 
  arrange(price)

#===============================================================================
# To Miss Top 8
#===============================================================================

# Home teams
miss_top_8 <-
  all_tab_markets |>
  filter(market_name == "To Miss Top 8") |>
  transmute(market_name,
            selection = prop_name,
            price) |> 
  arrange(price)

#===============================================================================
# Top 4
#===============================================================================

top_4 <-
  all_tab_markets |>
  filter(market_name == "Top 4") |>
  transmute(market_name = "Top 4",
            selection = prop_name,
            price) |> 
  arrange(price)

#===============================================================================
# To Miss Top 4
#===============================================================================

miss_top_4 <-
  all_tab_markets |>
  filter(market_name == "To Miss Top 4") |>
  transmute(market_name,
            selection = prop_name,
            price) |> 
  arrange(price)

}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
