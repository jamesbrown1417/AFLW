# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/AFL%20Football/featured?jurisdiction=SA"
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/AFL%20Football/competitions/AFL?homeState=SA&jurisdiction=SA"


# Function to fix team names
source("Functions/fix_team_names.R")

  # Function to fetch and parse JSON with exponential backoff
  fetch_data_with_backoff <-
    function(url,
             delay = 1,
             max_retries = 5,
             backoff_multiplier = 2) {
      tryCatch({
        # Attempt to fetch and parse the JSON
        tab_response <-
          read_html_live(url) |>
          html_nodes("pre") %>%
          html_text() %>%
          fromJSON(simplifyVector = FALSE)
        
        # Return the parsed response
        return(tab_response)
      }, error = function(e) {
        if (max_retries > 0) {
          # Log the retry attempt
          message(sprintf("Error encountered. Retrying in %s seconds...", delay))
          
          # Wait for the specified delay
          Sys.sleep(delay)
          
          # Recursively call the function with updated parameters
          return(
            fetch_data_with_backoff(
              url,
              delay * backoff_multiplier,
              max_retries - 1,
              backoff_multiplier
            )
          )
        } else {
          # Max retries reached, throw an error
          stop("Failed to fetch data after multiple retries.")
        }
      })
    }
  
  tab_response <- fetch_data_with_backoff(tab_url)

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
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name, -prop_id)

# Away teams
away_teams <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = price) |> 
  select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")
