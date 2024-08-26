# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
competitions_api_url = "https://api.dabble.com.au/competitions/ad4c78ec-e39d-45ee-8cec-ff5d485a3205/sport-fixtures"

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

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

#===============================================================================
# Get Fixture Details
#===============================================================================

fixtures <-
  all_h2h |>
  distinct(match, id)

#===============================================================================
# Prop Data
#===============================================================================

# Get List of Fixture URLs
fixture_urls <- paste0("https://api.dabble.com.au/sportfixtures/details/", fixtures$id)

# Function to get fixture details
get_fixture_details <- function(url) {
  
  # Get response from URL
  fixture_response <-
    request(url) |>
    req_perform() |>
    resp_body_json()
  
  # Get Prices Information
  all_prices <-
    map_dfr(fixture_response$data$prices, ~{
      tibble(
        id = .x$id,
        marketId = .x$marketId,
        selectionId = .x$selectionId,
        price = .x$price
      )
    })
  
  # Get Markets Information
  all_markets <-
    map_dfr(fixture_response$data$markets, ~{
      tibble(
        market_name = .x$name,
        id = .x$id
      )
    })
  
  
  # Get Selections Information
  all_selections <-
    map_dfr(fixture_response$data$selections, ~{
      tibble(
        selection_name = .x$name,
        id = .x$id
      )
    })
  
  # Combine together
  all_prices |>
    left_join(all_markets, by = c("marketId" = "id")) |>
    left_join(all_selections, by = c("selectionId" = "id")) |> 
    mutate(match_id = str_remove(url, "https://api.dabble.com.au/sportfixtures/details/"))
}

# Map over data
prop_data <- map_dfr(fixture_urls, get_fixture_details, .progress = TRUE)

# Add match details
prop_data <-
  prop_data |>
  left_join(fixtures, by = c("match_id" = "id"))

#===============================================================================
# Get Player Disposals
#===============================================================================

# Filter to player disposals markets
player_disposals_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Disposals O/U"))

# Alternate Player Disposals
alternate_player_disposals_markets <-
  prop_data |> 
  filter(str_detect(market_name, "To Have"))

# Extract player names
player_disposals_markets <-
  player_disposals_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_disposals_markets <-
  alternate_player_disposals_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Disposals", player_name, line, over_price = price)

# Over lines
over_lines <-
  player_disposals_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Disposals") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_disposals_markets)

# Under lines
under_lines <-
  player_disposals_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Disposals") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_disposals_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Get Player Goals
#===============================================================================

# Filter to player goals markets
player_goals_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Goals O/U"))

# Alternate Player Goals
alternate_player_goals_markets <-
  prop_data |>
  mutate(market_name = if_else(market_name == "Anytime Goal Scorer", "To Kick 1+ Goals", market_name)) |>
  mutate(market_name = if_else(market_name == "Anytime Goalscorer", "To Kick 1+ Goals", market_name)) |>
  filter(str_detect(market_name, "To Kick")) |> 
  filter(str_detect(market_name, "(Half$)|(Quarter$)", negate = TRUE))

# Extract player names
player_goals_markets <-
  player_goals_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_goals_markets <-
  alternate_player_goals_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Goals", player_name, line, over_price = price)

# Over lines
over_lines <-
  player_goals_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Goals") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_goals_markets)

# Under lines
under_lines <-
  player_goals_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Goals") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_goals_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)


#===============================================================================
# Get Player Fantasy
#===============================================================================

# Filter to player fantasy_points markets
player_fantasy_points_markets <-
  prop_data |> 
  filter(str_detect(market_name, "Fantasy Points"))

# Alternate Player Fantasy
alternate_player_fantasy_points_markets <-
  prop_data |>
  filter(str_detect(market_name, "Fantasy Points"))

# Extract player names
player_fantasy_points_markets <-
  player_fantasy_points_markets |> 
  filter(str_detect(selection_name, "Over|Under")) |>
  mutate(player_name = str_extract(selection_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(selection_name, "[0-9]{1}[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(selection_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under"))

alternate_player_fantasy_points_markets <-
  alternate_player_fantasy_points_markets |>
  mutate(player_name = str_remove(selection_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Fantasy Points", player_name, line, over_price = price)

# Over lines
over_lines <-
  player_fantasy_points_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Fantasy Points") |>
  select(match, market_name, player_name, line, over_price = price) |> 
  bind_rows(alternate_player_fantasy_points_markets)

# Under lines
under_lines <-
  player_fantasy_points_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Fantasy Points") |>
  select(match, market_name, player_name, line, under_price = price)

# Combine
dabble_player_fantasy_points_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price) |> 
  mutate(agency = "Dabble") |> 
  distinct(match, market_name, player_name, line, .keep_all = TRUE)

#===============================================================================
# Fix team and player names-----------------------------------------------------
#===============================================================================

# Fix player names--------------------------------------------------------------

# Disposals
dabble_player_disposals_markets <-
  dabble_player_disposals_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "Cameron Rayner") ~ "Cam Rayner",
    str_detect(player_name, "Lachlan Ash") ~ "Lachie Ash",
    str_detect(player_name, "Mitch Hinge") ~ "Mitchell Hinge",
    str_detect(player_name, "Matthew Roberts") ~ "Matt Roberts",
    str_detect(player_name, "Connor MacDonald") ~ "Connor Macdonald",
    str_detect(player_name, "Cameron Mackenzie") ~ "Cam Mackenzie",
    .default = player_name
  )) |>
  left_join(player_names,
            by = c("player_name" = "player_full_name"))

# Goals
dabble_player_goals_markets <-
  dabble_player_goals_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "Cameron Rayner") ~ "Cam Rayner",
    str_detect(player_name, "Lachlan Ash") ~ "Lachie Ash",
    str_detect(player_name, "Matt Cottrell") ~ "Matthew Cottrell",
    str_detect(player_name, "Tom Berry") ~ "Thomas Berry",
    str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
    str_detect(player_name, "Mitch Hinge") ~ "Mitchell Hinge",
    str_detect(player_name, "Matthew Roberts") ~ "Matt Roberts",
    str_detect(player_name, "Connor MacDonald") ~ "Connor Macdonald",
    str_detect(player_name, "Cameron Mackenzie") ~ "Cam Mackenzie",
    str_detect(player_name, "Joshua Rachele") ~ "Josh Rachele",
    str_detect(player_name, "Tom J. Lynch") ~ "Tom Lynch",
    str_detect(player_name, "Mitchell Lewis") ~ "Mitch Lewis",
    str_detect(player_name, "Ollie Dempsey") ~ "Oliver Dempsey",
    .default = player_name
  )) |>
  left_join(player_names,
            by = c("player_name" = "player_full_name"))

# Fantasy Points
dabble_player_fantasy_points_markets <-
  dabble_player_fantasy_points_markets |>
  mutate(player_name = case_when(
    str_detect(player_name, "Cameron Rayner") ~ "Cam Rayner",
    str_detect(player_name, "Lachlan Ash") ~ "Lachie Ash",
    str_detect(player_name, "Matt Cottrell") ~ "Matthew Cottrell",
    str_detect(player_name, "Tom Berry") ~ "Thomas Berry",
    str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
    str_detect(player_name, "Mitch Hinge") ~ "Mitchell Hinge",
    str_detect(player_name, "Matthew Roberts") ~ "Matt Roberts",
    str_detect(player_name, "Connor MacDonald") ~ "Connor Macdonald",
    str_detect(player_name, "Cameron Mackenzie") ~ "Cam Mackenzie",
    .default = player_name
  )) |>
  left_join(player_names,
            by = c("player_name" = "player_full_name"))

# Fix Team Names----------------------------------------------------------------

# Points
dabble_player_disposals_markets <-
  dabble_player_disposals_markets |>
  rename(player_team = team_name) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Goals
dabble_player_goals_markets <-
  dabble_player_goals_markets |>
  rename(player_team = team_name) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

# Fantasy Points
dabble_player_fantasy_points_markets <-
  dabble_player_fantasy_points_markets |>
  rename(player_team = team_name) |>
  separate(match, c("home_team", "away_team"), sep = " v ") |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, away_team, sep = " v ")) |>
  mutate(opposition_team = case_when(
    player_team == home_team ~ away_team,
    player_team == away_team ~ home_team
  )) |>
  select(
    match,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    market_name,
    line,
    over_price,
    under_price,
    agency
  )

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

dabble_player_disposals_markets |> write_csv("Data/scraped_odds/dabble_player_disposals.csv")
dabble_player_goals_markets |> write_csv("Data/scraped_odds/dabble_player_goals.csv")
dabble_player_fantasy_points_markets |> write_csv("Data/scraped_odds/dabble_player_fantasy_points.csv")
