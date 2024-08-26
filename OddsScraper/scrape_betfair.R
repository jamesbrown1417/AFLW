# Libraries and functions
library(tidyverse)
library(httr2)
library(jsonlite)
library(abettor)

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to convert betfair odds
convert_odds <- function(listed_odds, commission = 0.05) {
  # Calculate the actual odds
  actual_odds <- (listed_odds - 1) * (1 - commission) + 1
  return(actual_odds)
}

# Function to fix team names
source("Functions/fix_team_names.R")

# Get ENV Variables
betfair_username <- Sys.getenv("BETFAIR_USER")
betfair_password <- Sys.getenv("BETFAIR_PASS")
betfair_app_key <- Sys.getenv("BETFAIR_APP")

# Log in to API
loginBF(username = betfair_username,
        password = betfair_password,
        applicationKey = betfair_app_key)

# All AFL Markets
all_afl_markets <-
  listMarketTypes(
  eventTypeIds = 61420,
  # marketTypeCodes = "MATCH_ODDS",
  competitionIds = 11897406,
  toDate = (format(Sys.time() + 86400 * 60, "%Y-%m-%dT%TZ")) ## Look ahead until the next 60 days
)

# Create a function that takes marekTypeCode and returns the market catalogue info
enumerate_markets <- function(marketTypeCode) {
  # Get market catalogue
  market_catalogue <-
    listMarketCatalogue(
      eventTypeIds = 61420,
      marketTypeCodes = marketTypeCode,
      competitionIds = 11897406,
      toDate = (format(Sys.time() + 86400 * 60, "%Y-%m-%dT%TZ")) ## Look ahead until the next 60 days
    )
  
  # Get event names
  event_names <- market_catalogue$event$name
  event_ids <- market_catalogue$event$id
  market_ids <- market_catalogue$marketId
  market_names <- market_catalogue$marketName
  
  # Get runners table
  runners_table <-
    market_catalogue$runners |>
    imap( ~ mutate(.x, row_num = .y)) |>
    bind_rows() |> 
    select(runnerName, row_num, runnerId = selectionId, handicap)
  
  # Create a tibble
  output_tibble <-
    tibble(
      event_name = event_names,
      event_id = event_ids,
      market_id = market_ids,
      market_name = market_names
    ) |> 
    mutate(row_num = row_number())
  
  # Join based on row num
  output_tibble <-
    output_tibble |>
    left_join(runners_table, by = "row_num") |>
    select(-row_num)
  
  # Return market catalogue
  return(output_tibble)
}

# All market names and IDs
all_afl_market_codes <-
map(all_afl_markets$marketType, enumerate_markets) |>
  bind_rows() |> 
  filter(event_name != "AFL")

#===============================================================================
# Get Head to Head Odds
#===============================================================================

# Define the function to get head-to-head odds for a given market ID
get_head_to_head_odds <- function(market_id, all_afl_market_codes) {
  # Get the market book for the given market ID
  head_to_head_odds <- listMarketBook(marketIds = market_id, priceData = "EX_BEST_OFFERS") |>
    pull(runners) |>
    bind_rows() |>
    as_tibble() |>
    unnest(6) |>
    unnest(6) |>
    select(runnerId = selectionId, price, size) |> 
    mutate(market_id = market_id) |> 
    left_join(all_afl_market_codes, by = c("market_id", "runnerId"))
  
  # Get home odds
  home_odds <-
    head_to_head_odds |>
    separate(
      event_name,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team),
           runnerName = fix_team_names(runnerName)) |>
    mutate(event_name = paste(home_team, "v", away_team)) |>
    filter(runnerName == home_team) |>
    mutate(market_name = "Head To Head") |> 
    select(
      match = event_name,
      home_team,
      away_team,
      home_win = price,
      home_liquidity = size,
      market_name
    ) |>
    arrange(desc(home_win)) |>
    slice_head(n = 1)
  
  # Get away odds
  away_odds <- head_to_head_odds |>
    separate(
      event_name,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team),
           runnerName = fix_team_names(runnerName)) |>
    mutate(event_name = paste(home_team, "v", away_team)) |>
    filter(runnerName == away_team) |>
    mutate(market_name = "Head To Head") |> 
    select(
      match = event_name,
      home_team,
      away_team,
      away_win = price,
      away_liquidity = size,
      market_name
    ) |>
    arrange(desc(away_win)) |>
    slice_head(n = 1)
  
  # Join home and away odds
  head_to_head_odds <- home_odds |>
    left_join(away_odds, by = c("match", "home_team", "away_team", "market_name")) |>
    mutate(home_win = convert_odds(home_win),
           away_win = convert_odds(away_win)) |> 
    mutate(agency = "Betfair")
  
  return(head_to_head_odds)
}

# Get all MATCH_ODDS codes
match_odds_codes <-
  all_afl_market_codes |>
  filter(market_name == "Match Odds") |>
  pull(market_id) |> 
  unique()

# Get safe version of function
safe_get_head_to_head_odds <- safely(get_head_to_head_odds)

# Get head to head odds for all MATCH_ODDS codes
all_head_to_head_odds <-
  map(match_odds_codes, safe_get_head_to_head_odds, all_afl_market_codes, .progress = TRUE) |> 
  # Get result part
  map("result") |>
  # Keep only non null
  keep(~ !is.null(.x)) |>
  bind_rows()

# Write to CSV
write_csv(all_head_to_head_odds, "Data/scraped_odds/betfair_h2h.csv")

#===============================================================================
# Get Disposal Lines
#===============================================================================

# Define the function to get head-to-head odds for a given market ID
get_disposal_odds <- function(market_id, all_afl_market_codes) {
  # Get the market book for the given market ID
  disposal_odds <- listMarketBook(marketIds = market_id, priceData = "EX_BEST_OFFERS") |>
    pull(runners) |>
    bind_rows() |>
    as_tibble() |>
    unnest(6) |>
    unnest(6) |>
    select(runnerId = selectionId, price, size) |> 
    mutate(market_id = market_id) |> 
    left_join(all_afl_market_codes, by = c("market_id", "runnerId"))
  
  # Get over odds
  over_odds <-
    disposal_odds |>
    separate(
      event_name,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    filter(str_detect(runnerName, "Over")) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team),
           runnerName = fix_team_names(runnerName)) |>
    mutate(event_name = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(market_name, "Player Disposals - ")) |>
    mutate(market_name = "Player Disposals") |> 
    mutate(line = str_remove(runnerName, "Over ")) |>
    mutate(line = str_remove(line, " Disposals")) |>
    mutate(line = as.numeric(line)) |>
    select(
      match = event_name,
      home_team,
      away_team,
      player_name,
      line,
      over_price = price,
      over_liquidity = size,
      market_name
    ) |>
    arrange(desc(over_price)) |>
    slice_head(n = 1)
  
  # Get under odds
  under_odds <- disposal_odds |>
    separate(
      event_name,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    filter(str_detect(runnerName, "Under")) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team),
           runnerName = fix_team_names(runnerName)) |>
    mutate(event_name = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(market_name, "Player Disposals - ")) |>
    mutate(market_name = "Player Disposals") |> 
    mutate(line = str_remove(runnerName, "Under ")) |>
    mutate(line = str_remove(line, " Disposals")) |>
    mutate(line = as.numeric(line)) |>
    select(
      match = event_name,
      home_team,
      away_team,
      player_name,
      line,
      under_price = price,
      under_liquidity = size,
      market_name
    ) |>
    arrange(desc(under_price)) |>
    slice_head(n = 1)
  
  # Join over and under odds
  disposal_odds_combined <-
    over_odds |>
    left_join(under_odds) |> 
    mutate(over_price = convert_odds(over_price),
           under_price = convert_odds(under_price)) |> 
    mutate(agency = "Betfair")
  
  return(disposal_odds_combined)
}

# Get all MATCH_ODDS codes
disposal_codes <-
  all_afl_market_codes |>
  filter(str_detect(market_name, "Player Disposals")) |>
  pull(market_id) |> 
  unique()

# Get safe version of function
safe_get_disposal_odds <- safely(get_disposal_odds)

# Get disposal odds for all MATCH_ODDS codes
all_disposal_odds <-
  map(disposal_codes, safe_get_disposal_odds, all_afl_market_codes, .progress = TRUE) |> 
  # Get result part
  map("result") |>
  # Keep only non null
  keep(~ !is.null(.x)) |>
  bind_rows()

# Add player teams to disposals
all_disposal_odds <-
  all_disposal_odds |> 
left_join(player_names, by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

# Write to CSV
write_csv(all_disposal_odds, "Data/scraped_odds/betfair_player_disposals.csv")
