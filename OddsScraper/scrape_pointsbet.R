# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Function to fix team names
source("Functions/fix_team_names.R")

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

pointsbet_h2h_main <- function() {
  # URL of website
  pointsbet_url = "https://api.pointsbet.com/api/v2/competitions/7523/events/featured?includeLive=false&page=1"
  
  # Make request and get response
  pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |>
    resp_body_json()
  
  # List of matches and data
  events <- pointsbet_response$events
  
  # Loop through to get all data--------------------------------------------------
  
  # Create empty vectors
  match_names <- c()
  match_starts_at <- c()
  home_teams <- c()
  away_teams <- c()
  event_names <- c()
  outcome_names <- c()
  outcome_prices <- c()
  keys <- c()
  
  # Loop through events
  for (match in events) {
    for (market in match$specialFixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, match$name)
        match_starts_at <- c(match_starts_at, match$startsAt)
        home_teams <- c(home_teams, match$homeTeam)
        away_teams <- c(away_teams, match$awayTeam)
        event_names <- c(event_names, market$eventName)
        outcome_names <- c(outcome_names, outcome$name)
        outcome_prices <- c(outcome_prices, outcome$price)
        keys <- c(keys, match$key)
      }
    }
  }
  
  # Output tibble
  pointsbet_data <-
    tibble(
      match = match_names,
      start_time = match_starts_at,
      home_team = home_teams,
      away_team = away_teams,
      event = event_names,
      outcome = outcome_names,
      price = outcome_prices
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time) |> 
    mutate(across(everything(), str_squish))
  
  #===============================================================================
  # Head to head markets
  #===============================================================================
  
  # Filter to head to head markets
  pointsbet_data_h2h <-
    pointsbet_data |>
    filter(event == "Match Result")
  
  # Home Teams
  pointsbet_data_h2h_home <-
    pointsbet_data_h2h |>
    filter(home_team == outcome) |>
    select(match,
           start_time,
           market = event,
           home_team,
           home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))
  
  # Away Teams
  pointsbet_data_h2h_away <-
    pointsbet_data_h2h |>
    filter(away_team == outcome) |>
    select(match,
           start_time,
           market = event,
           away_team,
           away_win = price) |> 
    mutate(away_team = fix_team_names(away_team))
  
  # Combine
  pointsbet_h2h <-
    full_join(
      pointsbet_data_h2h_home,
      pointsbet_data_h2h_away,
      by = c("match", "start_time", "market")
    ) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market = "Head To Head") |>
    select(match,
           start_time,
           market_name = market,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(home_win = as.numeric(home_win),
           away_win = as.numeric(away_win)) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Pointsbet")
  
  # Write to csv
  write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")
  
  #===============================================================================
  # Line Markets
  #===============================================================================
  
  # Filter to head to head markets
  pointsbet_data_line <-
    pointsbet_data |>
    filter(event == "Line")
  
  # Home Teams
  pointsbet_data_line_home <-
    pointsbet_data_line |>
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    mutate(home_line = as.numeric(str_extract(outcome, "-?\\d+\\.?\\d*"))) |> 
    select(-outcome)
  
  # Away Teams
  pointsbet_data_line_away <-
    pointsbet_data_line |>
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = price) |> 
    mutate(away_line = as.numeric(str_extract(outcome, "-?\\d+\\.?\\d*"))) |> 
    select(-outcome)
  
  # Combine
  pointsbet_line <-
    full_join(
      pointsbet_data_line_home,
      pointsbet_data_line_away
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market = "Line") |>
    select(match,
           start_time,
           market_name = market,
           home_team,
           home_line,
           home_win,
           away_team,
           away_line,
           away_win) |>
    mutate(home_win = as.numeric(home_win),
           away_win = as.numeric(away_win)) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Pointsbet")
  
  # Write to csv
  write_csv(pointsbet_line, "Data/scraped_odds/pointsbet_line.csv")
  
  #===============================================================================
  # Player Props
  #===============================================================================
  
  # Get unique keys
  keys <- unique(keys)
  
  # Get each match's api page
  match_urls <-
    paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)
  
  # Create a function that gets the player props from each URL
  get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors
    match_names <- c()
    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()
    event_key <- c()
    market_key <- c()
    outcome_key <- c()
    
    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
      for (outcome in market$outcomes) {
        # Append data to vectors
        match_names <- c(match_names, pointsbet_response$name)
        
        if (!is.null(market$name)) {
          market_names <- c(market_names, market$name)
        } else {
          market_names <- c(market_names, NA)
        }
        
        if (!is.null(outcome$name)) {
          outcome_names <- c(outcome_names, outcome$name)
        } else {
          outcome_names <- c(outcome_names, NA)
        }
        
        if (!is.null(outcome$outcomeType)) {
          outcome_types <- c(outcome_types, outcome$outcomeType)
        } else {
          outcome_types <- c(outcome_types, NA)
        }
        
        if (!is.null(outcome$price)) {
          outcome_prices <- c(outcome_prices, outcome$price)
        } else {
          outcome_prices <- c(outcome_prices, NA)
        }
        
        event_key <- c(event_key, pointsbet_response$key)
        
        if (!is.null(market$key)) {
          market_key <- c(market_key, market$key)
        } else {
          market_key <- c(market_key, NA)
        }
        
        if (!is.null(outcome$key)) {
          outcome_key <- c(outcome_key, outcome$key)
        } else {
          outcome_key <- c(outcome_key, NA)
        }
      }
    }
    
    # Output tibble
    tibble(
      match = match_names,
      market = market_names,
      outcome = outcome_names,
      outcome_type = outcome_types,
      price = outcome_prices,
      EventKey = event_key,
      MarketKey = market_key,
      OutcomeKey = outcome_key
    )
  }
  
  # Map function to each URL
  pointsbet_data_player_props <- map_df(match_urls, get_player_props)
  
  #===============================================================================
  # Player Disposals
  #===============================================================================
  
  # Player disposals alternative totals----------------------------------------------
  
  # Filter list to player disposals
  pointsbet_player_disposals_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Disposals")) |>
    filter(str_detect(market, "To Get")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      outcome = case_when(
        outcome == "Lebron James" ~ "LeBron James",
        .default = outcome
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Disposals",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Player disposals over / under----------------------------------------------------
  
  # Filter list to player disposals over under
  pointsbet_player_disposals_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Disposals Over/Under"))
  
  # Get Overs
  pointsbet_player_disposals_over <-
    pointsbet_player_disposals_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Disposals",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_disposals_under <-
    pointsbet_player_disposals_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Disposals",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey_unders = OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_disposals_over_under <-
    pointsbet_player_disposals_over |>
    left_join(pointsbet_player_disposals_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )
  
  #===============================================================================
  # Player Fantasy Points
  #===============================================================================
  
  # Player fantasy alternative totals----------------------------------------------
  
  # Filter list to player fantasy
  pointsbet_player_fantasy_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Fantasy")) |>
    filter(str_detect(market, "To Get")) |>
    mutate(line = str_extract(market, "[0-9]{1,3}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      outcome = case_when(
        outcome == "Lebron James" ~ "LeBron James",
        .default = outcome
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Fantasy",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Player fantasy over / under----------------------------------------------------
  
  # Filter list to player fantasy over under
  pointsbet_player_fantasy_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Fantasy Over/Under"))
  
  # Get Overs
  pointsbet_player_fantasy_over <-
    pointsbet_player_fantasy_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Fantasy",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_fantasy_under <-
    pointsbet_player_fantasy_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Fantasy",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey_unders = OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_fantasy_over_under <-
    pointsbet_player_fantasy_over |>
    left_join(pointsbet_player_fantasy_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )
  
  #===============================================================================
  # Player Goals
  #===============================================================================
  
  # Player goals alternative totals----------------------------------------------
  
  # Filter list to player goals
  pointsbet_player_goals_lines <-
    pointsbet_data_player_props |>
    mutate(market = ifelse(str_detect(market,"Anytime Goalscorer"), "To Kick 1+ Goals", market)) |>
    filter(str_detect(market, "^To Kick \\d+\\+ Goals")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
      outcome = case_when(
        outcome == "Lebron James" ~ "LeBron James",
        .default = outcome
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Goals",
      player_name = outcome,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Player goals over / under----------------------------------------------------
  
  # Filter list to player goals over under
  pointsbet_player_goals_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Goals Over/Under"))
  
  # Get Overs
  pointsbet_player_goals_over <-
    pointsbet_player_goals_over_under |>
    filter(str_detect(outcome, "Over")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Over ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Goals",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  # Get Unders
  pointsbet_player_goals_under <-
    pointsbet_player_goals_over_under |>
    filter(str_detect(outcome, "Under")) |>
    mutate(player_name = outcome) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " Under ") |>
    mutate(line = as.numeric(line)) |>
    mutate(match = str_replace(match, "@", "v")) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Lebron James" ~ "LeBron James",
        .default = player_name
      )
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Goals",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      under_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey_unders = OutcomeKey
    )
  
  # Combine overs and unders
  pointsbet_player_goals_over_under <-
    pointsbet_player_goals_over |>
    left_join(pointsbet_player_goals_under) |>
    select(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      opposition_team,
      line,
      over_price,
      under_price,
      agency,
      contains("Key")
    )
  
  #===============================================================================
  # Player Marks
  #===============================================================================
  
  # Player marks alternative totals----------------------------------------------
  
  # Filter list to player marks
  pointsbet_player_marks_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pick Your Own Marks")) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " To Take ") |> 
    mutate(line = str_extract(line, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Marks",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  #===============================================================================
  # Player Tackles
  #===============================================================================
  
  # Player tackles alternative totals----------------------------------------------
  
  # Filter list to player tackles
  pointsbet_player_tackles_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Pick Your Own Tackles")) |>
    separate(outcome,
             into = c("player_name", "line"),
             sep = " To Record ") |> 
    mutate(line = str_extract(line, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    separate(
      match,
      into = c("home_team", "away_team"),
      sep = " v ",
      remove = FALSE
    ) |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == team_name, away_team, home_team)) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name = "Player Tackles",
      player_name,
      player_team = team_name,
      opposition_team,
      line,
      over_price = price,
      agency = "Pointsbet",
      EventKey,
      MarketKey,
      OutcomeKey
    )
  
  #===============================================================================
  # Write to CSV
  #===============================================================================
  
  # Disposals
  pointsbet_player_disposals_lines |>
    bind_rows(pointsbet_player_disposals_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
      "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Disposals") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_player_disposals.csv")
  
  # Goals
  pointsbet_player_goals_lines |>
    bind_rows(pointsbet_player_goals_over_under) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
      "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Goals") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_player_goals.csv")
  
  # Fantasy Points
  pointsbet_player_fantasy_lines |>
    bind_rows(pointsbet_player_fantasy_over_under) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "under_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
      "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Fantasy Points") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_player_fantasy_points.csv")
  
  # Tackles
  pointsbet_player_tackles_lines |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Player Tackles") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_player_tackles.csv")
  
  # Marks
  pointsbet_player_marks_lines |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(
      "match",
      "home_team",
      "away_team",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "over_price",
      "agency",
      "opposition_team",
      "EventKey",
      "MarketKey",
      "OutcomeKey",
    ) |>
    mutate(market_name = "Player Marks") |>
    mutate(agency = "Pointsbet") |>
    write_csv("Data/scraped_odds/pointsbet_player_marks.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()
