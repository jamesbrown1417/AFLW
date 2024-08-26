# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=79"

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Keep only matches
matches <-
  matches |> 
  keep(~ .x$masterEventClassName == "Matches")

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$marketDesc
  market_propositions = market$outcomeName
  market_prices = market$price
  market_handicaps = market$points
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices,
         handicaps = market_handicaps)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices,
    handicaps = market_info$handicaps
  )
}

# Map functions to data
all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Win")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Win")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names
betright_head_to_head_markets <-
  betright_head_to_head_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Line Markets
#===============================================================================

# Home teams
home_teams_line <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Handicap")) |> 
  mutate(market_name = "Line") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  rename(home_line = handicaps) |>
  select(-propositions)

# Away teams
away_teams_line <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Handicap")) |> 
  mutate(market_name = "Line") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  rename(away_line = handicaps) |>
  select(-propositions)

# Combine
betright_line_markets <-
  home_teams_line |>
  left_join(away_teams_line) |> 
  select(match, start_time, market_name, home_team, home_win, home_line, away_team, away_win, away_line) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Fix team names
betright_line_markets <-
  betright_line_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_line_markets, "Data/scraped_odds/betright_line.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Disposals
player_disposals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G312&format=json")

# Player Disposal Over/Under
player_disposal_ou_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G669&format=json")

# Player Goals
player_goals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G311&format=json")

# Afl Fantasy
player_fantasy_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G513&format=json")

# Marks
player_marks_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G592&format=json")

# Tackles
player_tackles_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G593&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  event_id <- c()
  outcome_title <- c()
  outcome_name <- c()
  outcome_id <- c()
  group_by_header <- c()
  fixed_market_id <- c()
  price <- c()
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      event_id <- c(event_id, event$eventId)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      outcome_id <- c(outcome_id, outcome$outcomeId)
      group_by_header <- c(group_by_header, outcome$groupByHeader)
      fixed_market_id <- c(fixed_market_id, outcome$fixedMarketId)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    event_id = event_id,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    outcome_id = outcome_id,
    group_by_header = group_by_header,
    fixed_market_id = fixed_market_id,
    price = price,
    link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

# Get player disposals data--------------------------------------------------------

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# All Data

# Disposals
betright_player_disposals_alternate <-
  map(player_disposals_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Disposals O/U
betright_player_disposals_over_under_all <- tryCatch({
  
  betright_player_disposals_over_under_all <-
  map(player_disposal_ou_links, safe_get_prop_data) |>
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |>
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |>
    left_join(match_names) |>
    filter(!is.na(outcome_name))
  
}, error = function(e) {
  NULL  # This will assign NULL to 'betright_player_disposals_over_under_all' if an error occurs
})

# Combine
betright_player_disposals_all <-
  bind_rows(
    betright_player_disposals_alternate,
    betright_player_disposals_over_under_all
  )

# Get Overs (over under markets)
betright_player_disposals_overs <-
  betright_player_disposals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(
    match,
    player_name,
    line,
    over_price,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  )

# Get Unders (over under markets)
betright_player_disposals_unders <-
  betright_player_disposals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(
    match,
    player_name,
    line,
    under_price,
    group_by_header,
    event_id,
    outcome_name_under = outcome_name,
    outcome_id_under = outcome_id,
    fixed_market_id_under = fixed_market_id
  )

# Get alternate player disposals markets
betright_alternate_disposals <-
  betright_player_disposals_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_player_disposals <-
  betright_player_disposals_overs |>
  bind_rows(betright_alternate_disposals) |>
  left_join(betright_player_disposals_unders) |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Disposals") |>
  separate(match,
           into = c("home_team", "away_team"),
           sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(
    player_name = case_when(
      str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
      str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
      str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
      str_detect(player_name, "Harrison Himmelberg") ~ "Harry Himmelberg",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(
    opposition_team = case_when(
      team_name == away_team ~ home_team,
      team_name == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Disposals",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_name_under,
    outcome_id,
    outcome_id_under,
    fixed_market_id,
    fixed_market_id_under
  )

# Get player goals data--------------------------------------------------------

# Goals
betright_player_goals_alternate <-
  map(player_goals_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  filter(str_detect(outcome_title, "(Quarter)|(Half)", negate = TRUE)) |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Goals O/U
betright_player_goals_over_under_all <- tryCatch({
  
  map(player_goals_over_under_links, safe_get_prop_data) |>
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |>
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |>
    left_join(match_names) |>
    filter(!is.na(outcome_name))
  
}, error = function(e) {
  NULL  # This will assign NULL to 'betright_player_goals_over_under_all' if an error occurs
})

# Combine
betright_player_goals_all <-
  bind_rows(
    betright_player_goals_alternate,
    betright_player_goals_over_under_all
  )

# Get Overs (over under markets)
betright_player_goals_overs <-
  betright_player_goals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Over")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Over ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  select(
    match,
    player_name,
    line,
    over_price,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  )

# Get Unders (over under markets)
betright_player_goals_unders <-
  betright_player_goals_all |>
  filter(str_detect(outcome_title, "Over/Under")) |>
  filter(str_detect(outcome_name, "Under")) |> 
  separate(outcome_name, into = c("player_name", "line"), sep = " Under ", remove = FALSE) |> 
  mutate(line = as.numeric(line)) |> 
  rename(under_price = price) |> 
  select(
    match,
    player_name,
    line,
    under_price,
    group_by_header,
    event_id,
    outcome_name_under = outcome_name,
    outcome_id_under = outcome_id,
    fixed_market_id_under = fixed_market_id
  )

# Get alternate player goals markets
betright_alternate_goals <-
  betright_player_goals_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_player_goals <-
  betright_player_goals_overs |>
  bind_rows(betright_alternate_goals) |>
  left_join(betright_player_goals_unders) |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Goals") |>
  separate(match,
           into = c("home_team", "away_team"),
           sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(
    player_name = case_when(
      str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
      str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
      str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
      str_detect(player_name, "Harrison Himmelberg") ~ "Harry Himmelberg",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(
    opposition_team = case_when(
      team_name == away_team ~ home_team,
      team_name == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Goals",
    player_name,
    player_team = team_name,
    line,
    over_price,
    under_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_name_under,
    outcome_id,
    outcome_id_under,
    fixed_market_id,
    fixed_market_id_under
  )

# Get player fantasy_points data--------------------------------------------------------

# Fantasy Points
betright_player_fantasy_points_alternate <-
  map(player_fantasy_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows()

if (ncol(betright_player_fantasy_points_alternate) < 2) {
  betright_player_fantasy_points_alternate$outcome_title <- NA
  betright_player_fantasy_points_alternate$outcome_name <- NA
  betright_player_fantasy_points_alternate$outcome_id <- NA
  betright_player_fantasy_points_alternate$fixed_market_id <- NA
  betright_player_fantasy_points_alternate$price <- NA
  betright_player_fantasy_points_alternate$group_by_header <- NA
  betright_player_fantasy_points_alternate$event_id <- NA
}

betright_player_fantasy_points_alternate <-
  betright_player_fantasy_points_alternate |> 
  filter(str_detect(outcome_title, "(Quarter)|(Half)", negate = TRUE)) |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_fantasy_points_all <- betright_player_fantasy_points_alternate
  
# Get alternate player fantasy_points markets
betright_alternate_fantasy_points <-
  betright_player_fantasy_points_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_title, "Fantasy Points - ")) |> 
  mutate(line = str_extract(outcome_name, "\\d+")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_player_fantasy_points <-
  betright_alternate_fantasy_points |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Fantasy Points") |>
  separate(match,
           into = c("home_team", "away_team"),
           sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(
    player_name = case_when(
      str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
      str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
      str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
      str_detect(player_name, "Harrison Himmelberg") ~ "Harry Himmelberg",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(
    opposition_team = case_when(
      team_name == away_team ~ home_team,
      team_name == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Fantasy Points",
    player_name,
    player_team = team_name,
    line,
    over_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  ) |> 
  # Convert anything logical to numeric
  mutate_if(is.logical, as.numeric)

# Get player marks data--------------------------------------------------------

# Marks
betright_player_marks_alternate <-
  map(player_marks_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows()

if (ncol(betright_player_marks_alternate) < 2) {
  betright_player_marks_alternate$outcome_title <- NA
  betright_player_marks_alternate$outcome_name <- NA
  betright_player_marks_alternate$outcome_id <- NA
  betright_player_marks_alternate$fixed_market_id <- NA
  betright_player_marks_alternate$price <- NA
  betright_player_marks_alternate$group_by_header <- NA
  betright_player_marks_alternate$event_id <- NA
}

betright_player_marks_alternate <-
  betright_player_marks_alternate |> 
  filter(str_detect(outcome_title, "(Quarter)|(Half)", negate = TRUE)) |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_marks_all <- betright_player_marks_alternate

# Get alternate player marks markets
betright_alternate_marks <-
  betright_player_marks_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_player_marks <-
  betright_alternate_marks |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Marks") |>
  separate(match,
           into = c("home_team", "away_team"),
           sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(
    player_name = case_when(
      str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
      str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
      str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
      str_detect(player_name, "Ashley Johnson") ~ "Ash Johnson",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(
    opposition_team = case_when(
      team_name == away_team ~ home_team,
      team_name == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Marks",
    player_name,
    player_team = team_name,
    line,
    over_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
    fixed_market_id
  ) |> 
  # Convert anything logical to numeric
  mutate_if(is.logical, as.numeric)


# Get player tackles data--------------------------------------------------------

# Tackles
betright_player_tackles_alternate <-
  map(player_tackles_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows()

if (ncol(betright_player_tackles_alternate) < 2) {
  betright_player_tackles_alternate$outcome_title <- NA
  betright_player_tackles_alternate$outcome_name <- NA
  betright_player_tackles_alternate$outcome_id <- NA
  betright_player_tackles_alternate$fixed_market_id <- NA
  betright_player_tackles_alternate$price <- NA
  betright_player_tackles_alternate$group_by_header <- NA
  betright_player_tackles_alternate$event_id <- NA
}

betright_player_tackles_alternate <-
  betright_player_tackles_alternate |> 
  filter(str_detect(outcome_title, "(Quarter)|(Half)", negate = TRUE)) |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Combine
betright_player_tackles_all <- betright_player_tackles_alternate

# Get alternate player tackles markets
betright_alternate_tackles <-
  betright_player_tackles_all |>
  filter(str_detect(outcome_title, "Over/Under", negate = TRUE)) |> 
  mutate(player_name = str_remove(outcome_name, " \\d+\\+$")) |> 
  mutate(line = str_extract(outcome_name, "\\d+\\+$")) |>
  mutate(line = str_remove(line, "\\+$")) |>
  mutate(line = as.integer(line) - 0.5) |> 
  rename(over_price = price) |> 
  select(match, player_name, line, over_price, group_by_header, event_id, outcome_name, outcome_id, fixed_market_id)

# Combine
betright_player_tackles <-
  betright_alternate_tackles |>
  mutate(agency = "BetRight") |>
  mutate(market_type = "Player Tackles") |>
  separate(match,
           into = c("home_team", "away_team"),
           sep = " v ") |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(
    player_name = case_when(
      str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
      str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
      str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
      str_detect(player_name, "Harrison Himmelberg") ~ "Harry Himmelberg",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(
    opposition_team = case_when(
      team_name == away_team ~ home_team,
      team_name == home_team ~ away_team
    )
  ) |>
  transmute(
    match = paste0(home_team, " v ", away_team),
    home_team,
    away_team,
    market_name = "Player Tackles",
    player_name,
    player_team = team_name,
    line,
    over_price,
    agency = "BetRight",
    opposition_team,
    group_by_header,
    event_id,
    outcome_name,
    outcome_id,
  ) |> 
  # Convert anything logical to numeric
  mutate_if(is.logical, as.numeric)

#===============================================================================
# Write to CSV
#===============================================================================

betright_player_disposals |> write_csv("Data/scraped_odds/betright_player_disposals.csv")
betright_player_goals |> write_csv("Data/scraped_odds/betright_player_goals.csv")
betright_player_marks |> write_csv("Data/scraped_odds/betright_player_marks.csv")
betright_player_tackles |> write_csv("Data/scraped_odds/betright_player_tackles.csv")
betright_player_fantasy_points |> write_csv("Data/scraped_odds/betright_player_fantasy_points.csv")

