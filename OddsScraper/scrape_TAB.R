# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
# tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/AFL%20Football/featured?jurisdiction=SA"
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/AFL%20Football/competitions/AFL%20Womens?homeState=SA&jurisdiction=SA"

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

main_tab <- function() {

# # Make request and get response
# tab_response <-
#     request(tab_url) |>
#     req_perform() |> 
#     resp_body_json()

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

# # Get index element of competitions with name value "AFL"
# names_list <- map(tab_response$competitions, "name")
# index <- which(names_list == "AFL")
# 
# # Get the response
# tab_response <- tab_response$competitions[[index]]

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

#===============================================================================
# Line markets
#===============================================================================

# Home teams
home_team_lines <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line" | market_name == "Pick Your Own Line") |> 
  rename(home_win = price) |>
  mutate(team = str_remove(prop_name, " [\\+\\-].*$")) |>
  filter(team == home_team) |>
  mutate(home_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id, -team) |> 
  mutate(line = abs(home_line)) |> 
  distinct(match, home_line, home_win, .keep_all = TRUE)

# Away teams
away_team_lines <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line" | market_name == "Pick Your Own Line") |> 
  rename(away_win = price) |>
  mutate(team = str_remove(prop_name, " [\\+\\-].*$")) |>
  filter(team == away_team) |>
  mutate(away_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id, -team) |> 
  mutate(line = abs(away_line)) |> 
  distinct(match, away_line, away_win, .keep_all = TRUE)

# Combine
tab_line_markets <-
  home_team_lines |>
  left_join(away_team_lines, relationship = "many-to-many") |> 
  mutate(market_name = "Line") |> 
  filter(home_line != away_line) |> 
  select(match, start_time, market_name, home_team, home_line, home_win, away_team, away_line, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "TAB")

# Fix team names
tab_line_markets <-
  tab_line_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_line_markets, "Data/scraped_odds/tab_line.csv")

#===============================================================================
# Totals
#===============================================================================

# Total Overs
totals_overs <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |> 
  filter(str_detect(prop_name, "Over")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Points") |>
  select(match, start_time, market_name, home_team, away_team, line, over_price = price) |> 
  distinct(match, line, over_price, .keep_all = TRUE)

# Total Unders
totals_unders <-
all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Total Points Over/Under" |
           market_name == "Pick Your Own Total") |> 
  filter(str_detect(prop_name, "Under")) |>
  mutate(line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |> 
  mutate(market_name = "Total Points") |>
  select(match, start_time, market_name, home_team, away_team, line, under_price = price) |> 
  distinct(match, line, under_price, .keep_all = TRUE)

# Combine
tab_totals_markets <-
  totals_overs |>
  left_join(totals_unders) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, start_time, market_name, home_team, away_team, line, over_price, under_price) |> 
  mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |> 
  mutate(agency = "TAB") |> 
  arrange(start_time, match, line)

# Write to csv
write_csv(tab_totals_markets, "Data/scraped_odds/tab_totals.csv")
 
#===============================================================================
# Player Disposals
#===============================================================================

# Filter to player disposals markets
player_disposals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Disposals$"))

# Alternate Player Disposals
alternate_player_disposals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Disposals$"))

# Extract player names
player_disposals_markets <-
    player_disposals_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under"))

alternate_player_disposals_markets <-
    alternate_player_disposals_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Disposals", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
    player_disposals_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Disposals") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_disposals_markets)

# Under lines
under_lines <-
    player_disposals_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Disposals") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_disposals_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_disposals_markets <-
    tab_player_disposals_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = case_when(
    player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
    player_name == "Mark OConnor" ~ "Mark O'Connor",
    player_name == "Massimo DAmbrosio" ~ "Massimo D'Ambrosio",
    player_name == "Xavier OHalloran" ~ "Xavier O'Halloran",
    .default = player_name
  )) |> 
    left_join(player_names, by = c("player_name" = "player_full_name")) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
    relocate(player_team, opposition_team, .after = player_name)

#===============================================================================
# Player Goals
#===============================================================================

# Filter to player goals markets
player_goals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Goals$"))

# Alternate Player Goals
alternate_player_goals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(\\d+\\+ Goals$)|(A Goal)")) |> 
    filter(!str_detect(market_name, "Quarter|Qtr")) |>
    filter(!str_detect(market_name, "Half")) |>
    filter(!str_detect(market_name, "Disposals")) |>
    mutate(market_name = if_else(str_detect(market_name, "A Goal"), "1+ Goals", market_name))

# Extract player names
player_goals_markets <-
    player_goals_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Goals", line - 0.5, line))

alternate_player_goals_markets <-
    alternate_player_goals_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Goals", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
    player_goals_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Goals") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_goals_markets)

# Under lines
under_lines <-
    player_goals_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Goals") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_goals_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_goals_markets <-
    tab_player_goals_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = case_when(
    player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
    player_name == "Matt Cottrell" ~ "Matthew Cottrell",
    player_name == "Mark OConnor" ~ "Mark O'Connor",
    player_name == "Massimo DAmbrosio" ~ "Massimo D'Ambrosio",
    player_name == "Xavier OHalloran" ~ "Xavier O'Halloran",
    .default = player_name
  )) |> 
  left_join(player_names, by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

#===============================================================================
# Player Marks
#===============================================================================

# Filter to player marks markets
player_marks_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "\\+ Marks$"))

# Alternate Player Marks
alternate_player_marks_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "(\\d+\\+ Marks$)")) |> 
  filter(!str_detect(market_name, "Quarter|Qtr")) |>
  filter(!str_detect(market_name, "Half")) |>
  filter(!str_detect(market_name, "Disposals"))

# Extract player names
player_marks_markets <-
  player_marks_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |> 
  mutate(line = if_else(market_name == "Alternate Player Marks", line - 0.5, line))

alternate_player_marks_markets <-
  alternate_player_marks_markets |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Marks", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
  player_marks_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Marks") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_marks_markets)

# Under lines
under_lines <-
  player_marks_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Marks") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_marks_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_marks_markets <-
  tab_player_marks_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = case_when(
    player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
    player_name == "Matt Cottrell" ~ "Matthew Cottrell",
    player_name == "Mark OConnor" ~ "Mark O'Connor",
    player_name == "Massimo DAmbrosio" ~ "Massimo D'Ambrosio",
    player_name == "Xavier OHalloran" ~ "Xavier O'Halloran",
    .default = player_name
  )) |> 
  left_join(player_names, by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

#===============================================================================
# Player Tackles
#===============================================================================

# Filter to player tackles markets
player_tackles_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "\\+ Tackles$"))

# Alternate Player Tackles
alternate_player_tackles_markets <-
  all_tab_markets |> 
  filter(str_detect(market_name, "(\\d+\\+ Tackles$)")) |> 
  filter(!str_detect(market_name, "Quarter|Qtr")) |>
  filter(!str_detect(market_name, "Half")) |>
  filter(!str_detect(market_name, "Disposals"))

# Extract player names
player_tackles_markets <-
  player_tackles_markets |> 
  filter(str_detect(prop_name, "Over|Under")) |>
  mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
  mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
  mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
  mutate(line = as.numeric(line)) |>
  mutate(type = str_detect(prop_name, "Over|\\+")) |> 
  mutate(type = ifelse(type, "Over", "Under")) |> 
  mutate(line = if_else(market_name == "Alternate Player Tackles", line - 0.5, line))

alternate_player_tackles_markets <-
  alternate_player_tackles_markets |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  mutate(line = str_extract(market_name, "\\d+")) |> 
  mutate(line = as.numeric(line) - 0.5) |> 
  transmute(match, market_name = "Player Tackles", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
  player_tackles_markets |> 
  filter(type == "Over") |> 
  mutate(market_name = "Player Tackles") |>
  select(match, market_name, player_name, line, over_price = price, prop_id) |> 
  bind_rows(alternate_player_tackles_markets)

# Under lines
under_lines <-
  player_tackles_markets |> 
  filter(type == "Under") |> 
  mutate(market_name = "Player Tackles") |>
  select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_tackles_markets <-
  over_lines |>
  full_join(under_lines) |> 
  select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
  mutate(agency = "TAB")

# Fix team names
tab_player_tackles_markets <-
  tab_player_tackles_markets |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  mutate(player_name = case_when(
    player_name == "Nasiah W-Milera" ~ "Nasiah Wanganeen-Milera",
    player_name == "Matt Cottrell" ~ "Matthew Cottrell",
    player_name == "Mark OConnor" ~ "Mark O'Connor",
    player_name == "Massimo DAmbrosio" ~ "Massimo D'Ambrosio",
    player_name == "Xavier OHalloran" ~ "Xavier O'Halloran",
    .default = player_name
  )) |> 
  left_join(player_names, by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

tab_player_disposals_markets |> write_csv("Data/scraped_odds/tab_player_disposals.csv")
tab_player_goals_markets |> write_csv("Data/scraped_odds/tab_player_goals.csv")
tab_player_marks_markets |> write_csv("Data/scraped_odds/tab_player_marks.csv")
tab_player_tackles_markets |> write_csv("Data/scraped_odds/tab_player_tackles.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
