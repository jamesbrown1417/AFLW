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

betr_url = "https://api.betr.ext.thebetmakers.com/api/v4/combined/sports/basecompetition/events-with-markets?base_competition_id=1000265&main_markets_only=true"

# Make request and get response
betr_response <-
  request(betr_url) |>
  req_perform() |> 
  resp_body_json()

all_betr_markets <- map_df(betr_response$data$competitions[[1]]$events, ~{
  match_name <- .x$name
  map_df(.x$markets, ~{
    market_name <- .x$name
    map_df(.x$selections, ~{
      tibble(
        match = match_name,
        market = market_name,
        selection = .x$name,
        line = .x$selection_prices[[1]]$line,
        price = .x$selection_prices[[1]]$price
      )
    })
  })
}) |> 
  filter(price != 1)


#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betr_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market, "match winner")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = price) |>
  select(-market, -selection, -line)

# Away teams
away_teams <-
  all_betr_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market, "match winner")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = price) |>
  select(-market, -selection, -line)

# Combine
betr_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Betr")

# Fix team names
betr_head_to_head_markets <-
  betr_head_to_head_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betr_head_to_head_markets, "Data/scraped_odds/betr_h2h.csv")

#===============================================================================
# Line Markets
#===============================================================================

# Home teams
home_teams_line <-
  all_betr_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(selection, "(\\-)|(\\+)")) |>
  mutate(market_name = "Line") |> 
  separate(selection, into = c("team", "line"), sep = "\\-|\\+", remove = FALSE) |>
  mutate(team = trimws(team)) |>
  group_by(match) |> 
  filter(home_team == team) |> 
  rename(home_win = price) |> 
  mutate(handicap = str_extract(selection, "[\\-\\+]\\d+\\.\\d")) |> 
  mutate(home_line = as.numeric(handicap)) |>
  select(-market, -selection, -handicap, -line, -team)

# Away teams
away_teams_line <-
  all_betr_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(selection, "(\\-)|(\\+)")) |>
  mutate(market_name = "Line") |>
  separate(selection, into = c("team", "line"), sep = "\\-|\\+", remove = FALSE) |>
  mutate(team = trimws(team)) |>
  group_by(match) |>
  filter(away_team == team) |>
  rename(away_win = price) |>
  mutate(handicap = str_extract(selection, "[\\-\\+]\\d+\\.\\d")) |>
  mutate(away_line = as.numeric(handicap)) |>
  select(-market, -selection, -handicap, -line, -team)

# Combine
betr_line_markets <-
  home_teams_line |>
  left_join(away_teams_line) |> 
  select(match, market_name, home_team, home_win, home_line, away_team, away_win, away_line) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "Betr")

# Fix team names
betr_line_markets <-
  betr_line_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betr_line_markets, "Data/scraped_odds/betr_line.csv")
