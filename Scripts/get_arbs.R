# library(shiny)
library(tidyverse)

#===============================================================================
# Read in Data
#===============================================================================

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# Get Start Times
start_times <-
  all_odds_files |>
  select(match, start_time) |>
  distinct(match, .keep_all = TRUE)

# For each match, get all home wins
all_home <-
  all_odds_files |>
  arrange(match, start_time, desc(home_win)) |>
  select(match, start_time, market_name, home_team, home_win, home_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# For each match, get all away wins
all_away <-
  all_odds_files |>
  arrange(match, start_time, desc(away_win)) |>
  select(match, start_time, market_name, away_team, away_win, away_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# Combine
all_odds_h2h <-
  all_home |>
  full_join(all_away, relationship = "many-to-many", by = c("match", "market_name")) |>
  mutate(margin = (1/home_win + 1/away_win)) |> 
  mutate(margin = round(100*(margin - 1), digits = 3)) |> 
  arrange(margin)

##%######################################################%##
#                                                          #
####                   Player Disposals                 ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_disposals <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_disposals") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####              Player Fantasy Points                 ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_fantasy_points <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_fantasy_points") |>
  map(read_csv, num_threads = 12 ) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows)

##%######################################################%##
#                                                          #
####   Get all over under comparisons of same market    ####
#                                                          #
##%######################################################%##

# H2H---------------------------------------------------------------------------
h2h_arbs <-
  all_odds_h2h |> 
  mutate(margin = -1*margin) |> 
  filter(margin > 0)

# Disposals------------------------------------------------------------------------
disposals_unders <-
  all_player_disposals |>
  filter(market_name == "Player Disposals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

disposals_overs <-
  all_player_disposals |>
  filter(market_name == "Player Disposals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

disposals_arbs <-
  disposals_unders |>
  inner_join(
    disposals_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Fantasy Points----------------------------------------------------------------------
fantasy_points_unders <-
  all_player_fantasy_points |>
  filter(market_name == "Player Fantasy Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

fantasy_points_overs <-
  all_player_fantasy_points |>
  filter(market_name == "Player Fantasy Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

fantasy_points_arbs <-
  fantasy_points_unders |>
  inner_join(
    fantasy_points_overs,
    by = c(
      "match",
      
      "market_name",
      "player_name",
      "player_team",
      "line",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

#===============================================================================
# Get all ARBs together
#===============================================================================

all_arbs <-
  bind_rows(
    disposals_arbs,
    fantasy_points_arbs
  ) |>
  arrange(desc(margin)) |> 
  filter(!is.na(player_name))

# H2H Arbs
h2h_arbs
all_arbs
