# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Get fixtures data
current_season_fixture <- read_rds("Data/current_fixture.rds")

# # Google sheets authentification -----------------------------------------------
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
# gs4_auth(token = drive_token())

# Run all odds scraping scripts-------------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
run_scraping("OddsScraper/H2H-Only//scrape_betr_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_BetRight_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_pointsbet_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_sportsbet_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_TAB_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_TopSport_h2h.R")
# run_scraping("OddsScraper/H2H-Only//scrape_bet365_h2h.R")
# run_scraping("OddsScraper/H2H-Only//Neds/scrape_neds_h2h.R")
run_scraping("OddsScraper/H2H-Only//scrape_unibet.R")
run_scraping("OddsScraper/H2H-Only//scrape_dabble_h2h.R")

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
  arrange(margin) |>
  left_join(current_season_fixture) |>
  relocate(round, start_time, venue, .after = match)

# Write as RDS
all_odds_h2h |> write_rds("Data/processed_odds/all_h2h.rds")