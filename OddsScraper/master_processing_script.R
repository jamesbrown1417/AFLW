# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Get empirical probability function
source("Functions/get_empirical_probabilities_last_n.R")

# Read in 2023 empirical probabilities
empirical_probabilities_2023 <- read_rds("Data/empirical_probabilities_2023.rds")

# Get fixtures data
current_season_fixture <- read_rds("Data/current_fixture.rds")

# Run all odds scraping scripts-------------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# List of scripts to run
scraping_scripts <- c(
  "OddsScraper/scrape_betr.R",
  "OddsScraper/scrape_BetRight.R",
  # "OddsScraper/scrape_Palmerbet.R",
  "OddsScraper/scrape_pointsbet.R",
  "OddsScraper/scrape_sportsbet.R",
  "OddsScraper/scrape_TAB.R",
  "OddsScraper/scrape_TopSport.R",
  "OddsScraper/scrape_bet365.R",
  # "OddsScraper/scrape_bluebet.R",
  "OddsScraper/Neds/scrape_neds.R",
  # "OddsScraper/scrape_unibet.R",
  "OddsScraper/scrape_dabble.R",
  "OddsScraper/scrape_unibet.R",
  "OddsScraper/scrape_betfair.R"
)

# Run all odds scraping scripts in parallel
future_map(scraping_scripts, run_scraping)

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
  select(match, start_time, market_name, home_team, home_win, home_liquidity, home_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# For each match, get all away wins
all_away <-
  all_odds_files |>
  arrange(match, start_time, desc(away_win)) |>
  select(match, start_time, market_name, away_team, away_win, away_liquidity, away_agency = agency) |> 
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

# # Google Sheets-----------------------------------------------------
# sheet <- gs4_find("AFL Data")
# 
# write_sheet(sheet, data = all_odds_h2h, sheet = "H2H")

# Write as RDS
all_odds_h2h |> write_rds("Data/processed_odds/all_h2h.rds")

##%######################################################%##
#                                                          #
####                      Line                          ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "line") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# Arrange to get biggest discrepancies
all_odds_line <-
  all_odds_files |>
  arrange(match, start_time, home_line) |>
  group_by(match, home_team, away_team) |> 
  mutate(min_home_line = min(home_line), max_home_line = max(home_line)) |>
  mutate(diff = max_home_line - min_home_line) |>
  ungroup() |>
  arrange(desc(diff), match, desc(home_line)) |> 
  select(-min_home_line, -max_home_line)
# 
# # Google Sheets-----------------------------------------------------
# write_sheet(sheet, data = all_odds_line, sheet = "Line")

# Write as RDS
all_odds_line |> write_rds("Data/processed_odds/all_line.rds")

##%######################################################%##
#                                                          #
####                 Player Disposals                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_disposals <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "disposals") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Add empirical probabilities---------------------------------------------------

# Disposals
distinct_disposal_combos <-
  all_player_disposals |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "disposals")

player_emp_probs_2023 <-
  empirical_probabilities_2023 |>
  filter(stat == "disposals") |>
  select(-stat) |> 
  rename(player_name = player_full_name,
         empirical_prob_over_2023 = emp_prob_2023)
  
player_emp_probs_2024 <-
  future_pmap(distinct_disposal_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

all_player_disposals <-
  all_player_disposals |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs_2023, by = c("player_name", "line")) |>
  left_join(player_emp_probs_2024, by = c("player_name", "line")) |>
  mutate(empirical_prob_under_2023 = 1 - empirical_prob_over_2023) |> 
  mutate(empirical_prop_under_last_3 = 1 - emp_prob_last_3,
         empirical_prop_under_last_5 = 1 - emp_prob_last_5,
         empirical_prop_under_last_7 = 1 - emp_prob_last_7,
         empirical_prop_under_last_10 = 1 - emp_prob_last_10) |> 
  mutate(
    diff_over_2023 = empirical_prob_over_2023 - implied_prob_over,
    diff_over_last_3 = emp_prob_last_3 - implied_prob_over,
    diff_over_last_5 = emp_prob_last_5 - implied_prob_over,
    diff_over_last_7 = emp_prob_last_7 - implied_prob_over,
    diff_over_last_10 = emp_prob_last_10 - implied_prob_over,
    diff_under_2023 = empirical_prob_under_2023 - implied_prob_under,
    diff_under_last_3 = empirical_prop_under_last_3 - implied_prob_under,
    diff_under_last_5 = empirical_prop_under_last_5 - implied_prob_under,
    diff_under_last_7 = empirical_prop_under_last_7 - implied_prob_under,
    diff_under_last_10 = empirical_prop_under_last_10 - implied_prob_under) |> 
  mutate_if(is.double, round, 2) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line)

# # Add to google sheets
# write_sheet(sheet, data = all_player_disposals, sheet = "Player Disposals")

# Write as RDS
all_player_disposals |> write_rds("Data/processed_odds/all_player_disposals.rds")

##%######################################################%##
#                                                          #
####                   Player Goals                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_goals <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "goals") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Add empirical probabilities---------------------------------------------------

# goals
distinct_goal_combos <-
  all_player_goals |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "goals")

player_emp_probs_2023 <-
  empirical_probabilities_2023 |>
  filter(stat == "goals") |>
  select(-stat) |> 
  rename(player_name = player_full_name,
         empirical_prob_over_2023 = emp_prob_2023)

player_emp_probs_2024 <-
  future_pmap(distinct_goal_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

all_player_goals <-
  all_player_goals |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs_2023, by = c("player_name", "line")) |>
  left_join(player_emp_probs_2024, by = c("player_name", "line")) |>
  mutate(empirical_prob_under_2023 = 1 - empirical_prob_over_2023) |>
  mutate(empirical_prop_under_last_3 = 1 - emp_prob_last_3,
         empirical_prop_under_last_5 = 1 - emp_prob_last_5,
         empirical_prop_under_last_7 = 1 - emp_prob_last_7,
         empirical_prop_under_last_10 = 1 - emp_prob_last_10) |> 
  mutate(
    diff_over_2023 = empirical_prob_over_2023 - implied_prob_over,
    diff_over_last_3 = emp_prob_last_3 - implied_prob_over,
    diff_over_last_5 = emp_prob_last_5 - implied_prob_over,
    diff_over_last_7 = emp_prob_last_7 - implied_prob_over,
    diff_over_last_10 = emp_prob_last_10 - implied_prob_over,
    diff_under_2023 = empirical_prob_under_2023 - implied_prob_under,
    diff_under_last_3 = empirical_prop_under_last_3 - implied_prob_under,
    diff_under_last_5 = empirical_prop_under_last_5 - implied_prob_under,
    diff_under_last_7 = empirical_prop_under_last_7 - implied_prob_under,
    diff_under_last_10 = empirical_prop_under_last_10 - implied_prob_under) |>
  mutate_if(is.double, round, 2) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line)

# # Add to google sheets
# write_sheet(sheet, data = all_player_goals, sheet = "Player Goals")

# Write as RDS
all_player_goals |> write_rds("Data/processed_odds/all_player_goals.rds")

##%######################################################%##
#                                                          #
####               Player Fantasy Points                ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_fantasy_points <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "fantasy_points") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Add empirical probabilities---------------------------------------------------

# fantasy_points
distinct_point_combos <-
  all_player_fantasy_points |>
  distinct(player_name, line) |>
  rename(player_full_name = player_name) |>
  mutate(stat = "fantasy_points")

player_emp_probs_2023 <-
  empirical_probabilities_2023 |>
  filter(stat == "fantasy_points") |>
  select(-stat) |> 
  rename(player_name = player_full_name,
         empirical_prob_over_2023 = emp_prob_2023)

player_emp_probs_2024 <-
  future_pmap(distinct_point_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

all_player_fantasy_points <-
  all_player_fantasy_points |>
  mutate(
    implied_prob_over = 1 / over_price,
    implied_prob_under = 1 / under_price
  ) |>
  left_join(player_emp_probs_2023, by = c("player_name", "line")) |>
  left_join(player_emp_probs_2024, by = c("player_name", "line")) |>
  mutate(empirical_prob_under_2023 = 1 - empirical_prob_over_2023) |>
  mutate(empirical_prop_under_last_3 = 1 - emp_prob_last_3,
         empirical_prop_under_last_5 = 1 - emp_prob_last_5,
         empirical_prop_under_last_7 = 1 - emp_prob_last_7,
         empirical_prop_under_last_10 = 1 - emp_prob_last_10) |>
  mutate(
    diff_over_2023 = empirical_prob_over_2023 - implied_prob_over,
    diff_over_last_3 = emp_prob_last_3 - implied_prob_over,
    diff_over_last_5 = emp_prob_last_5 - implied_prob_over,
    diff_over_last_7 = emp_prob_last_7 - implied_prob_over,
    diff_over_last_10 = emp_prob_last_10 - implied_prob_over,
    diff_under_2023 = empirical_prob_under_2023 - implied_prob_under,
    diff_under_last_3 = empirical_prop_under_last_3 - implied_prob_under,
    diff_under_last_5 = empirical_prop_under_last_5 - implied_prob_under,
    diff_under_last_7 = empirical_prop_under_last_7 - implied_prob_under,
    diff_under_last_10 = empirical_prop_under_last_10 - implied_prob_under) |>
  mutate_if(is.double, round, 2) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line)

# # Add to google sheets
# write_sheet(sheet, data = all_player_fantasy_points, sheet = "Player Fantasy Points")

# Write as RDS
all_player_fantasy_points |> write_rds("Data/processed_odds/all_player_fantasy_points.rds")

##%######################################################%##
#                                                          #
####                   Player Marks                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_marks <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "marks") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Add empirical probabilities---------------------------------------------------

# marks
distinct_mark_combos <-
  all_player_marks |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "marks")

player_emp_probs_2023 <-
  empirical_probabilities_2023 |>
  filter(stat == "marks") |>
  select(-stat) |> 
  rename(player_name = player_full_name,
         empirical_prob_over_2023 = emp_prob_2023)

player_emp_probs_2024 <-
  future_pmap(distinct_mark_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

all_player_marks <-
  all_player_marks |>
  mutate(
    implied_prob_over = 1 / over_price
  ) |>
  left_join(player_emp_probs_2023, by = c("player_name", "line")) |>
  left_join(player_emp_probs_2024, by = c("player_name", "line")) |>
  mutate(
    diff_over_2023 = empirical_prob_over_2023 - implied_prob_over,
    diff_over_last_3 = emp_prob_last_3 - implied_prob_over,
    diff_over_last_5 = emp_prob_last_5 - implied_prob_over,
    diff_over_last_7 = emp_prob_last_7 - implied_prob_over,
    diff_over_last_10 = emp_prob_last_10 - implied_prob_over) |> 
  mutate_if(is.double, round, 2) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line)

# # Add to google sheets
# write_sheet(sheet, data = all_player_marks, sheet = "Player Marks")

# Write as RDS
all_player_marks |> write_rds("Data/processed_odds/all_player_marks.rds")

##%######################################################%##
#                                                          #
####                   Player Tackles                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_tackles <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "tackles") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Add empirical probabilities---------------------------------------------------

# tackles
distinct_tackle_combos <-
  all_player_tackles |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "tackles")

player_emp_probs_2023 <-
  empirical_probabilities_2023 |>
  filter(stat == "tackles") |>
  select(-stat) |> 
  rename(player_name = player_full_name,
         empirical_prob_over_2023 = emp_prob_2023)

player_emp_probs_2024 <-
  future_pmap(distinct_tackle_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

all_player_tackles <-
  all_player_tackles |>
  mutate(
    implied_prob_over = 1 / over_price
  ) |>
  left_join(player_emp_probs_2023, by = c("player_name", "line")) |>
  left_join(player_emp_probs_2024, by = c("player_name", "line")) |>
  mutate(
    diff_over_2023 = empirical_prob_over_2023 - implied_prob_over,
    diff_over_last_3 = emp_prob_last_3 - implied_prob_over,
    diff_over_last_5 = emp_prob_last_5 - implied_prob_over,
    diff_over_last_7 = emp_prob_last_7 - implied_prob_over,
    diff_over_last_10 = emp_prob_last_10 - implied_prob_over) |> 
  mutate_if(is.double, round, 2) |>
  group_by(player_name, line) |>
  mutate(
    min_implied_prob = min(implied_prob_over, na.rm = TRUE),
    max_implied_prob = max(implied_prob_over, na.rm = TRUE)
  ) |>
  mutate(variation = max_implied_prob - min_implied_prob) |>
  ungroup() |>
  select(-min_implied_prob,-max_implied_prob) |>
  arrange(desc(variation), player_name, desc(over_price), line)

# # Add to google sheets
# write_sheet(sheet, data = all_player_tackles, sheet = "Player Tackles")

# Write as RDS
all_player_tackles |> write_rds("Data/processed_odds/all_player_tackles.rds")
