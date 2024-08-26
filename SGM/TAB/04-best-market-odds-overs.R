#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)

#===============================================================================
# Read in data
#===============================================================================

# Get Data
source("SGM/TAB/tab_sgm.R")
player_disposals_data <- read_rds("Data/processed_odds/all_player_disposals.rds")
player_goals_data <- read_rds("Data/processed_odds/all_player_goals.rds")

# Get those markets where tab has the best odds in the market
tab_best <-
  player_disposals_data |>
  bind_rows(player_goals_data) |>
  arrange(player_name, market_name, line, desc(over_price)) |>
  group_by(player_name, market_name, line) |>
  slice_head(n = 1) |>
  ungroup() |>
  filter(agency == "TAB") |>
  transmute(match,
            player_name,
            player_team,
            market_name,
            line,
            price = over_price,
            type = "Overs",
            diff_over_2023,
            diff_over_last_10) |> 
  select(-price)
# filter(market_name == "Player Fantasy Points")

#===============================================================================
# Get all 2 way combinations
#===============================================================================

# All bets
tab_sgm_bets <-
  tab_sgm |> 
  select(match, player_name, player_team, market_name, line, price, contains("id")) |> 
  left_join(tab_best) |>
  filter(!is.na(diff_over_last_10))

# Filter to only Overs
tab_sgm_bets <-
  tab_sgm_bets |> 
  filter(type == "Overs") |> 
  distinct(match, player_name, market_name, line, .keep_all = TRUE) |> 
  filter(!is.na(player_name)) |> 
  filter(diff_over_last_10 > 0.05)

# Generate all combinations of two rows
row_combinations <- combn(nrow(tab_sgm_bets), 2)

# Get list of tibbles of the two selected rows
list_of_dataframes <-
  map(
    .x = seq_len(ncol(row_combinations)),
    .f = function(i) {
      tab_sgm_bets[row_combinations[, i], ] |> 
        mutate(combination = i)
    }
  )

# Keep only those where the match is the same
retained_combinations <-
  list_of_dataframes |> 
  # Keep only dataframes where first and second row match are equal
  keep(~.x$match[1] == .x$match[2]) |> 
  keep(~prod(.x$price) >= 1.5 & prod(.x$price) <= 3)

#===============================================================================
# Call function
#===============================================================================

# Custom function to apply call_sgm_tab to a tibble
apply_sgm_function <- function(tibble) {
  
  # Random Pause between 0.5 and 0.7 seconds
  Sys.sleep(runif(1, 0.5, 0.8))
  
  # Call function
  call_sgm_tab(
    data = tab_sgm,
    player_names = tibble$player_name,
    stat_counts = tibble$line,
    markets = tibble$market_name
  )
}

# Make safe function
apply_sgm_function_safe <- safely(apply_sgm_function, otherwise = NA)

# Applying the function to each tibble in the list 
results <- map(retained_combinations, apply_sgm_function_safe, .progress = TRUE)

# Bind all results together
results_table <-
  results |>
  # keep only first part of list
  map(1) |>
  keep(~is.data.frame(.x)) |>
  bind_rows() |> 
  arrange(desc(Adjustment_Factor)) |> 
  mutate(Diff = 1/Unadjusted_Price - 1/Adjusted_Price) |> 
  mutate(Diff = round(Diff, 2)) |>
  arrange(desc(Diff))

