# Get functions to determine player correlations based on historical data

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(tidytable)
library(stringr)
library(fitzRoy)
library(infotheo)
`%notin%` <- Negate(`%in%`)

source("Functions/data_processing_functions.R")

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

#===============================================================================
# Get historical data
#===============================================================================

# 2014 - 2022
afl_fantasy_2014_2022_data <- readRDS("C:/Users/a1645197/AFL-Fantasy/Data/afl_fantasy_2014_2022_data.rds")

# 2023
afl_fantasy_2023_data <- get_fantasy_data(season = 2023)

# Combine
player_stats <- bind_rows(afl_fantasy_2014_2022_data, afl_fantasy_2023_data)

#===============================================================================
# Create functions to get correlation tables
#===============================================================================

# Fantasy Points----------------------------------------------------------------
get_player_correlations <-
  function(player_a, player_b, seasons = c("2023")) {
    # Player A data
    player_a_data <-
      player_stats |>
      filter(season_name %in% seasons) |>
      filter(player_full_name == player_a) |>
      filter(tog_percentage >= 60) |>
      select(
        player_a_name = player_full_name,
        match_name,
        season_name,
        round,
        player_team,
        player_a_fantasy_points = fantasy_points
      )
    
    # Player B data
    player_b_data <-
      player_stats |>
      filter(season_name %in% seasons) |>
      filter(player_full_name == player_b) |>
      filter(tog_percentage >= 60) |>
      select(
        player_b_name = player_full_name,
        match_name,
        season_name,
        round,
        player_b_fantasy_points = fantasy_points
      )
    
    # Combine
    combined_data <-
      inner_join(player_a_data, player_b_data)
    
    # Get correlation between two players
    player_corr_test <- tryCatch({
      cor.test(x = combined_data$player_a_fantasy_points,
               y = combined_data$player_b_fantasy_points)
    }, error = function(e) {
      return(list(estimate = NA, conf.int = c(NA, NA), p.value = NA))
    })
    
    tibble(player_a,
           player_b,
           games = nrow(combined_data),
           corr = player_corr_test$estimate,
           CI_lower = player_corr_test$conf.int[1],
           CI_upper = player_corr_test$conf.int[2],
           P = player_corr_test$p.value)
  }

# Disposals---------------------------------------------------------------------
get_player_correlations_disposals <-
  function(player_a, player_b, seasons = c("2023")) {
    # Player A data
    player_a_data <-
      player_stats |>
      filter(season_name %in% seasons) |>
      filter(player_full_name == player_a) |>
      filter(tog_percentage >= 60) |>
      select(
        player_a_name = player_full_name,
        match_name,
        season_name,
        round,
        player_team,
        player_a_disposals = disposals
      )
    
    # Player B data
    player_b_data <-
      player_stats |>
      filter(season_name %in% seasons) |>
      filter(player_full_name == player_b) |>
      filter(tog_percentage >= 60) |>
      select(
        player_b_name = player_full_name,
        match_name,
        season_name,
        round,
        player_b_disposals = disposals
      )
    
    # Combine
    combined_data <-
      inner_join(player_a_data, player_b_data)
    
    # Get correlation between two players
    player_corr_test <- tryCatch({
      cor.test(x = combined_data$player_a_disposals,
               y = combined_data$player_b_disposals)
    }, error = function(e) {
      return(list(estimate = NA, conf.int = c(NA, NA), p.value = NA))
    })
    
    tibble(player_a,
           player_b,
           games = nrow(combined_data),
           corr = player_corr_test$estimate,
           CI_lower = player_corr_test$conf.int[1],
           CI_upper = player_corr_test$conf.int[2],
           P = player_corr_test$p.value)
  }

#===============================================================================
# Apply function to player combinations - Fantasy Points
#===============================================================================

# Player combinations for this year---------------------------------------------
player_combinations <-
afl_fantasy_2023_data |>
  distinct(player_full_name, player_team) |>
  cross_join(afl_fantasy_2023_data |>
               distinct(player_full_name, player_team)) |> 
  filter(player_team.x == player_team.y) |> 
  filter(player_full_name.x != player_full_name.y) |> 
  mutate(id = paste0(player_full_name.x, player_full_name.y)) |> 
  mutate(id = str_remove_all(id, " ")) |>   
  mutate(id = str_to_lower(id)) |> 
  mutate(id = str_arrange(id)) |> 
  distinct(id, .keep_all = TRUE) |> 
  select(player_a = player_full_name.x, player_b = player_full_name.y)

# map over all combinations
all_correlations <-
  map2_df(
    player_combinations$player_a,
    player_combinations$player_b,
    get_player_correlations
  )

# Remove instances of too few games
player_correlation_output <-
all_correlations |> 
  filter(games >= 10) |> 
  arrange(desc(corr))

# Player combinations for 2021-2023---------------------------------------------
# map over all combinations
all_correlations_all <-
  map2_df(
    player_combinations$player_a,
    player_combinations$player_b,
    get_player_correlations,
    seasons = c("2021", "2022", "2023")
  )

# Remove instances of too few games
player_correlation_output_all <-
all_correlations_all |> 
  filter(games >= 25) |> 
  arrange(desc(corr))

# Write out to data folder------------------------------------------------------
write_rds(player_correlation_output, "Data/player_correlations_fantasy_23.rds")
write_rds(player_correlation_output_all, "Data/player_correlations_fantasy_all.rds")

#===============================================================================
# Apply function to player combinations - Disposals
#===============================================================================

# map over all combinations
all_correlations_disposals <-
  map2_df(
    player_combinations$player_a,
    player_combinations$player_b,
    get_player_correlations_disposals
  )

# Remove instances of too few games
player_correlation_output_disposals <-
all_correlations_disposals |> 
  filter(games >= 10) |> 
  arrange(desc(corr))

# Player combinations for 2021-2023---------------------------------------------
# map over all combinations
all_correlations_all_disposals <-
  map2_df(
    player_combinations$player_a,
    player_combinations$player_b,
    get_player_correlations_disposals,
    seasons = c("2021", "2022", "2023")
  )

# Remove instances of too few games
player_correlation_output_all_disposals <-
all_correlations_all_disposals |> 
  filter(games >= 25) |> 
  arrange(desc(corr))

# Write out to data folder------------------------------------------------------
write_rds(player_correlation_output_disposals, "Data/player_correlations_disposals_23.rds")
write_rds(player_correlation_output_all_disposals, "Data/player_correlations_disposals_all.rds")
