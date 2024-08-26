library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)


# Bet365 SGM-----------------------------------------------------------------
bet365_sgm <-
  read_csv("../../Data/scraped_odds/bet365_player_disposals.csv") |> 
  bind_rows(read_csv("../../Data/scraped_odds/bet365_player_goals.csv")) |> 
  rename(price = over_price) |>
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
  select(-contains("under"))


#===============================================================================
# Function to get SGM Price
#===============================================================================

call_sgm_bet365 <- function(data, player_names, stat_counts, markets) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             line == stat_counts[i],
             market_name == markets[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  if (nrow(filtered_df) != length(player_names)) {
    return(NULL)
  }
  
  unadjusted_price <- prod(filtered_df$price)
  
  adjusted_price = 1/(0.004 + (1/unadjusted_price)) |> round(2)
  
  adjustment_factor <- adjusted_price / unadjusted_price
  
  combined_list <- paste(player_names, stat_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  market_string <- paste(markets, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Markets = market_string,
    Unadjusted_Price = round(unadjusted_price, 2),
    Adjusted_Price = round(adjusted_price, 2),
    Adjustment_Factor = adjustment_factor,
    Agency = 'Bet365'
  )
  
  return(output_data)
  
}

# call_sgm_bet365(
#   data = bet365_sgm,
#   player_names = c("Charlie Curnow", "Blake Acres"),
#   stat_counts = c(2.5, 19.5),
#   markets = c("Player Goals", "Player Disposals")
# )
