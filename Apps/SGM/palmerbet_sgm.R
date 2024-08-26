library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(mongolite)
uri <- Sys.getenv("mongodb_connection_string")

# Palmerbet SGM-----------------------------------------------------------------
palmersgm_con <- mongo(collection = "Palmerbet-SGM", db = "Odds", url = uri)
palmerbet_sgm <- palmersgm_con$find('{}') |> tibble() |> mutate(number_of_disposals = str_extract(market, "[0-9]{1,2}")) |> mutate(number_of_disposals = paste0(number_of_disposals, "+"))

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
get_sgm_palmerbet <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player == player_names[[i]] &
               number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  market_ids <- unique(as.character(filtered_df$market_id))
  
  selected_markets <- lapply(market_ids, function(market_id) {
    market_data <- filtered_df %>% 
      filter(market_id == !!market_id)
    
    id_list <- as.character(market_data$outcome_id)
    price_list <- as.numeric(market_data$price)
    market_name <- as.character(market_data$market[1])  # assuming the market name is consistent for all outcomes within a market
    
    outcomes <- lapply(1:length(id_list), function(i) 
      list(price = unbox(price_list[i]), title = unbox(id_list[i]))
    )
    
    list(
      id = unbox(market_id),
      name = unbox(market_name),
      outcomes = outcomes
    )
  })
  
  selected_legs <- lapply(1:length(filtered_df$outcome_id), function(i) 
    list(marketId = unbox(filtered_df$market_id[i]), title = unbox(filtered_df$outcome_id[i]))
  )
  
  payload <- list(
    markets = selected_markets,
    legs = selected_legs
  )
  
  return(payload)
}


#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_palmerbet <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player == player_names[i],
             number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  unadjusted_price <- prod(filtered_df$price)
  
  payload <- get_sgm_palmerbet(data, player_names, disposal_counts)
  
  url <- 'https://pricing.palmerbet.online:9698/AustralianRules/price?channel=website'
  
  headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
               'Content-Type' = 'application/json;charset=UTF-8',
               'Origin' = 'https://www.palmerbet.com/',
               'Referer' = 'https://www.palmerbet.com/')
  
  # Error handling for the POST request
  tryCatch({
    response <- POST(url, body = toJSON(payload), add_headers(.headers = headers), encode = "json")
  }, error = function(e) {
    message("Error in POST request: ", e)
    return(NULL)
  })
  
  # If there is no response, return NULL
  if (is.null(response)) {
    return(NULL)
  }
  
  response_content <- content(response, "parsed")
  
  # Assuming the structure of response is similar to the pointsbet one 
  adjusted_price <- as.numeric(response_content$price)
  adjustment_factor <- adjusted_price / unadjusted_price
  combined_list <- paste(player_names, disposal_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Palmerbet'
  )
  
  return(output_data)
}
