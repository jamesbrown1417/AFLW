# Libraries and functions
library(tidyverse)
library(jsonlite)
library(httr)

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

# Wrap everything in try catch:
tryCatch(
  {
    # Get URL to make requests from
    game_id_url = "https://www.unibet.com.au/sportsbook-feeds/views/filter/australian_rules/all/matches?includeParticipants=true&useCombined=true&ncid=1709729109"
    
    # Make request
    game_id_response = GET(game_id_url)
    
    # Convert response to JSON
    game_id_json = fromJSON(content(game_id_response, "text"))
    
    # Get event information and IDs
    event_info <-
      game_id_json$layout$sections$widgets[[2]]$matches$highlightedEvents$event |> 
      tibble()
    
    event_info <- event_info[[1]][[1]] |> tibble()
    event_info <- event_info$event
    
    # Get event IDs and names
    event_info <-
      event_info |>
      select(event_id = id, match = name) |>
      mutate(match = str_replace(match, " - ", " v "))
    
    # Add all markets url
    event_info <-
      event_info |>
      mutate(all_markets_url =
               paste0("https://oc-offering-api.kambicdn.com/offering/v2018/ubau/betoffer/event/" ,
                      event_id,
                      ".json?lang=en_AU&market=AU&ncid=1709729109"))
    
    # Create a function that takes the url and returns a json of the data
    get_unibet <- function(match, url) {
      response = GET(url)
      json = fromJSON(content(response, "text"))
      output <- tibble(json)
      output <- output$json[[1]] |> tibble()
      output <- unnest(output, cols = c("criterion") , names_repair = "universal")
      output <- unnest(output, cols = c("outcomes") , names_repair = "universal")
      output <- unnest(output, cols = c("tags") , names_repair = "universal")
      output$match <- match
      
      return(output)
    }
    
    #===================================================================================================
    # Get all market data
    #===================================================================================================
    
    all_markets <-
      event_info |>
      select(match, url = all_markets_url) |>
      pmap(get_unibet) |>
      bind_rows()
    
    #===================================================================================================
    # Head to Head
    #===================================================================================================
    
    # All head to head
    all_head_to_head <-
      all_markets |> 
      filter(label...4 == "Regular Time") |> 
      select(match, date = closed, participant, oddsAmerican) |>
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(date = as_date(ymd_hms(date)))
    
    # All home teams
    home_head_to_head <-
      all_head_to_head |>
      filter(home_team == participant) |>
      select(match, date, home_team = participant, home_win = price)
    
    # All away teams
    away_head_to_head <-
      all_head_to_head |>
      filter(away_team == participant) |>
      select(match, date, away_team = participant, away_win = price)
    
    # Combine
    head_to_head_combined <-
      home_head_to_head |> 
      full_join(away_head_to_head) |> 
      mutate(home_team = fix_team_names(home_team),
             away_team = fix_team_names(away_team)) |>
      mutate(match = paste(home_team, "v", away_team, sep = " "))
    
    #===================================================================================================
    # Disposals
    #===================================================================================================
    
    # All Player Disposals
    all_player_disposals <-
      all_markets |> 
      filter(str_detect(label...4, "Player to get \\d+ or more Disposals")) |>
      filter(type == "OT_YES") |> 
      mutate(line = str_extract(label...4, "\\d+")) |> 
      mutate(line = as.numeric(line)) |>
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      mutate(line = line - 0.5) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(home_team = fix_team_names(home_team),
             away_team = fix_team_names(away_team)) |>
      mutate(match = paste(home_team, "v", away_team, sep = " ")) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_disposals_overs <- 
      all_player_disposals |>
      filter(type == "OT_YES") |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_disposals_unders <- 
      all_player_disposals |>
      filter(type == "OT_NO") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_disposals_combined <-
      player_disposals_overs |>
      full_join(player_disposals_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      mutate(
        player_name =
          case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Samuel Wicks" ~ "Sam Wicks",
            player_name == "Jacob Van Rooyen" ~  "Jacob van Rooyen",
            player_name == "Matthew Rowell" ~ "Matt Rowell",
            player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
            player_name == "Cameron Rayner" ~ "Cam Rayner",
            player_name == "Callum M Brown" ~ "Callum M. Brown",
            player_name == "Lachlan Ash" ~ "Lachie Ash",
            player_name == "Lachlan Schultz" ~ "Lachie Schultz",
            player_name == "Will Powell" ~ "Wil Powell",
            player_name == "Alex Neal-bullen" ~ "Alex Neal-Bullen",
            .default = player_name)) |>
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Goals
    #===================================================================================================
    
    # All Player Goals
    all_player_goals <-
      all_markets |> 
      filter(str_detect(label...4, "(^Goal Scorer$)|(To score at least)")) |>
      mutate(label...4 = str_replace(label...4, "Goal Scorer", "To score at least 1 Goal")) |>
      mutate(line = str_extract(label...4, "\\d+")) |> 
      filter(criterion$name == "Yes" | is.na(criterion$name)) |>
      mutate(line = as.numeric(line)) |>
      select(match, date = closed, participant, oddsAmerican, line, type) |>
      filter(!is.na(participant)) |>
      mutate(line = line - 0.5) |> 
      mutate(oddsAmerican = as.numeric(oddsAmerican)) |>
      mutate(price = ifelse(oddsAmerican < 0, (100 / abs(oddsAmerican)) + 1, (oddsAmerican / 100) + 1)) |>
      distinct(match, date, participant, price, line, type) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
      mutate(home_team = fix_team_names(home_team),
             away_team = fix_team_names(away_team)) |>
      mutate(match = paste(home_team, "v", away_team, sep = " ")) |> 
      mutate(date = as_date(ymd_hms(date))) |> 
      separate(participant, c("last_name", "first_name"), sep = ", ", remove = TRUE) |> 
      mutate(player_name = paste(first_name, last_name, sep = " ")) |>
      select(-first_name, -last_name)
    
    # Overs
    player_goals_overs <- 
      all_player_goals |>
      select(match, date, line, player_name, over_price = price)
    
    # Unders
    player_goals_unders <- 
      all_player_goals |>
      filter(type == "OT_NO") |>
      select(match, date, line, player_name, under_price = price)
    
    # Combine
    player_goals_combined <-
      player_goals_overs |>
      full_join(player_goals_unders) |>
      mutate(agency = "Unibet") |>
      arrange(match, player_name, line) |>
      mutate(over_price = round(over_price, 2),
             under_price = round(under_price, 2)) |> 
      mutate(
        player_name =
          case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Samuel Wicks" ~ "Sam Wicks",
            player_name == "Jacob Van Rooyen" ~  "Jacob van Rooyen",
            player_name == "Matthew Rowell" ~ "Matt Rowell",
            player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
            player_name == "Cameron Rayner" ~ "Cam Rayner",
            player_name == "Callum M Brown" ~ "Callum M. Brown",
            player_name == "Lachlan Ash" ~ "Lachie Ash",
            player_name == "Lachlan Schultz" ~ "Lachie Schultz",
            player_name == "Will Powell" ~ "Wil Powell",
            player_name == "Alex Neal-bullen" ~ "Alex Neal-Bullen",
            .default = player_name)) |>
      left_join(player_names[,c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
      rename(player_team = team_name) |> 
      separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
      relocate(player_team, opposition_team, .after = player_name)
    
    #===================================================================================================
    # Write out data
    #===================================================================================================
    
    # Disposals
    player_disposals_combined |> 
      mutate(market_name = "Player Disposals") |> 
      write_csv("Data/scraped_odds/unibet_player_disposals.csv")
    
    # Goals
    player_goals_combined |> 
      mutate(market_name = "Player Goals") |> 
      write_csv("Data/scraped_odds/unibet_player_goals.csv")
    
  },
  error = function(e) {
    message("Error in scraping: ", e)
  })
