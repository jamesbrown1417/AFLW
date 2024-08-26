# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(glue)

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {

# Read scraped HTML from the BET365_HTML Folder
scraped_file <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "h2h")[[1]]

# Get Teams
bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantFixtureDetailsHigher_TeamWrapper ") |> 
    html_text()

# Get H2H Odds
bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantOddsOnly50_Odds") |> 
    html_text()

# Get Handicap
bet365_handicap <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Handicap") |> 
    html_text()

# Get indices of elements that do contain "O" or "U"
totals_indices <- which(str_detect(bet365_handicap, "O|U"))

# Get Handicap Price
bet365_handicap_price <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Odds") |> 
    html_text()

# Remove empty strings
bet365_handicap_price <- bet365_handicap_price[!bet365_handicap_price == ""]

# Remove Totals Indices
if (length(totals_indices) > 0) {
  bet365_handicap <- bet365_handicap[-totals_indices]
  bet365_handicap_price <- bet365_handicap_price[-totals_indices]
}

# Get Start Time
bet365_start_time <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-MarketFixtureDetailsLabel") |>
    html_nodes(".rcl-MarketHeaderLabel-isdate, .src-ParticipantFixtureDetailsHigher_BookCloses ") |> 
    html_text()

# Get indices that contain a number surrounded by spaces
start_time_indices <- which(str_detect(bet365_start_time, " \\d+ "))

# Empty list
result_list <- list()

# Split into chunks from each index to the next and from the last to the end of the vector
for (i in 1:length(start_time_indices)) {
    if (i != length(start_time_indices)) {
        result = (bet365_start_time[start_time_indices[i]:start_time_indices[i + 1]])
        result = result[-length(result)]
    } else {
        result = (bet365_start_time[start_time_indices[i]:length(bet365_start_time)])
    }
    result_list[[i]] <- result
}

# Turn each vector into a tibble
start_dates <- map(result_list, ~ expand_grid(.x[1], .x[2:length(.x)])) |> bind_rows()
names(start_dates) <- c("start_date", "start_time")

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

home_h2h <- tibble(home_teams, home_odds) |>
    bind_cols(start_dates)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

away_h2h <- tibble(away_teams, away_odds) |>
    bind_cols(start_dates)

# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_date = `start_date...3`,
              start_time = `start_time...4`, 
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(start_time = dmy_hm(paste(start_date, "2023", start_time))) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365") |> 
    select(-start_date)

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")

#===============================================================================
# Create Handicap table--------------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_handicap <- bet365_handicap[seq(1, length(bet365_handicap), 2)]
home_handicap_price <- bet365_handicap_price[seq(1, length(bet365_handicap_price), 2)]

home_handicap <- tibble(home_teams, home_handicap, home_handicap_price) |>
    bind_cols(start_dates)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_handicap <- bet365_handicap[seq(2, length(bet365_handicap), 2)]
away_handicap_price <- bet365_handicap_price[seq(2, length(bet365_handicap_price), 2)]

away_handicap <- tibble(away_teams, away_handicap, away_handicap_price) |>
    bind_cols(start_dates)

# Combine together into one table
bet365_handicap <-
    bind_cols(home_handicap, away_handicap) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_date = `start_date...4`,
              start_time = `start_time...5`, 
              market_name = "Line",
              home_team = home_teams,
              home_line = as.numeric(home_handicap),
              home_win = as.numeric(home_handicap_price),
              away_team = away_teams,
              away_line = as.numeric(away_handicap),
              away_win = as.numeric(away_handicap_price)) |>
    mutate(start_time = dmy_hm(paste(start_date, "2023", start_time))) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365") |> 
    select(-start_date)

# Write to csv
write_csv(bet365_handicap, "Data/scraped_odds/bet365_line.csv")
}

# Create safe version of functions-----------------------------------------------
get_head_to_head_safe <- safely(get_head_to_head, otherwise = NULL)

# # Run functions-----------------------------------------------------------------
tryCatch(get_head_to_head(), error = function(e) print("Error in get_head_to_head()"))

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

# Function to read in disposals html and output table
read_bet365_disposals_html <- function(html_path) {
    
    # Read in the txt data as html
    bet365 <- read_html(html_path)
    
    # Get match name
    bet365_match_name <-
        bet365 |>
        html_nodes(".sph-FixturePodHeader_TeamName ") |>
        html_text()
    
    bet365_match_name <- glue_collapse(bet365_match_name,sep = " v ")
    
    # Extract the disposals data table----------------------------------------------
    # Get the disposals table
    bet365_disposals <-
        bet365 |>
        html_nodes(".bbl-FilteredMarketGroupWithHScrollerContainer_Wide")
    
    # Player names
    bet365_disposals_player_names <-
        bet365_disposals[[2]] |>
        html_nodes(".bbl-BetBuilderParticipantLabel") |>
        html_text()
    
    # Odds
    bet365_disposals_odds <-
        bet365_disposals[[2]] |>
        html_nodes(".bbl-BetBuilderParticipant") |>
        html_text()
    
    # Get indices for each player
    bet365_indices <- seq_along(bet365_disposals_player_names)
    
    # Get 10+ disposals odds
    bet365_disposals_odds_10plus <-
        bet365_disposals_odds[bet365_indices]
    
    # Get 15+ disposals odds
    bet365_disposals_odds_15plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names)]
    
    # Get 20+ disposals odds
    bet365_disposals_odds_20plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names) * 2]
    
    # Get 25+ disposals odds
    bet365_disposals_odds_25plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names) * 3]
    
    # Get 30+ disposals odds
    bet365_disposals_odds_30plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names) * 4]
    
    # Get 35+ disposals odds
    bet365_disposals_odds_35plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names) * 5]
    
    # Get 40+ disposals odds
    bet365_disposals_odds_40plus <-
        bet365_disposals_odds[bet365_indices + length(bet365_disposals_player_names) * 6]
    
    # Create data frame
    bet365_disposals_odds_df <-
        tibble(
            player = bet365_disposals_player_names,
            odds_10plus = bet365_disposals_odds_10plus,
            odds_15plus = bet365_disposals_odds_15plus,
            odds_20plus = bet365_disposals_odds_20plus,
            odds_25plus = bet365_disposals_odds_25plus,
            odds_30plus = bet365_disposals_odds_30plus,
            odds_35plus = bet365_disposals_odds_35plus,
            odds_40plus = bet365_disposals_odds_40plus
        )
    
    # Pivot longer
    bet365_disposals_odds_df <-
        bet365_disposals_odds_df |>
        pivot_longer(
            cols = odds_10plus:odds_40plus,
            names_to = "disposals",
            values_to = "odds"
        ) |>
        mutate(
            disposals = str_remove(disposals, "odds_"),
            disposals = str_replace(disposals, "plus", "+"),
            odds = as.numeric(odds)
        ) |>
        filter(!is.na(odds)) |>
        mutate(match = bet365_match_name) |>
        select(
            match,
            player_name = player,
            number_of_disposals = disposals,
            price = odds
        ) |>
        mutate(implied_probability = 1 / price)
    
    # Return table
    bet365_disposals_odds_df
}

# Function to read in goals html and output table
read_bet365_goals_html <- function(html_path) {
    
    # Read in the txt data as html
    bet365 <- read_html(html_path)
    
    # Get match name
    bet365_match_name <-
        bet365 |>
        html_nodes(".sph-FixturePodHeader_TeamName ") |>
        html_text()
    
    bet365_match_name <- glue_collapse(bet365_match_name,sep = " v ")
    
    # Extract the goals data table----------------------------------------------
    # Get the goals table
    bet365_goals <-
        bet365 |>
        html_nodes(".bbl-FilteredMarketGroupWithHScrollerContainer_Wide")
    
    # Player names
    bet365_goals_player_names <-
        bet365_goals[[1]] |>
        html_nodes(".bbl-BetBuilderParticipantLabel") |>
        html_text()
    
    # Determine if multi scorer is selected
    selected <-
        bet365 |>
        html_nodes(".bbl-TabSwitcherItem-selected") |>
        html_text()
    
    multi_scorer <- "Multi Scorer" %in% selected
    
    # Odds
    bet365_goal_odds <-
        bet365_goals[[1]] |>
        html_nodes(".bbl-BetBuilderParticipant") |>
        html_text()
    
    # Get indices for each player
    bet365_indices <- seq_along(bet365_goals_player_names)
    
    # Get 1+ goals odds
    bet365_goal_odds_1plus <-
        bet365_goal_odds[bet365_indices]
    
    # Get 2+ goals odds
    bet365_goal_odds_2plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names)]
    
    # Get 3+ goals odds
    bet365_goal_odds_3plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 2]
    
    # Get 4+ goals odds
    bet365_goal_odds_4plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 3]
    
    # Get 5+ goals odds
    bet365_goal_odds_5plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 4]
    
    # Get 6+ goals odds
    bet365_goal_odds_6plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 5]
    
    # Create data frame
    bet365_goal_odds_df <-
        tibble(
            player = bet365_goals_player_names,
            odds_1plus = bet365_goal_odds_1plus,
            odds_2plus = bet365_goal_odds_2plus,
            odds_3plus = bet365_goal_odds_3plus,
            odds_4plus = bet365_goal_odds_4plus,
            odds_5plus = bet365_goal_odds_5plus,
            odds_6plus = bet365_goal_odds_6plus
        )
    
    # Pivot longer
    bet365_goal_odds_df <-
        bet365_goal_odds_df |>
        pivot_longer(
            cols = odds_1plus:odds_6plus,
            names_to = "goals",
            values_to = "odds"
        ) |>
        mutate(
            goals = str_remove(goals, "odds_"),
            goals = str_replace(goals, "plus", "+"),
            odds = as.numeric(odds)
        ) |>
        filter(!is.na(odds)) |>
        mutate(match = bet365_match_name) |>
        select(
            match,
            player_name = player,
            number_of_goals = goals,
            price = odds
        ) |>
        mutate(implied_probability = 1 / price)
    
    # Return table if multi_scorer is selected, else return NULL
    if (multi_scorer) {
        return(bet365_goal_odds_df)
    } else {
        return(NULL)
    }
}

# Function to read disposal lines
read_bet365_disposal_lines_html <- function(html_path) {
  
  # Read in the txt data as html
  bet365 <- read_html(html_path)
  
  # Get match name
  bet365_match_name <-
    bet365 |>
    html_nodes(".sph-FixturePodHeader_TeamName ") |>
    html_text()
  
  bet365_match_name <- glue_collapse(bet365_match_name,sep = " v ")
  
  # Extract the disposals data table----------------------------------------------
  # Get the disposals table
  bet365_disposal_lines <-
    bet365 |>
    html_nodes(".bbl-BetBuilderMarketGroupContainer ")
  
  # Player names
  bet365_disposals_player_names <-
    bet365_disposal_lines[[1]] |>
    html_nodes(".bbl-BetBuilderParticipantLabel") |>
    html_text()
  
  # Odds
  bet365_disposals_odds <-
    bet365_disposal_lines[[1]] |>
    html_nodes(".bbl-BetBuilderParticipant_Odds") |>
    html_text()
  
  # Lines
  bet365_disposals_lines <-
    bet365_disposal_lines[[1]] |>
    html_nodes(".bbl-BetBuilderParticipant_Handicap") |>
    html_text()

  # Create data frame for Overs
  bet365_disposal_lines_odds_df_overs <-
    tibble(
      match = bet365_match_name,
      player_name = bet365_disposals_player_names,
      number_of_disposals = bet365_disposals_lines[1:(length(bet365_disposals_lines) / 2)],
      over_price = bet365_disposals_odds[1:(length(bet365_disposals_odds) / 2)],
    )
  
  # Create data frame for Unders
  bet365_disposal_lines_odds_df_unders <-
    tibble(
      match = bet365_match_name,
      player_name = bet365_disposals_player_names,
      number_of_disposals = bet365_disposals_lines[(length(bet365_disposals_lines) / 2 + 1):length(bet365_disposals_lines)],
      under_price = bet365_disposals_odds[(length(bet365_disposals_odds) / 2 + 1):length(bet365_disposals_odds)],
    )
  
  # Merge the two data frames
  bet365_disposal_lines_odds_df <-
    bet365_disposal_lines_odds_df_overs |>
    left_join(bet365_disposal_lines_odds_df_unders, by = c("match", "player_name", "number_of_disposals"))
  
  # Return table
  bet365_disposal_lines_odds_df
}

#===============================================================================
# Function to read disposal specials
#===============================================================================

read_bet365_disposal_specials_html <- function(html_path) {
  
  # Read in the txt data as html
  bet365 <- read_html(html_path)
  
  # Get match name
  bet365_match_name <-
    bet365 |>
    html_nodes(".sph-FixturePodHeader_TeamName ") |>
    html_text()
  
  bet365_match_name <- glue_collapse(bet365_match_name,sep = " v ")
  
  # Extract the disposals data table----------------------------------------------
  # Get the disposals table
  bet365_disposal_specials <-
    bet365 |>
    html_nodes(".bbl-BetBuilderMarketGroupContainer ")
  
  # Player names
  bet365_disposals_player_names <-
    bet365_disposal_specials[[1]] |>
    html_nodes(".gl-Market_General-columnheader") |> 
    html_nodes(".bbl-MarketColumnHeaderLeftAlign_Label ") |>
    html_text()
  
  # Odds
  bet365_disposals_odds <-
    bet365_disposal_specials[[1]] |>
    html_nodes(".gl-Market_General-columnheader") |> 
    html_nodes(".bbl-BetBuilderParticipant_Odds ") |>
    html_text()
  
  # Lines
  bet365_disposals_lines <-
    bet365_disposal_specials[[1]] |>
    html_nodes(".gl-Market_General-columnheader") |> 
    html_nodes(".bbl-BetBuilderParticipantLabel_Name ") |>
    html_text()
  
  # Get all over prices---------------------------------------------------------
  
  # Calculate the number of complete cycles of 'take 5, skip 5' in the vector
  num_cycles <- length(bet365_disposals_odds) / 10
  
  # Generate an index sequence dynamically based on the length of my_vector
  index <- unlist(lapply(1:ceiling(num_cycles), function(i) {
    start <- (i - 1) * 10 + 1
    end <- start + 4
    seq(start, min(end, length(bet365_disposals_odds)))
  }))
  
  # Subset the original vector with the dynamic index
  bet365_disposals_odds_overs <- bet365_disposals_odds[index]
  
  # Create data frame for Overs
  bet365_disposal_lines_odds_df_overs <-
    tibble(
      match = bet365_match_name,
      player_name = rep(bet365_disposals_player_names, each = 5),
      number_of_disposals = bet365_disposals_lines,
      over_price = bet365_disposals_odds_overs,
    )
  
  # Get all under prices--------------------------------------------------------
  
  bet365_disposals_odds_unders <- bet365_disposals_odds[-index]
  
  # Create data frame for Unders
  bet365_disposal_lines_odds_df_unders <-
    tibble(
      match = bet365_match_name,
      player_name = rep(bet365_disposals_player_names, each = 5),
      number_of_disposals = bet365_disposals_lines,
      under_price = bet365_disposals_odds_unders
    )
  
  # Merge the two data frames
  bet365_disposal_lines_odds_df <-
    bet365_disposal_lines_odds_df_overs |>
    left_join(bet365_disposal_lines_odds_df_unders, by = c("match", "player_name", "number_of_disposals"))
  
  # Return table
  bet365_disposal_lines_odds_df
}
   
# Map Over the Files------------------------------------------------------------
goals_list <- list.files("Data/BET365_HTML", pattern = "players_a", full.names = TRUE)
disposals_list <- list.files("Data/BET365_HTML", pattern = "players", full.names = TRUE)
disposal_specials_list <- list.files("Data/BET365_HTML", pattern = "players_b", full.names = TRUE)

# Create safe versions of each function
read_bet365_goals_html <- safely(read_bet365_goals_html)
read_bet365_disposals_html <- safely(read_bet365_disposals_html)
read_bet365_disposal_lines_html <- safely(read_bet365_disposal_lines_html, otherwise = NULL)
read_bet365_disposal_specials_html <- safely(read_bet365_disposal_specials_html, otherwise = NULL)

# Get all data
bet365_goals <- map(goals_list, read_bet365_goals_html) |> map_dfr(~.x$result)
bet365_disposals <- map(disposals_list, read_bet365_disposals_html) |> map_dfr(~.x$result)
bet365_disposals_lines <- map(disposal_specials_list, read_bet365_disposal_lines_html) |> map_dfr(~.x$result)
bet365_disposal_specials <- map(disposal_specials_list, read_bet365_disposal_specials_html) |> map_dfr(~.x$result)

# If empty give columns
if (nrow(bet365_disposals_lines) == 0) {
  bet365_disposals_lines <- tibble(
    match = character(),
    player_name = character(),
    opposition_team = character(),
    number_of_disposals = character(),
    over_price = numeric(),
    under_price = numeric()
  )
}

if (nrow(bet365_disposal_specials) == 0) {
  bet365_disposal_specials <- tibble(
    match = character(),
    player_name = character(),
    opposition_team = character(),
    number_of_disposals = character(),
    over_price = numeric(),
    under_price = numeric()
  )
}

# Add match info
bet365_goals <-
bet365_goals |> 
    separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
        player_name = case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
            player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
            .default = player_name
        )
    ) |>
    left_join(player_names, by = c("player_name" = "player_full_name")) |>
    mutate(line = as.numeric(str_extract(number_of_goals, "\\d+"))) |>
    mutate(line = line - 0.5) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |> 
    transmute(match,
              home_team,
              away_team,
              player_team,
              opposition_team,
              market_name = "Player Goals",
              player_name,
              line,
              over_price = price,
              agency = "Bet365")

# Add match info - Disposals
bet365_disposals <-
bet365_disposals |> 
    separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
        player_name = case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
            player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
            .default = player_name
        )
    ) |>
    left_join(player_names, by = c("player_name" = "player_full_name")) |>
    mutate(line = as.numeric(str_extract(number_of_disposals, "\\d+"))) |>
    mutate(line = line - 0.5) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |> 
    transmute(match,
              home_team,
              away_team,
              player_team,
              opposition_team,
              market_name = "Player Disposals",
              player_name,
              line,
              over_price = price,
              agency = "Bet365")

# Add match info - Disposal lines
bet365_disposals_lines <-
  bet365_disposals_lines |> 
  separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Matthew Roberts" ~ "Matt Roberts",
      player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
      player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
      .default = player_name
    )
  ) |>
  left_join(player_names, by = c("player_name" = "player_full_name")) |>
  mutate(line = as.numeric(number_of_disposals)) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |> 
  transmute(match,
            home_team,
            away_team,
            player_team,
            opposition_team,
            market_name = "Player Disposals",
            player_name,
            line,
            over_price,
            under_price,
            agency = "Bet365")

# Add match info - Disposal specials
bet365_disposal_specials <-
  bet365_disposal_specials |> 
  separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
      player_name == "Matthew Roberts" ~ "Matt Roberts",
      player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
      player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
      .default = player_name
    )
  ) |>
  left_join(player_names, by = c("player_name" = "player_full_name")) |>
  mutate(line = as.numeric(str_extract(number_of_disposals, "\\d+"))) |> 
  mutate(line = line - 0.5) |>
  rename(player_team = team_name) |> 
  mutate(opposition_team = ifelse(home_team == player_team, away_team, home_team)) |> 
  transmute(match,
            home_team,
            away_team,
            player_team,
            opposition_team,
            market_name = "Player Disposals",
            player_name,
            line,
            over_price,
            under_price,
            agency = "Bet365")

# Fix opposition team if table is null
if (nrow(bet365_disposals_lines) == 0) {
  bet365_disposals_lines <- 
    bet365_disposal_specials |> 
    mutate(opposition_team = "")}

if (nrow(bet365_disposal_specials) == 0) {
  bet365_disposal_specials <- 
    bet365_disposal_specials |> 
    mutate(opposition_team = "")}

bet365_disposal_specials <-
  bet365_disposal_specials |> 
  mutate(over_price = as.numeric(over_price), under_price = as.numeric(under_price))

bet365_disposals_lines <-
  bet365_disposals_lines |> 
  mutate(over_price = as.numeric(over_price), under_price = as.numeric(under_price))

# Combine
bet365_disposals <-
  bet365_disposals |> 
  # bind_rows(bet365_disposals_lines) |> 
  bind_rows(bet365_disposal_specials)

# Write to rds
write_csv(bet365_disposals, "Data/scraped_odds/bet365_player_disposals.csv")
write_csv(bet365_goals, "Data/scraped_odds/bet365_player_goals.csv")
