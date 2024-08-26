# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(glue)

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

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
