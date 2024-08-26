# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Aussie_Rules/AFL_-_Round_24/Matches"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
topsport_url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
    topsport_url |>
    read_html() |>
    html_nodes("dd") |> 
    html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {

# Function to get head to head data--------------------------------------------#
get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[2, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[3, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
}

# Map function to main markets list
topsport_h2h <- map(main_markets, get_h2h) |> bind_rows()

# Fix names
topsport_h2h <-
topsport_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time)) |>
    filter(!is.na(home_win))

# Write to csv
write_csv(topsport_h2h, "Data/scraped_odds/topsport_h2h.csv")
}

##%######################################################%##
#                                                          #
####                    Run function                    ####
#                                                          #
##%######################################################%##

h2h_safe_topsport <- safely(head_to_head_main)

# Run functions
h2h_safe_topsport()

props_function <- function() {

#===============================================================================
# Player Disposals
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Aussie_Rules/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
    url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player disposals--------------------------------------

# Get URLs
pick_your_own_disposals_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Player_to_Have_[0-9]{1,2}_Disposals")]

# Get Unique URLs
pick_your_own_disposals_markets <- unique(pick_your_own_disposals_markets)

# Map function
player_disposals_alternate <-
map(pick_your_own_disposals_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = as.numeric(line) - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(
        player_name =
            case_when(
                player_name == "Matthew Roberts" ~ "Matt Roberts",
                player_name == "Samuel Wicks" ~ "Sam Wicks",
                player_name == "Harrison Himmelberg" ~ "Harry Himmelberg",
                player_name == "Jordan de Goey" ~ "Jordan De Goey",
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

# Get data for player disposals over/under-----------------------------------------

# Get URLs
player_disposals_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Total_Disposals_.*\\(")]

# Only proceed if markets have been picked up above
if (length(player_disposals_markets) > 0) {

# Map function
player_disposals_lines <-
    map(player_disposals_markets, read_topsport_html) |> 
    bind_rows() |> 
    mutate(line = as.numeric(line)) |>
    rename(over_price = Win)

# Get Overs
player_disposals_lines_overs <-
    player_disposals_lines |> 
    filter(str_detect(Selection, "Over")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Over ") |> 
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(line = as.numeric(line)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
        player_name =
          case_when(
              player_name == "Matthew Roberts" ~ "Matt Roberts",
              player_name == "Samuel Wicks" ~ "Sam Wicks",
              player_name == "Harrison Himmelberg" ~ "Harry Himmelberg",
              player_name == "Jordan de Goey" ~ "Jordan De Goey",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

# Get Unders
player_disposals_lines_unders <-
    player_disposals_lines |> 
    filter(str_detect(Selection, "Under")) |>
    separate(Selection, into = c("player_name", "line"), sep = " Under ") |> 
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(line = as.numeric(line)) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    rename(under_price = over_price) |>
    mutate(
        player_name =
          case_when(
              player_name == "Harrison Himmelberg" ~ "Harry Himmelberg",
              player_name == "Jordan de Goey" ~ "Jordan De Goey",
            .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

# Combine
player_disposals_lines <- 
    player_disposals_lines_overs |> 
    left_join(player_disposals_lines_unders) |> 
    mutate(market_name = "Player Disposals") |> 
    mutate(agency = "TopSport") }

#===============================================================================
# Player Goals
#===============================================================================

# Function to read the html of a given url
read_topsport_html_goals <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Aussie_Rules/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    market_name <- str_replace(market_name, "Anytime", "1")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
        url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player goals--------------------------------------

# Get URLs
pick_your_own_goals_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "(To_Score_[0-9]{1,2}_Goals)|(Anytime_Goal_Scorer)")]

# Get Unique URLs
pick_your_own_goals_markets <- unique(pick_your_own_goals_markets)

# Remove Quarter Markets
pick_your_own_goals_markets <- 
    pick_your_own_goals_markets[str_detect(pick_your_own_goals_markets, "(Quarter)|(Half)", negate = TRUE)]

# Map function
player_goals_alternate <-
    map(pick_your_own_goals_markets, read_topsport_html_goals) |> 
    bind_rows() |> 
    mutate(line = as.numeric(line) - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
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
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))

#===============================================================================
# Player Fantasy Points
#===============================================================================

# Function to read the html of a given url
read_topsport_html_fantasy_points <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Aussie_Rules/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    market_name <- str_replace(market_name, "Anytime", "1")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
        url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player fantasy_points--------------------------------------

# Get URLs
pick_your_own_fantasy_points_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Fantasy")]

# Get Unique URLs
pick_your_own_fantasy_points_markets <- unique(pick_your_own_fantasy_points_markets)

# Only proceed if markets have been picked up above
if (length(pick_your_own_fantasy_points_markets) > 0) {

# Map function
player_fantasy_points_alternate <-
    map(pick_your_own_fantasy_points_markets, read_topsport_html_fantasy_points) |> 
    map(mutate, Win = as.numeric(Win)) |>
    bind_rows() |> 
    filter(!is.na(Win)) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(
        player_name =
            case_when(
                player_name == "Matthew Roberts" ~ "Matt Roberts",
                player_name == "Samuel Wicks" ~ "Sam Wicks",
                player_name == "Jordan de Goey" ~ "Jordan De Goey",
                player_name == "Jacob Van Rooyen" ~  "Jacob van Rooyen",
                player_name == "Matthew Rowell" ~ "Matt Rowell",
                player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
                player_name == "Cameron Rayner" ~ "Cam Rayner",
                player_name == "Callum M Brown" ~ "Callum M. Brown",
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}

#===============================================================================
# Player Marks
#===============================================================================

# Function to read the html of a given url
read_topsport_html_marks <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Aussie_Rules/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    market_name <- str_replace(market_name, "Anytime", "1")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
        url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player marks--------------------------------------

# Get URLs
pick_your_own_marks_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Marks")]

# Get Unique URLs
pick_your_own_marks_markets <- unique(pick_your_own_marks_markets)

# Only proceed if markets have been picked up above
if (length(pick_your_own_marks_markets) > 0) {

# Map function
player_marks_alternate <-
    map(pick_your_own_marks_markets, read_topsport_html_marks) |> 
    map(mutate, Win = as.numeric(Win)) |>
    bind_rows() |> 
    filter(!is.na(Win)) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(
        player_name =
            case_when(
                player_name == "Matthew Roberts" ~ "Matt Roberts",
                player_name == "Samuel Wicks" ~ "Sam Wicks",
                player_name == "Jacob Van Rooyen" ~  "Jacob van Rooyen",
                player_name == "Matthew Rowell" ~ "Matt Rowell",
                player_name == "Jordan de Goey" ~ "Jordan De Goey",
                player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
                player_name == "Cameron Rayner" ~ "Cam Rayner",
                player_name == "Callum M Brown" ~ "Callum M. Brown",
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}
#===============================================================================
# Player Tackles
#===============================================================================

# Function to read the html of a given url
read_topsport_html_tackles <- function(url) {
    
    # Get market name from url
    market_name <- str_extract(url, "(?<=Aussie_Rules/).*")
    market_name <- str_remove(market_name, "\\/.*$")
    
    market_name <- str_replace(market_name, "Anytime", "1")
    
    # Get line from market name
    line <- str_extract(market_name, "\\d+")
    
    # Get match name from html
    match_name_html <-    
        url |> 
        read_html() |>
        html_nodes("h1") |>
        html_text() |> 
        paste(collapse = " ")
    
    match_name_html <- strsplit(match_name_html, split = " - ")
    match_name <- match_name_html[[1]][length(match_name_html[[1]])]
    player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
    
    # Get data from html
    result <-    
        url |> 
        read_html() |>
        html_nodes(".marketTable") |> 
        html_table()
    
    # Get tibble
    result[[1]] |>
        mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
        mutate(match = match_name) |>
        mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player tackles--------------------------------------

# Get URLs
pick_your_own_tackles_markets <- 
    topsport_other_markets[str_detect(topsport_other_markets, "Tackles")]

# Get Unique URLs
pick_your_own_tackles_markets <- unique(pick_your_own_tackles_markets)

# Only proceed if markets have been picked up above
if (length(pick_your_own_tackles_markets) > 0) {

# Map function
player_tackles_alternate <-
    map(pick_your_own_tackles_markets, read_topsport_html_tackles) |> 
    map(mutate, Win = as.numeric(Win)) |>
    bind_rows() |> 
    filter(!is.na(Win)) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(over_price = Win) |> 
    rename(player_name = Selection) |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
    mutate(
        player_name =
            case_when(
                player_name == "Matthew Roberts" ~ "Matt Roberts",
                player_name == "Samuel Wicks" ~ "Sam Wicks",
                player_name == "Jordan de Goey" ~ "Jordan De Goey",
                player_name == "Jacob Van Rooyen" ~  "Jacob van Rooyen",
                player_name == "Matthew Rowell" ~ "Matt Rowell",
                player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
                player_name == "Cameron Rayner" ~ "Cam Rayner",
                player_name == "Callum M Brown" ~ "Callum M. Brown",
                .default = player_name)) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    relocate(match, .before = player_name) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(player_team == home_team, away_team, home_team))
}

#===============================================================================
# Write to CSV
#===============================================================================

if (!exists("player_disposals_lines")) {
    player_disposals_lines <- tibble() } 

# Disposals
player_disposals_alternate |>
    bind_rows(player_disposals_lines) |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Disposals") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_disposals.csv")

# Goals
player_goals_alternate |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Goals") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_goals.csv")

# Tackles

if (exists("player_tackles_alternate")) {

player_tackles_alternate |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Tackles") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_tackles.csv")}

# Marks

if (exists("player_marks_alternate")) {

player_marks_alternate |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Marks") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_marks.csv")}

# Fantasy Points

if (exists("player_fantasy_points_alternate")) {
    
player_fantasy_points_alternate |>
    mutate(match = paste(home_team, away_team, sep = " v ")) |>
    select(any_of(
        c(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team"))) |>
    mutate(market_name = "Player Fantasy Points") |>
    mutate(agency = "TopSport") |> 
    write_csv("Data/scraped_odds/topsport_player_fantasy_points.csv")}
}

# Make Safe Version
safe_props <- safely(props_function)
safe_props()
