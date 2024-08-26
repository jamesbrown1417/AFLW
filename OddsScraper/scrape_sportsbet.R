# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/australian-rules/womens-afl"

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
    
# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
}

# Function to get odds
get_odds <- function(match) {
    odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
}

# Function to get start time
get_start_time <- function(match) {
    start_time <-
        match |>
        html_nodes(".oneLine_f15ay66x") |>
        html_text()
    
    # Output
    tibble(start_time)
}

# Map functions to each match and combine together
all_main_market_data <-
bind_cols(
    map(matches, get_team_names) |> bind_rows(),
    map(matches, get_odds) |> bind_rows(),
    map(matches, get_start_time) |> bind_rows()
)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

sportsbet_h2h <-
all_main_market_data |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")

}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {

# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()

    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]

    # Output
    tibble(home_team, away_team)
}


# Get match links
match_links <-
sportsbet_url |>
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |>
    html_attr("href")

# Get match IDs from links
match_ids <-
match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()

# Get data from main market page
matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")

# Get team names that correspond to each match link
team_names <-
    map_dfr(matches, get_team_names) |>
    bind_cols("match_id" = match_ids) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team))

# Match info links
match_info_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/SportCard?displayWinnersPriceMkt=true&includeLiveMarketGroupings=true&includeCollection=true")

# Main Markets
main_market_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/125/Markets")

# Player disposals links
player_disposals_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/601/Markets")

# Player goals links
player_goals_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/148/Markets")

# Get IDs needed for SGM engine-------------------------------------------------
read_prop_url_metadata <- function(url) {

    # Make request and get response
    sb_response <-
        request(url) |>
        req_perform() |>
        resp_body_json()

    # Empty vectors to append to
    class_external_id = c()
    competition_external_id = c()
    event_external_id = c()

    # Append to vectors
    class_external_id = c(class_external_id, sb_response$classExternalId)
    competition_external_id = c(competition_external_id, sb_response$competitionExternalId)
    event_external_id = c(event_external_id, sb_response$externalId)

    # Output
    tibble(class_external_id,
           competition_external_id,
           event_external_id,
           url) |>
        mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
        rename(match_id = url) |>
        mutate(match_id = as.numeric(match_id))
}

# Safe version that just returns NULL if there is an error
safe_read_prop_metadata <- safely(read_prop_url_metadata, otherwise = NULL)

# Map function to player points urls
player_prop_metadata <-
    map(match_info_links, safe_read_prop_metadata)

# Get just result part from output
player_prop_metadata <-
    player_prop_metadata |>
    map("result") |>
    map_df(bind_rows)

# Function to read a url and get the player props-------------------------------

read_prop_url <- function(url) {

    # Make request and get response
    sb_response <-
        request(url) |>
        req_perform() |>
        resp_body_json()

    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()

    # Loop through each market
    for (market in sb_response) {
        for (selection in market$selections) {

            # Append to vectors
            prop_market_name = c(prop_market_name, market$name)
            selection_name_prop = c(selection_name_prop, selection$name)
            prop_market_selection = c(prop_market_selection, selection$resultType)
            prop_market_price = c(prop_market_price, selection$price$winPrice)
            player_id = c(player_id, selection$externalId)
            market_id = c(market_id, market$externalId)
            if (is.null(selection$unformattedHandicap)) {
                selection$unformattedHandicap = NA
                handicap = c(handicap, selection$unformattedHandicap)
            } else {
                selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
                handicap = c(handicap, selection$unformattedHandicap)
            }
        }
    }

    # Output
    tibble(prop_market_name,
           selection_name_prop,
           prop_market_selection,
           prop_market_price,
           player_id,
           market_id,
           handicap,
           url)
}

# Safe version that just returns NULL if there is an error
safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)


#===============================================================================
# Main Markets
#===============================================================================

# Map function to player disposals urls
main_market_data <-
    map(main_market_links, safe_read_prop_url)

# Get just result part from output
main_market_data <-
    main_market_data |>
    map("result") |>
    map_df(bind_rows)

# Line Data---------------------------------------------------------------------

# Home teams
home_team_lines <-
    main_market_data |>
    filter(str_detect(prop_market_name, "^Line")) |> 
    filter(prop_market_selection == "H") |> 
    rename(home_win = prop_market_price) |> 
    mutate(home_line = handicap) |>
    mutate(home_team = fix_team_names(selection_name_prop)) |> 
    select(home_team, home_line, home_win, url)

# Away teams
away_team_lines <-
    main_market_data |>
    filter(str_detect(prop_market_name, "^Line")) |> 
    filter(prop_market_selection == "A") |> 
    rename(away_win = prop_market_price) |> 
    mutate(away_line = handicap*-1) |>
    mutate(away_team = fix_team_names(selection_name_prop)) |> 
    select(away_team, away_line, away_win, url)


# Combine
sportsbet_line_markets <-
    home_team_lines |>
    left_join(away_team_lines) |> 
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Line") |> 
    select(match, market_name, home_team, home_line, home_win, away_team, away_line, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Sportsbet")

# Write to csv
write_csv(sportsbet_line_markets, "Data/scraped_odds/sportsbet_line.csv")


#===============================================================================
# Player Disposals
#===============================================================================

# Map function to player disposals urls
player_disposals_data <-
    map(player_disposals_links, safe_read_prop_url)

# Get just result part from output
player_disposals_data <-
    player_disposals_data |>
    map("result") |>
    map_df(bind_rows)

# If nrow is zero, make empty tibble
if (nrow(player_disposals_data) == 0) {
  player_disposals_data <- tibble(
    prop_market_name = character(),
    url = character(),
    handicap = numeric(),
    selection_name_prop = character(),
    prop_market_price = numeric(),
    class_external_id = character(),
    competition_external_id = numeric(),
    event_external_id = numeric(),
    market_id = numeric(),
    player_id = numeric()
  )
}

# Add market name
player_disposals_data <-
    player_disposals_data |>
    filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
    mutate(market_name = "Player Disposals") |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team)) |>
    left_join(player_prop_metadata)

# Get player disposals alternate lines---------------------------------------------

player_disposals_alternate <-
    player_disposals_data |>
    filter(str_detect(prop_market_name, "To Get")) |>
    mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    rename(player_name = selection_name_prop) |>
    mutate(
        player_name =
            case_when(
                player_name == "Matthew Roberts" ~ "Matt Roberts",
                player_name == "Samuel Wicks" ~ "Sam Wicks",
                player_name == "Nathan Fyfe" ~ "Nat Fyfe",
                player_name == "Tom J. Lynch" ~ "Tom Lynch",
                player_name == "Brad Hill" ~ "Bradley Hill",
                player_name == "Mitch Hinge" ~ "Mitchell Hinge",
                player_name == "Minairo Frederick" ~ "Michael Frederick",
                player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
                player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
                .default = player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Disposals",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id
    )

# Get player disposals over / under -----------------------------------------------

player_disposals_over <-
    player_disposals_data |>
    filter(str_detect(selection_name_prop, "Over")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Over")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
    rename(line = handicap) |>
    mutate(
        player_name =
            case_when(
              player_name == "Matthew Roberts" ~ "Matt Roberts",
              player_name == "Samuel Wicks" ~ "Sam Wicks",
              player_name == "Nathan Fyfe" ~ "Nat Fyfe",
              player_name == "Tom J. Lynch" ~ "Tom Lynch",
              player_name == "Brad Hill" ~ "Bradley Hill",
              player_name == "Mitch Hinge" ~ "Mitchell Hinge",
              player_name == "Minairo Frederick" ~ "Michael Frederick",
              player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
              player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
                .default = player_name)) |>
    rename(over_price = prop_market_price) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Disposals",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        over_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id
    )

player_disposals_under <-
    player_disposals_data |>
    filter(str_detect(selection_name_prop, "Under")) |>
    separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
    mutate(line_2 = as.numeric(line_2)) |>
    mutate(handicap = coalesce(handicap, line_2)) |>
    rename(player_name = selection_name_prop) |>
    mutate(player_name = str_remove(player_name, " Under")) |>
    mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
    rename(line = handicap) |>
    mutate(
        player_name =
            case_when(
              player_name == "Matthew Roberts" ~ "Matt Roberts",
              player_name == "Samuel Wicks" ~ "Sam Wicks",
              player_name == "Nathan Fyfe" ~ "Nat Fyfe",
              player_name == "Tom J. Lynch" ~ "Tom Lynch",
              player_name == "Brad Hill" ~ "Bradley Hill",
              player_name == "Mitch Hinge" ~ "Mitchell Hinge",
              player_name == "Minairo Frederick" ~ "Michael Frederick",
              player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
              player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
                .default = player_name)) |>
    rename(under_price = prop_market_price) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
    relocate(match, .before = player_name) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Disposals",
        player_name,
        player_team = team_name,
        opposition_team,
        line,
        under_price,
        agency = "Sportsbet",
        class_external_id,
        competition_external_id,
        event_external_id,
        market_id,
        player_id_unders = player_id
    )

# Combine
player_disposals_over_under <-
    player_disposals_over |>
    left_join(player_disposals_under)

#===============================================================================
# Player Goals
#===============================================================================

# Map function to player goals urls
player_goals_data <-
  map(player_goals_links, safe_read_prop_url)

# Get just result part from output
player_goals_data <-
  player_goals_data |>
  map("result") |>
  map_df(bind_rows)

# If nrow is zero, make empty tibble
if (nrow(player_goals_data) == 0) {
  player_goals_data <- tibble(
    prop_market_name = character(),
    url = character(),
    handicap = numeric(),
    selection_name_prop = character(),
    prop_market_price = numeric(),
    class_external_id = character(),
    competition_external_id = numeric(),
    event_external_id = numeric(),
    market_id = numeric(),
    player_id = numeric()
  )
}

# Add market name
player_goals_data <-
  player_goals_data |>
  filter(str_detect(prop_market_name, "Qtr", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "Quarter", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "Half", negate = TRUE)) |>
  filter(str_detect(prop_market_name, "Early", negate = TRUE)) |>
  mutate(market_name = "Player Goals") |>
  mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
  rename(match_id = url) |>
  mutate(match_id = as.numeric(match_id)) |>
  left_join(team_names, by = "match_id") |>
  mutate(match = paste(home_team, "v", away_team)) |>
  left_join(player_prop_metadata) |>
  mutate(
    prop_market_name = if_else(
      prop_market_name == "Any Time Goal Scorer",
      "To Score 1 or More Goals",
      prop_market_name
    )
  )

# Get player goals alternate lines---------------------------------------------

player_goals_alternate <-
  player_goals_data |>
  filter(str_detect(prop_market_name, "To Score")) |>
  mutate(line = str_extract(prop_market_name, "\\d{1,2}")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  rename(player_name = selection_name_prop) |>
  mutate(
    player_name =
      case_when(
        player_name == "Matthew Roberts" ~ "Matt Roberts",
        player_name == "Samuel Wicks" ~ "Sam Wicks",
        player_name == "Nathan Fyfe" ~ "Nat Fyfe",
        player_name == "Tom J. Lynch" ~ "Tom Lynch",
        player_name == "Brad Hill" ~ "Bradley Hill",
        player_name == "Mitch Hinge" ~ "Mitchell Hinge",
        player_name == "Minairo Frederick" ~ "Michael Frederick",
        player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
        player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
        player_name == "Lachlan Fogarty" ~ "Lachie Fogarty",
        player_name == "Matthew Taberner" ~ "Matt Taberner",
        .default = player_name)) |>
  rename(over_price = prop_market_price) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player Goals",
    player_name,
    player_team = team_name,
    opposition_team,
    line,
    over_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id
  )

# Get player goals over / under -----------------------------------------------

player_goals_over <-
  player_goals_data |>
  filter(str_detect(selection_name_prop, "Over")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Over ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |>
  mutate(player_name = str_remove(player_name, " Over")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |>
  mutate(
    player_name =
      case_when(
        player_name == "Matthew Roberts" ~ "Matt Roberts",
        player_name == "Samuel Wicks" ~ "Sam Wicks",
        player_name == "Nathan Fyfe" ~ "Nat Fyfe",
        player_name == "Tom J. Lynch" ~ "Tom Lynch",
        player_name == "Brad Hill" ~ "Bradley Hill",
        player_name == "Mitch Hinge" ~ "Mitchell Hinge",
        player_name == "Minairo Frederick" ~ "Michael Frederick",
        player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
        player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
        player_name == "Lachlan Fogarty" ~ "Lachie Fogarty",
        player_name == "Matthew Taberner" ~ "Matt Taberner",
        .default = player_name)) |>
  rename(over_price = prop_market_price) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player Goals",
    player_name,
    player_team = team_name,
    opposition_team,
    line,
    over_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id
  )

player_goals_under <-
  player_goals_data |>
  filter(str_detect(selection_name_prop, "Under")) |>
  separate(selection_name_prop, into = c("name_2", "line_2"), sep = " Under ", remove = FALSE) |>
  mutate(line_2 = as.numeric(line_2)) |>
  mutate(handicap = coalesce(handicap, line_2)) |>
  rename(player_name = selection_name_prop) |>
  mutate(player_name = str_remove(player_name, " Under")) |>
  mutate(player_name = str_remove(player_name, " \\d+\\.\\d+")) |>
  rename(line = handicap) |>
  mutate(
    player_name =
      case_when(
        player_name == "Matthew Roberts" ~ "Matt Roberts",
        player_name == "Samuel Wicks" ~ "Sam Wicks",
        player_name == "Nathan Fyfe" ~ "Nat Fyfe",
        player_name == "Tom J. Lynch" ~ "Tom Lynch",
        player_name == "Brad Hill" ~ "Bradley Hill",
        player_name == "Mitch Hinge" ~ "Mitchell Hinge",
        player_name == "Minairo Frederick" ~ "Michael Frederick",
        player_name == "Bailey Williams (WB)" ~ "Bailey Williams",
        player_name == "Bailey J. Williams (WCE)" ~ "Bailey J. Williams",
        player_name == "Lachlan Fogarty" ~ "Lachie Fogarty",
        player_name == "Matthew Taberner" ~ "Matt Taberner",
        .default = player_name)) |>
  rename(under_price = prop_market_price) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  mutate(opposition_team = if_else(team_name == home_team, away_team, home_team)) |>
  relocate(match, .before = player_name) |>
  transmute(
    match,
    home_team,
    away_team,
    market_name = "Player Goals",
    player_name,
    player_team = team_name,
    opposition_team,
    line,
    under_price,
    agency = "Sportsbet",
    class_external_id,
    competition_external_id,
    event_external_id,
    market_id,
    player_id_unders = player_id
  )

# Combine
player_goals_over_under <-
  player_goals_over |>
  left_join(player_goals_under)

#===============================================================================
# Write to CSV
#===============================================================================

# Disposals
player_disposals_alternate |>
    bind_rows(player_disposals_over_under) |>
    select(
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
        "opposition_team",
        "class_external_id",
        "competition_external_id",
        "event_external_id",
        "market_id",
        "player_id",
        "player_id_unders"
    ) |>
    mutate(market_name = "Player Disposals") |>
    mutate(agency = "Sportsbet") |>
    write_csv("Data/scraped_odds/sportsbet_player_disposals.csv")

# Goals
player_goals_alternate |>
  bind_rows(player_goals_over_under) |>
  select(
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
    "opposition_team",
    "class_external_id",
    "competition_external_id",
    "event_external_id",
    "market_id",
    "player_id",
    "player_id_unders"
  ) |>
  mutate(market_name = "Player Goals") |>
  mutate(agency = "Sportsbet") |>
  write_csv("Data/scraped_odds/sportsbet_player_goals.csv")
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
