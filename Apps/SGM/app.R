#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)  
library(mongolite)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in and normalise DVP Data
#===============================================================================

# Read in data
dvp_data <-
  read_csv("../../DVP/dvp_data.csv")

# Read in position data---------------------------------------------------------
player_positions <-
  read_excel("../../DVP/AFL-Players-Positions-2024.xlsx") |>
  select(
    player_full_name,
    player_team = team_name,
    pos_1 = `position 1`,
    pos_2 = `position 2`
  ) |>
  mutate(pos_1_factor = factor(
    pos_1,
    levels = 1:11,
    labels = c(
      "Key Defender",
      "Small Defender",
      "Offensive Defender",
      "CBA > 50%",
      "CBA < 50%",
      "Wing",
      "Contested",
      "Uncontested",
      "Ruck",
      "Key Forward",
      "Small Forward"
    )
  )) |> 
  mutate(pos_2_factor = factor(
    pos_2,
    levels = 1:11,
    labels = c(
      "Key Defender",
      "Small Defender",
      "Offensive Defender",
      "CBA > 50%",
      "CBA < 50%",
      "Wing",
      "Contested",
      "Uncontested",
      "Ruck",
      "Key Forward",
      "Small Forward"
    )
  )) |> 
  select(player_name = player_full_name, player_team, Position = pos_1_factor)


dvp_data <-
  dvp_data %>%
  mutate(dvp = ifelse(market_name == "Player Goals", rnorm(nrow(dvp_data)), dvp)) |> 
  group_by(market_name) %>%
  mutate(
    DVP_Category = cut(
      dvp,
      breaks = quantile(dvp, probs = 0:5/5, na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Terrible", "Bad", "Neutral", "Good", "Excellent")
    )
  ) %>%
    mutate(DVP_Category = as.character(DVP_Category)) |> 
    mutate(DVP_Category = ifelse(market_name == "Player Goals", "Neutral", DVP_Category)) |> 
  ungroup() %>%
  select(Position = Pos, opposition_team = Opponent, market_name, DVP_Category)

# Head to head data
h2h <- read_csv("../../Data/scraped_odds/sportsbet_h2h.csv")

# Matches in order
matches_in_order <-
  h2h %>%
  distinct(match) |> 
  pull()

#===============================================================================
# Create compare sgm function
#===============================================================================

# Source scripts
source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
source("pointsbet_sgm.R")
source("neds_sgm.R")
source("bet365_sgm.R")

# Dabble------------------------------------------------------------------
dabble_sgm_list <- list(
  read_csv("../../Data/scraped_odds/dabble_player_disposals.csv"),
  read_csv("../../Data/scraped_odds/dabble_player_goals.csv"),
  read_csv("../../Data/scraped_odds/dabble_player_fantasy_points.csv")
)

dabble_sgm <-
  dabble_sgm_list |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  rename(price = over_price) |> 
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
  select(-contains("under"))

# TopSport SGM------------------------------------------------------------------
topsport_sgm_list <- list(
  read_csv("../../Data/scraped_odds/topsport_player_disposals.csv"),
  read_csv("../../Data/scraped_odds/topsport_player_goals.csv"),
  read_csv("../../Data/scraped_odds/topsport_player_tackles.csv"),
  read_csv("../../Data/scraped_odds/topsport_player_marks.csv"),
  read_csv("../../Data/scraped_odds/topsport_player_fantasy_points.csv")
)

topsport_sgm <-
  topsport_sgm_list |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  rename(price = over_price) |> 
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
  select(-contains("under"))

# Unibet SGM------------------------------------------------------------------
unibet_sgm_list <- list(
  read_csv("../../Data/scraped_odds/unibet_player_disposals.csv"),
  read_csv("../../Data/scraped_odds/unibet_player_goals.csv")
)

unibet_sgm <-
  unibet_sgm_list |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  rename(price = over_price) |> 
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
  select(-contains("under"))

#===============================================================================
# Create compare sgm function
#===============================================================================

compare_sgm <- function(player_names, stat_counts, markets) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, stat_counts, markets) {
    tryCatch({
      func(sgm, player_names, stat_counts, markets)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Markets = NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }
  
  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, stat_counts, markets)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, stat_counts, markets)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, stat_counts, markets)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, stat_counts, markets)
  neds_data <- handle_call_sgm(call_sgm_neds, neds_sgm, player_names, stat_counts, markets)
  bet365_data <- handle_call_sgm(call_sgm_bet365, bet365_sgm, player_names, stat_counts, markets)
  
  # Bind together and return
  bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, neds_data, bet365_data) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

#===============================================================================
# Compare CGM function
#===============================================================================

compare_cgm <- function(player_names_cross, lines_cross, market_names_cross) {
  # List of each agency data
  all_data <- list(pointsbet_sgm, sportsbet_sgm, tab_sgm, betright_sgm, neds_sgm, bet365_sgm, dabble_sgm, topsport_sgm, unibet_sgm)
  
  # Function to get cross game multi data
  get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross) {
    if (length(player_names_cross) != length(lines_cross) || length(lines_cross) != length(market_names_cross)) {
      stop("All lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names_cross)) {
      temp_df <- data %>%
        filter(player_name == player_names_cross[i],
               line == lines_cross[i],
               market_name == market_names_cross[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    if (nrow(filtered_df) != length(player_names_cross)) {
      return(NULL)
    }
    
    price <- prod(filtered_df$price)
    
    combined_list <- paste(player_names_cross, lines_cross, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    market_string <- paste(market_names_cross, collapse = ", ")
    match_string <- paste(filtered_df$match, collapse = ", ")
    
    output_data <- data.frame(
      Selections = player_string,
      Matches = match_string,
      Markets = market_string,
      Price = round(price, 2),
      Agency = first(data$agency)
    )
    
    return(output_data)
  }
  
  # Function to handle errors in the get_cgm function
  handle_get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross) {
    tryCatch({
      get_cgm(data, player_names_cross, lines_cross, market_names_cross)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections = NA, Matches = NA, Markets = NA, Price = NA, Agency = NA)
    })
  }
  
  # Map over list of dataframes
  cgm_all <- map_dfr(all_data, handle_get_cgm, player_names_cross, lines_cross, market_names_cross) %>%
    arrange(desc(Price))
  
  return(cgm_all)
}

# Read in datasets--------------------------------------------------------------
disposals <-
  read_rds("../../Data/processed_odds/all_player_disposals.rds") |> 
  rename(price = over_price,
         empirical_probability_2023 = empirical_prob_over_2023,
         diff_2023 = diff_over_2023)

goals <-
  read_rds("../../Data/processed_odds/all_player_goals.rds") |> 
  rename(price = over_price,
         empirical_probability_2023 = empirical_prob_over_2023,
         diff_2023 = diff_over_2023)

marks <- 
  read_rds("../../Data/processed_odds/all_player_marks.rds") |> 
  rename(price = over_price,
         empirical_probability_2023 = empirical_prob_over_2023,
         diff_2023 = diff_over_2023)

tackles <- 
  read_rds("../../Data/processed_odds/all_player_tackles.rds") |> 
  rename(price = over_price,
         empirical_probability_2023 = empirical_prob_over_2023,
         diff_2023 = diff_over_2023)

fantasy_points <- 
  read_rds("../../Data/processed_odds/all_player_fantasy_points.rds") |> 
  rename(price = over_price,
         empirical_probability_2023 = empirical_prob_over_2023,
         diff_2023 = diff_over_2023)

disposals <-
  disposals |>
  bind_rows(goals) |> 
  bind_rows(marks) |>
  bind_rows(tackles) |>
  bind_rows(fantasy_points) |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |> 
  relocate(Position, DVP_Category, .after = player_name)

# Create market best
disposals <-
  disposals |>
  group_by(match, player_name, market_name, line) |>
  mutate(max_player_diff = max(diff_over_last_10, na.rm = TRUE)) |>
  ungroup()

# Unique matches
matches <- matches_in_order

# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  group_by(player_name, match, line, market_name) |>
  mutate(market_best = max_player_diff == diff_over_last_10) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |> 
  transmute(match,
         player_name,
         Position,
         Matchup = DVP_Category,
         market_name,
         line,
         price,
         agency,
         prob_2023 = round(empirical_probability_2023, 2),
         diff_2023 = round(diff_2023, 2),
         prob_last_10 = round(emp_prob_last_10, 2),
         diff_last_10 = round(diff_over_last_10, 2),
         market_best)

# Get correlations
correlations_2023 <-
  read_rds("../../Data/player_correlations_disposals_23.rds") |>
  mutate_if(is.numeric, round, digits = 2)

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Multitool"),
  
  tabsetPanel(
    
    # SGM Tab
    tabPanel("SGM",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "match",
                   "Select Match",
                   choices = matches,
                   selected = NULL
                 ),
                 selectInput(
                   "agency",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market",
                   "Select Market",
                   choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   multiple = TRUE
                 ),
                 selectInput(
                   "matchup",
                   "Select Difficulty",
                   choices = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
                   selected = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
                   multiple = TRUE
                 ),
                 checkboxInput("best_odds", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),
                 DT::dataTableOutput("selected"),
                 h3("Pairwise Correlations"),
                 DT::dataTableOutput("correlations"),
                 h3("SGM Information"),
                 uiOutput("summary"),
                 h3("Odds Comparison"),
                 actionButton("get_comparison", label = "Compare Odds"),
                 actionButton("clear_comparison", label = "Clear Selections"),
                 DT::dataTableOutput("odds_compare")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Player List", 
                            DT::dataTableOutput("table")
                   )
                   )
               )
             )
    ),
    
    # Cross Game Multi Tab
    tabPanel("Cross Game Multi",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "agency_cross",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market_cross",
                   "Select Market",
                   choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   multiple = TRUE
                 ),
                 selectInput(
                   "matchup_cross",
                   "Select Difficulty",
                   choices = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
                   selected = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
                   multiple = TRUE
                 ),
                 checkboxInput("best_odds_cross", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),
                 DT::dataTableOutput("selected_cross"),
                 h3("Multi Information"),
                 uiOutput("summary_cross"),
                 actionButton("get_comparison_cross", label = "Compare Odds"),
                 actionButton("clear_comparison_cross", label = "Clear Selections"),
                 DT::dataTableOutput("odds_compare_cross")
               ),
               
               mainPanel(
                 DT::dataTableOutput("table_cross")
               )
             )
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  # For the "SGM" panel
  output$table <- renderDT({
    filtered_data <-
      disposals_display[disposals_display$match == input$match &
                          disposals_display$agency == input$agency &
                          disposals_display$Matchup %in% input$matchup &
                          disposals_display$market_name %in% input$market,]
    
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data, selection = "multiple", filter = "top")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <-
          disposals_display[disposals_display$match == input$match &
                              disposals_display$agency == input$agency &
                              disposals_display$Matchup %in% input$matchup &
                              disposals_display$market_name %in% input$market,]
        if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data)
      }
    })
  })
  
  # Get the table proxy
  proxy <- dataTableProxy("table")
  
  # Get the table proxy for the cross game multi
  proxy_cross <- dataTableProxy("table_cross")
  
  output$correlations <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
    
    correlations_table <- correlations_2023 |> filter(player_a %in% selected_data$player_name & player_b %in% selected_data$player_name)
    datatable(correlations_table)
  })
  
  # SGM Comparison
  observeEvent(input$get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match &
                                         disposals_display$agency == input$agency &
                                         disposals_display$Matchup %in% input$matchup &
                                         disposals_display$market_name %in% input$market,]
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
    
    player_names = selected_data$player_name
    lines = selected_data$line
    market_names = selected_data$market_name
    
    # Call function
    comparison_df <- compare_sgm(
      player_names = player_names,
      stat_counts = lines,
      markets = market_names)
    
    # populate DTOutput
    output$odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })
  
  # Observe the click event on the "clear_rows" button
  observeEvent(input$clear_comparison, {
    # Deselect all rows in the table
    selectRows(proxy, NULL)
  })
  
  observeEvent(input$clear_comparison_cross, {
    # Deselect all rows in the table
    selectRows(proxy_cross, NULL)
  })
  
  # observeEvent(input$get_combos, {
  #   # Get selected data
  #   filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
  #   if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
  #   selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
  #   
  #   player_names = selected_data$player_name
  #   number_of_disposals = selected_data$number_of_disposals
  #   
  #   # Call function
  #   combo_df <- sgm_combinations(player_names, number_of_disposals)
  #   
  #   # populate DTOutput
  #   output$all_selected_combinations <- renderDT({
  #     datatable(combo_df, extensions = "Buttons", options = list(buttons = c('copy', 'csv', 'excel')))
  #   })
  # })
  # 
  
  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match &
                                           disposals_display$agency == input$agency &
                                           disposals_display$Matchup %in% input$matchup &
                                           disposals_display$market_name %in% input$market,]
      if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_last_10)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                  " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
  
  # For the "Cross Game Multi" panel
  output$table_cross <- renderDT({
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                             disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data_cross, selection = "multiple", filter = "top")
  }, server = FALSE) 
  
  observeEvent(input$table_cross_rows_selected,{
    output$selected_cross <- renderDT({
      if(!is.null(input$table_cross_rows_selected)){
        filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                                 disposals_display$Matchup %in% input$matchup_cross &
                                                 disposals_display$market_name %in% input$market_cross,]
        
        if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
        
        selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data_cross)
      }
    })
  })
  
  # Cross Game Comparison
  observeEvent(input$get_comparison_cross, {
    # Get selected data
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                               disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price", "agency")]
    
    player_names_cross = selected_data_cross$player_name
    lines_cross = selected_data_cross$line
    market_names_cross = selected_data_cross$market_name
    
    # Call function
    comparison_df_cross <-
      compare_cgm(market_names_cross = market_names_cross,
                  player_names_cross = player_names_cross,
                  lines_cross = lines_cross)
    
    # populate DTOutput
    output$odds_compare_cross <- renderDT({
      datatable(comparison_df_cross)
    })
  })
  
  output$summary_cross <- renderUI({
    if(!is.null(input$table_cross_rows_selected)){
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                               disposals_display$market_name %in% input$market_cross,]
      
      if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
      
      selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2023)
      empirical_price_cross_l10 <- 1 / prod(selected_data_cross$prob_last_10)
      diff = 1/empirical_price_cross - 1/uncorrelated_price_cross
      diff_l10 = 1/empirical_price_cross_l10 - 1/uncorrelated_price_cross
      HTML(paste0("<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
                  " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
                  " <strong>Edge L10:</strong>", " ", round(100*diff_l10, 3), "%"), "<br/>",
                  " <strong>Edge 2023:</strong>", " ", round(100*diff, 3), "%")
    }
  })
}

##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
