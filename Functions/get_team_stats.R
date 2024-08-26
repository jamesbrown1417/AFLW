##%######################################################%##
#                                                          #
####       Function to get match total team stats       ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries
#===============================================================================

library(tidyverse)

#===============================================================================
# Function
#===============================================================================

get_team_stats <- function(data) {
  # Get total team stats for each match
  team_stats <-
    data |>
    group_by(match_name,
             round,
             season_name,
             player_team,
             venue,
             start_time_utc,
             opposition_team) |>
    summarise(Disposals = sum(disposals, na.rm = TRUE),
              Marks = sum(marks, na.rm = TRUE),
              Tackles = sum(tackles, na.rm = TRUE),
              Fantasy_Points = sum(fantasy_points, na.rm = TRUE),
              Total_Points = sum(goals*6 + behinds, na.rm = TRUE)) |> 
    ungroup() |> 
    arrange(desc(start_time_utc), match_name)
  
  return(team_stats)
}

##%######################################################%##
#                                                          #
####                  Opposition Team                   ####
#                                                          #
##%######################################################%##

#===============================================================================
# Disposals
#===============================================================================

# Assuming team_stats is your dataframe and it's already loaded
# Reorder opposition_team based on the median of Disposals
team_stats <- team_stats %>%
  mutate(opposition_team = factor(opposition_team, levels = team_stats %>%
                                    group_by(opposition_team) %>%
                                    summarise(MedianDisposals = median(Disposals)) %>%
                                    arrange(desc(MedianDisposals)) %>%
                                    pull(opposition_team)))

# Create the plot with enhanced aesthetics
ggplot(team_stats, aes(x = opposition_team, y = Disposals)) +
  geom_boxplot(outlier.shape = NA, fill = "royalblue1", color = "#1f2d3d", alpha = 0.8) + # Custom colors and transparency
  geom_jitter(width = 0.2, color = "orangered1", alpha = 0.5) + # Adding jitter to show individual data points
  labs(title = "Disposals by Opposition Team",
       subtitle = "Ordered by Median Disposals",
       x = "Opposition Team",
       y = "Disposals") +
  theme_minimal(base_size = 14) + # Larger base font size for readability
  theme(plot.title = element_text(size = 20, face = "bold"), # Enhancing title aesthetics
        plot.subtitle = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "gray20"), # Adjusting x-axis text
        axis.text.y = element_text(color = "gray20"), # Adjusting y-axis text
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none", # Assuming we don't need a legend
        panel.grid.major = element_line(color = "gray90"), # Lighter grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines for cleaner look
        plot.background = element_rect(fill = "gray98"), # Light gray background for softer look
        panel.border = element_rect(color = "gray70", fill = NA)) # Customizing panel border

#===============================================================================
# Fantasy Points
#===============================================================================

# Assuming team_stats is your dataframe and it's already loaded
# Reorder opposition_team based on the median of Fantasy_Points
team_stats <- team_stats %>%
  mutate(opposition_team = factor(opposition_team, levels = team_stats %>%
                                    group_by(opposition_team) %>%
                                    summarise(MedianFantasy_Points = median(Fantasy_Points)) %>%
                                    arrange(desc(MedianFantasy_Points)) %>%
                                    pull(opposition_team)))

# Create the plot with enhanced aesthetics
ggplot(team_stats, aes(x = opposition_team, y = Fantasy_Points)) +
  geom_boxplot(outlier.shape = NA, fill = "royalblue1", color = "#1f2d3d", alpha = 0.8) + # Custom colors and transparency
  geom_jitter(width = 0.2, color = "orangered1", alpha = 0.5) + # Adding jitter to show individual data points
  labs(title = "Fantasy Points by Opposition Team",
       subtitle = "Ordered by Median Fantasy Points",
       x = "Opposition Team",
       y = "Fantasy Points") +
  theme_minimal(base_size = 14) + # Larger base font size for readability
  theme(plot.title = element_text(size = 20, face = "bold"), # Enhancing title aesthetics
        plot.subtitle = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "gray20"), # Adjusting x-axis text
        axis.text.y = element_text(color = "gray20"), # Adjusting y-axis text
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none", # Assuming we don't need a legend
        panel.grid.major = element_line(color = "gray90"), # Lighter grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines for cleaner look
        plot.background = element_rect(fill = "gray98"), # Light gray background for softer look
        panel.border = element_rect(color = "gray70", fill = NA)) # Customizing panel border

##%######################################################%##
#                                                          #
####                         Team                       ####
#                                                          #
##%######################################################%##

#===============================================================================
# Disposals
#===============================================================================

# Assuming team_stats is your dataframe and it's already loaded
# Reorder player_team based on the median of Disposals
team_stats <- team_stats %>%
  mutate(player_team = factor(player_team, levels = team_stats %>%
                                    group_by(player_team) %>%
                                    summarise(MedianDisposals = median(Disposals)) %>%
                                    arrange(desc(MedianDisposals)) %>%
                                    pull(player_team)))

# Create the plot with enhanced aesthetics
ggplot(team_stats, aes(x = player_team, y = Disposals)) +
  geom_boxplot(outlier.shape = NA, fill = "royalblue1", color = "#1f2d3d", alpha = 0.8) + # Custom colors and transparency
  geom_jitter(width = 0.2, color = "orangered1", alpha = 0.5) + # Adding jitter to show individual data points
  labs(title = "Disposals by Team",
       subtitle = "Ordered by Median Disposals",
       x = "Team",
       y = "Disposals") +
  theme_minimal(base_size = 14) + # Larger base font size for readability
  theme(plot.title = element_text(size = 20, face = "bold"), # Enhancing title aesthetics
        plot.subtitle = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "gray20"), # Adjusting x-axis text
        axis.text.y = element_text(color = "gray20"), # Adjusting y-axis text
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none", # Assuming we don't need a legend
        panel.grid.major = element_line(color = "gray90"), # Lighter grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines for cleaner look
        plot.background = element_rect(fill = "gray98"), # Light gray background for softer look
        panel.border = element_rect(color = "gray70", fill = NA)) # Customizing panel border

#===============================================================================
# Fantasy Points
#===============================================================================

# Assuming team_stats is your dataframe and it's already loaded
# Reorder player_team based on the median of Fantasy_Points
team_stats <- team_stats %>%
  mutate(player_team = factor(player_team, levels = team_stats %>%
                                    group_by(player_team) %>%
                                    summarise(MedianFantasy_Points = median(Fantasy_Points)) %>%
                                    arrange(desc(MedianFantasy_Points)) %>%
                                    pull(player_team)))

# Create the plot with enhanced aesthetics
ggplot(team_stats, aes(x = player_team, y = Fantasy_Points)) +
  geom_boxplot(outlier.shape = NA, fill = "royalblue1", color = "#1f2d3d", alpha = 0.8) + # Custom colors and transparency
  geom_jitter(width = 0.2, color = "orangered1", alpha = 0.5) + # Adding jitter to show individual data points
  labs(title = "Fantasy Points by Team",
       subtitle = "Ordered by Median Fantasy Points",
       x = "Team",
       y = "Fantasy Points") +
  theme_minimal(base_size = 14) + # Larger base font size for readability
  theme(plot.title = element_text(size = 20, face = "bold"), # Enhancing title aesthetics
        plot.subtitle = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, color = "gray20"), # Adjusting x-axis text
        axis.text.y = element_text(color = "gray20"), # Adjusting y-axis text
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.position = "none", # Assuming we don't need a legend
        panel.grid.major = element_line(color = "gray90"), # Lighter grid lines
        panel.grid.minor = element_blank(), # Removing minor grid lines for cleaner look
        plot.background = element_rect(fill = "gray98"), # Light gray background for softer look
        panel.border = element_rect(color = "gray70", fill = NA)) # Customizing panel border
