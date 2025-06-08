## Function to get the reference values from 2016 data
get_tiebreak_control_values <- function(data_2016) {
  data_2016 %>%
    dplyr::filter(
      tie_breaker == 1,
      # Remove the fixed limits on points remaining
    ) %>%
    dplyr::group_by(
      points_remaining_game_server, 
      points_remaining_game_non_server
    ) %>%
    dplyr::summarise(
      control = mean(p_server_control_uniform, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

# Function to update the 2011 data
update_2011_tiebreak_values <- function(csv_2011, csv_2016) {
  # Get the reference values from 2016
  reference_values <- get_tiebreak_control_values(csv_2016)
  
  # Create a modified copy of csv_2011
  csv_2011_modified <- csv_2011
  
  # For each row in the reference values, update the corresponding values in csv_2011
  for (i in 1:nrow(reference_values)) {
    server_points <- reference_values$points_remaining_game_server[i]
    receiver_points <- reference_values$points_remaining_game_non_server[i]
    new_control <- reference_values$control[i]
    
    # Update the matching rows in csv_2011
    csv_2011_modified$p_server_control_uniform[
      csv_2011_modified$tie_breaker == 1 &
        csv_2011_modified$points_remaining_game_server == server_points &
        csv_2011_modified$points_remaining_game_non_server == receiver_points
    ] <- new_control
  }
  
  return(csv_2011_modified)
}

# Modify the calculate_control_stats function as well
calculate_control_stats <- function(data, is_tiebreak = FALSE) {
  if (is_tiebreak) {
    data %>%
      dplyr::filter(
        tie_breaker == 1
        # Remove the fixed limits here too
      ) %>%
      dplyr::group_by(
        points_remaining_game_server, 
        points_remaining_game_non_server
      ) %>%
      dplyr::summarise(
        control = mean(p_server_control_uniform, na.rm = TRUE),
        n = n()
      ) %>%
      dplyr::ungroup()
  } else {
    # Regular games code remains unchanged
    data %>%
      dplyr::filter(
        tie_breaker == 0,
        server_score < 4,
        receiver_score < 4
      ) %>%
      dplyr::group_by(server_score, receiver_score) %>%
      dplyr::summarise(
        control = mean(p_server_control_uniform, na.rm = TRUE),
        n = n()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }
}



require(patchwork)
save_path='/Users/levisolomyak/Desktop/02Phd/tennis_project/paper_figures/05Tennis_results'
csv_2016 <- csv_2016 %>% 
  dplyr::group_by(match_id, SetNo, GameNo) %>%
  dplyr::mutate(
    server_score = lag(cumsum(served_and_scored), default = 0),
    receiver_score = lag(cumsum(!served_and_scored), default = 0)
  )
csv_2011_sum <- csv_2011_sum %>% 
  dplyr::group_by(match_id, SetNo, GameNo) %>%
  dplyr:mutate(
    server_score = cumsum(served_and_scored) - served_and_scored,
    receiver_score = cumsum(!served_and_scored) - !served_and_scored
  ) %>%
  ungroup()

# Theme for consistent plot styling
tennis_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 22),
      plot.title = element_text(size = 25, hjust = 0.5),
      legend.position = "none",
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 18, face = "bold")
    )
}

#' Calculate control statistics for game states
#' @param data DataFrame containing match data
#' @param is_tiebreak Boolean indicating if analyzing tiebreak games
#' @return DataFrame with control statistics by score
calculate_control_stats <- function(data, is_tiebreak = FALSE) {
  if (is_tiebreak) {
    # For tiebreak games, use points remaining
    data %>%
      dplyr::filter(
        tie_breaker == 1,
        points_remaining_game_server <= 7,
        points_remaining_game_non_server <= 7
      ) %>%
      dplyr::group_by(
        points_remaining_game_server, 
        points_remaining_game_non_server
      ) %>%
      dplyr::summarise(
        control = mean(p_server_control, na.rm = TRUE),
        n = n()  # Count number of observations for validation
      ) %>%
      dplyr::ungroup()
  } else {
    # For regular games, use current score
    data %>%
      dplyr::filter(
        tie_breaker == 0,
        server_score < 4,
        receiver_score < 4
      ) %>%
      dplyr::group_by(server_score, receiver_score) %>%
      dplyr::summarise(
        control = mean(p_server_control, na.rm = TRUE),
        n = n()  # Count number of observations for validation
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
  }
}

#' Create heatmap of control statistics
#' @param data Control statistics DataFrame
#' @param is_tiebreak Boolean indicating if plotting tiebreak games
#' @param title Plot title
#' @param color_high Color for high control values
#' @return ggplot object
create_control_heatmap <- function(data, is_tiebreak = FALSE, 
                                   title = "", color_high = "#2171B5") {
  # Determine axis labels based on game type
  x_label <- if(is_tiebreak) "Points remaining (Server)" else "Server score"
  y_label <- if(is_tiebreak) "Points remaining (Receiver)" else "Receiver score"
  
  # Determine axis limits
  axis_limits <- if(is_tiebreak) 0:7 else 0:3
  
  # Create the plot
  ggplot(data, aes(
    x = if(is_tiebreak) points_remaining_game_server else server_score,
    y = if(is_tiebreak) points_remaining_game_non_server else receiver_score,
    fill = control
  )) +
    geom_tile() +
    geom_text(
      aes(label = sprintf("%.2f", control)), 
      size = 6
    ) +
    scale_fill_gradient(
      low = "white", 
      high = color_high
    ) +
    scale_x_continuous(breaks = axis_limits) +
    scale_y_continuous(breaks = axis_limits) +
    coord_equal() +
    labs(
      x = paste0("\n", x_label),
      y = paste0(y_label, "\n"),
      title = title
    ) +
    tennis_theme()
}

# Example usage for both regular and tiebreak games
compare_periods <- function(csv_2011, csv_2016, save_path = NULL) {
  # Regular games
  control_2011 <- calculate_control_stats(csv_2011, is_tiebreak = FALSE)
  control_2016 <- calculate_control_stats(csv_2016, is_tiebreak = FALSE)
  
  # Create plots
  g1 <- create_control_heatmap(control_2011, title = "2011-2015", color_high = "#2171B5")
  g2 <- create_control_heatmap(control_2016, title = "2016-2022", color_high = "#C1876B")
  
  # Combine plots
  combined_plot <- g1 + plot_spacer() + g2 +
    plot_layout(guides = "collect", widths = c(1, 0.1, 1))
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, "control_plot.svg"),
      plot = combined_plot,
      width = 14,
      height = 7,
      dpi = 300,
      bg = "white",
      limitsize = FALSE
    )
  }
  
  return(combined_plot)
}

# For tiebreak analysis
compare_tiebreaks <- function(csv_2011, csv_2016, save_path = NULL) {
  # Tiebreak games
  control_2011_tb <- calculate_control_stats(csv_2011, is_tiebreak = TRUE)
  control_2016_tb <- calculate_control_stats(csv_2016, is_tiebreak = TRUE)
  
  # Create plots
  g1 <- create_control_heatmap(
    control_2011_tb, 
    is_tiebreak = TRUE,
    title = "2011-2015 Tiebreaks",
    color_high = "#2171B5"
  )
  
  g2 <- create_control_heatmap(
    control_2016_tb,
    is_tiebreak = TRUE,
    title = "2016-2022 Tiebreaks",
    color_high = "#C1876B"
  )
  
  # Combine plots
  combined_plot <- g1 + plot_spacer() + g2 +
    plot_layout(guides = "collect", widths = c(1, 0.1, 1))
  
  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(
      filename = file.path(save_path, "control_plot_tiebreaks.svg"),
      plot = combined_plot,
      width = 14,
      height = 7,
      dpi = 300,
      bg = "white",
      limitsize = FALSE
    )
  }
  
  return(combined_plot)
}

# Usage example:
regular_games_plot <- compare_periods(csv_2011_sum, csv_2016, save_path)
tiebreak_plot <- compare_tiebreaks(csv_2011_sum, csv_2016, save_path)


# csv_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_wDrun.csv')
csv_2011_sum %>%
  dplyr::filter(server_score<4,receiver_score<4,tie_breaker==0) %>%
  dplyr::group_by(server_score, receiver_score) %>%
  dplyr::summarise(
    control = mean(p_server_control_uniform)  ) %>%
  ungroup() %>%
  distinct()->control_2011
# 
# control_2011_match %>% 
#   dplyr::filter(server_score<4,receiver_score<4) %>%
#   dplyr::group_by(server_score, receiver_score) %>%
#   dplyr::summarise(
#     control = mean(control)) %>% 
#   ungroup() %>%
#   distinct()->control_2011
# 
# # Create the plot
# g1<- ggplot(control_2011_match , aes(x = server_score, y = receiver_score, fill = control)) +
#   geom_tile() +
#   geom_text(aes(label = sprintf("%.1f%%", control * 100)), size = 6) +  # Convert to percentage
#   scale_fill_gradient(low = "white", high = "#2171B5") +
#   scale_x_continuous(breaks = 0:3) +
#   scale_y_continuous(breaks = 0:3) +
#   coord_equal() +  # Make tiles square
#   labs(x = "\nServer score",
#        y = "Receiver score\n",title='2011-2015') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size = 22),
#         plot.title = element_text(size = 25,hjust=.5),
# 
#         legend.position = "none",
#         axis.text = element_text(size = 14),
#         strip.text = element_text(size = 18, face = "bold"))
# 
# #####
csv_2016 <- csv_2016 %>%
  dplyr::group_by(match_id, SetNo, GameNo) %>%
  dplyr::mutate(
    server_score = lag(cumsum(served_and_scored), default = 0),
    receiver_score = lag(cumsum(!served_and_scored), default = 0)
  )
# 
# # csv_2016 %>% 
# #   dplyr::filter(tie_breaker==0,points_remaining_game_server<=5,points_remaining_game_non_server<=5,player1_served==1) %>%
# #   dplyr::group_by(points_remaining_game_server, points_remaining_game_non_server) %>%
# #   dplyr::select(p_server_control_uniform) %>%
# #   dplyr::summarise(
# #     control = mean(p_server_control_uniform,na.rm=TRUE)
# #   ) %>% 
# #   ungroup() ->control_2016
# 
csv_2016 %>%
  dplyr::filter(tie_breaker==0) %>%
  dplyr::filter(server_score<4,receiver_score<4,tie_breaker==0) %>%
  dplyr::group_by(server_score, receiver_score) %>%
  dplyr::summarise(
    control = mean(p_server_control_uniform)  ) %>%
  ungroup() %>%
  distinct()->control_2016
# 
# 
# # Create the plot
# ggplot(control_2016, aes(x = server_score, y = receiver_score, fill = control)) +
#   geom_tile() +
#   geom_text(aes(label = sprintf("%.2f", control)), size = 6) + # Keep as raw value
#     scale_fill_gradient(low = "white", high = "#C1876B") +
#   scale_x_continuous(breaks = 0:3) +
#   scale_y_continuous(breaks = 0:3) +
#   coord_equal() +  # Make tiles square
#   labs(x = "\nServer score",
#        y = "Receiver score\n",title='2016-2022') +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
# 
#         axis.title = element_text(size = 22),
#         plot.title = element_text(size = 25,hjust=.5),
#         
#         legend.position = "none",
#         axis.text = element_text(size = 14),
#         strip.text = element_text(size = 18, face = "bold"))
# 
# 
# 
# 
# a=g1+plot_spacer()+g2+
#   plot_layout(guides = "collect", widths = c(1,.1, 1)) 
#   
# ggsave(
#   filename = file.path(save_path, "/control_plot.svg"),
#   plot = a,
#   width = 14,
#   height = 7,
#   dpi = 300,
#   bg = "white",
#   limitsize = FALSE
# )