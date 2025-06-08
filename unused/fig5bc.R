# Function to create match distribution plots
control_stats=structure(list(
  server_score = rep(c(0, 1, 2, 3), each = 4),
  receiver_score = rep(c(0, 1, 2, 3), times = 4),
  control = c(
    # Server 0
    0.12, 0.14, 0.10, 0.07,
    # Server 15
    0.08, 0.11, 0.12, 0.09,
    # Server 30
    0.04, 0.07, 0.10, 0.11,
    # Server 40
    0.01, 0.03, 0.06, 0.10
  ),
  percentage = c(
    # Server 0
    "12%", "14%", "10%", "7%",
    # Server 15
    "8%", "11%", "12%", "9%",
    # Server 30
    "4%", "7%", "10%", "11%",
    # Server 40
    "1%", "3%", "6%", "10%"
  )
), class = "data.frame", row.names = c(NA, -16L))

csv_2011_sum <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_wDrun.csv')
csv_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2016_wDrun.csv')
tennis_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 27),
      plot.title = element_text(hjust = 0.5, size = 30, margin = margin(b = 10)),
      legend.position = "none")
}
create_match_distribution <- function(csv_2011, csv_2016) {
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  save_path='/Users/levisolomyak/Desktop/02Phd/tennis_project/paper_figures/05Tennis_results'
  
  process_data <- function(data) {
    # First create a long format dataset that includes all player appearances
    all_appearances <- bind_rows(
      # When player is player1
      data %>% 
        dplyr::select(player = player1, match_id, male) %>%
        distinct(),
      # When player is player2
      data %>% 
        dplyr::select(player = player2, match_id, male) %>%
        distinct()
    )
    
    # Now count unique matches per player
    player_matches <- all_appearances %>%
      dplyr::group_by(player, male) %>%
      dplyr::summarise(
        n_matches = n_distinct(match_id),
        .groups = "drop"
      ) %>%
      arrange(desc(n_matches))
    
    # Optional: Add some debug information
    print(paste("Total unique matches in dataset:", n_distinct(data$match_id)))
    print(paste("Players with most matches:"))
    print(head(player_matches))
    
    return(player_matches)
  }
  
  # Optional: Function to compare with other calculations
  compare_match_counts <- function(data, player_id) {
    # Count using process_data
    method1 <- process_data(data)
    
    # Count using match statistics from win probability function
    win_probs <- compute_player_win_prob(data, sum_2016, player_id)
    method2_serving <- length(unique(win_probs$serving$data$match_id))
    method2_receiving <- length(unique(win_probs$receiving$data$match_id))
    
    cat("Method 1 (process_data) matches:", 
        method1$n_matches[method1$player == player_name], "\n")
    cat("Method 2 (win_prob) matches:", 
        length(unique(c(
          win_probs$serving$data$match_id,
          win_probs$receiving$data$match_id
        ))), "\n")
    cat("  - Serving matches:", method2_serving, "\n")
    cat("  - Receiving matches:", method2_receiving, "\n")
  }
  # Process both datasets
  matches_2011 <- process_data(csv_2011)
  matches_2011$period <- '2011-2015'
  matches_2016 <- process_data(csv_2016)
  matches_2016$period <- '2016-2022'
  
  create_plot <- function(data, title, period = "2011", show_legend = FALSE, show_y = TRUE) {
    # Choose color scheme based on period
    if(period == "2011") {
      colors <- c(Male = "#1A4055", Female = "#4682B4")  # Dark and light blue
    } else {
      colors <- c(Male = "#6B3012", Female = "#C1876B")  # Dark and light terra cotta
    }
    
    base_plot <- ggplot(data, aes(x = n_matches, 
                                  fill = factor(male, labels = c("Female", "Male")))) +
      # Use position = "stack" for stacked bars
      geom_histogram(position = "stack", 
                     alpha = 0.7, 
                     color = "white", 
                     size = 0.5,
                     binwidth = 5,    # Fixed bin width
                     boundary = 0) +   # Ensure bins start at 0
      scale_fill_manual(values = colors,
                        name = "Gender") +
      # Fixed x-axis range and breaks
      scale_x_continuous(expand = c(0,0), 
                         limits = c(0, 131),
                         breaks = seq(0, 130, by = 25)) +
      # Fixed y-axis range and breaks
       scale_y_continuous(expand = c(0,0),
                   limits = c(0, 235),
                      breaks = seq(50, 200, by = 50))+
        labs(title = title,
           x = NULL,
           y = if(show_y) "Count" else NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 22),
        axis.title = element_text(size = 27),
        axis.text = element_text(size = 22),
        panel.grid = element_blank(),
        legend.position = c(.75, .8),
        legend.text = element_text(size = 27),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 30),
        aspect.ratio = 1,
        plot.margin = margin(5, 20, 5, 5)
      )
    
    return(base_plot)
  }
  # Create plots with different color schemes - only show legend on first plot
    p1 <- create_plot(matches_2011, "2011-2015", "2011", show_legend = TRUE, show_y = TRUE)
    p2 <- create_plot(matches_2016, "2016-2022", "2016", show_legend = FALSE, show_y = FALSE)
    
    # Add y-axis label only to the left plot
    p1 <- p1 + ylab("Number of players")
    
    # Combine plots with shared x-axis label
    combined_plot <- p1 + p2 +
      plot_annotation(
        caption = "Number of matches",
        theme = theme(
          plot.caption = element_text(size = 27, hjust = 0.5, margin = margin(t = 15)),
          plot.caption.position = "panel"
        )
      )
    match_plot <- p1 + p2  + 
      plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10))) &
      xlab("Number of matches")
      
    
    # Save plot
    ggsave(paste0(save_path, "/match_distribution.png"), 
           combined_plot, 
           width = 12,
           height = 8,
           dpi = 300,
           bg = 'white')
    
    # Return both the plot and the processed data
  return( combined_plot)
}
match_plot=create_match_distribution(csv_2011_sum,csv_2016)

create_control_heatmap_for_plot <- function(data, is_tiebreak = FALSE, 
                                            title = "", color_high = "#2171B5") {
  # Determine axis labels based on game type
  x_label <- if(is_tiebreak) "Points remaining (Server)" else "Server score"
  y_label <- if(is_tiebreak) "Points remaining (Receiver)" else "Receiver score"
  
  # Determine axis limits and labels
  axis_limits <- if(is_tiebreak) 0:7 else 0:3
  
  # Define tennis score labels
  if(!is_tiebreak) {
    tennis_labels <- c("0", "15", "30", "40")
  } else {
    tennis_labels <- as.character(axis_limits)  # Keep numeric for tiebreak
  }
  
  # Create the plot
  ggplot(data, aes(
    x = if(is_tiebreak) points_remaining_game_server else server_score,
    y = if(is_tiebreak) points_remaining_game_non_server else receiver_score,
    fill = control
  )) +
    geom_tile() +
    geom_text(
      aes(label = sprintf("%.0f%%", round(control * 100, 0))), 
      size = 6
    ) +
    scale_fill_gradient(
      low = "white", 
      high = color_high
    ) +
    scale_x_continuous(
      breaks = axis_limits,
      labels = tennis_labels,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = axis_limits,
      labels = tennis_labels,
      expand = c(0, 0)
    )+
    coord_equal() +
    labs(
      x = x_label,
      y = y_label,
      title = title
    ) +
    tennis_theme()+theme(
      # Re-add the tick marks
      axis.ticks.x = element_line(color = "black", size = 0.5),
      axis.ticks.y = element_line(color = "black", size = 0.5),
      # Adjust axis title margins to bring them closer to the plot
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(margin = margin(t = 2)), # Top margin for x-axis labels
      axis.text.y = element_text(margin = margin(r = 2))  # Right margin for y-axis labels
    )
}






plot_control <- create_control_heatmap_for_plot(control_stats, title = "Controllability", color_high = "#C1876B")

final_figure <- (
  plot_control | plot_spacer() | match_plot
) + 
  plot_layout(
    widths = c(1, .5, 2),
    guides = 'keep',
    heights = c(1, 1)
  ) 
ggsave(
  filename = file.path(save_path, "control_plot_single.png"),
  plot = plot_control,
  width = 8,
  height = 8,
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)
