create_tennis_heatmap <- function(player_effects, effect_type = "point", show_y_axis = TRUE, control_2011) {
  # Create score mapping dataframe
  score_mapping <- data.frame(
    numeric_score = 0:3,
    tennis_score = c("0", "15", "30", "40")
  )
  
  # Create grid of all possible score combinations
  score_grid <- expand.grid(
    server_score = 0:3,
    receiver_score = 0:3
  )
  
  # Calculate control effect for each score combination
  effects <- data.frame(
    server_score = score_grid$server_score,
    receiver_score = score_grid$receiver_score
  )
  
  # Match the control values from control_2011
  effects <- merge(effects, control_2011, 
                   by.x = c("server_score", "receiver_score"),
                   by.y = c("server_score", "receiver_score"))
  
  # Calculate effect based on type
  if(effect_type == "point") {
    effects$effect <- player_effects$beta_surprise + 
      player_effects$beta_surprise_int * effects$control
    effect_title <- "Winning Next Point"
  } else {
    effects$effect <- player_effects$beta_serve_surprise + 
      player_effects$beta_serve_surprise_int * effects$control
    effect_title <- "Serve Speed"
  }
  
  # Add tennis scores for display
  effects$server_display <- score_mapping$tennis_score[match(effects$server_score, 
                                                             score_mapping$numeric_score)]
  effects$receiver_display <- score_mapping$tennis_score[match(effects$receiver_score, 
                                                               score_mapping$numeric_score)]
  
  # Calculate limits based on data
  max_abs <- max(abs(effects$effect))
  limits <- c(-max_abs, max_abs)
  midpoint <- 0
  
  # Create the plot
  p <- ggplot(effects, aes(x = server_display, y = receiver_display)) +
    geom_tile(aes(fill = effect)) +
    geom_text(aes(label = sprintf("%.2f", round(effect, 2))), 
              color = 'black', size = 7) +
    scale_fill_gradient2(
      name = effect_title,
      low = "#4575B4",     
      mid = "#F7F7F7",     
      high = "#D73027",    
      midpoint = midpoint,
      limits = limits
    ) +
    scale_x_discrete(name = "Server score") +
    scale_y_discrete(name = if(show_y_axis) "Opponent score" else "") +
    ggtitle(player_effects$player_name) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      legend.position = "right",
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 16),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    )
  
  return(p)
}
create_mcmc_tennis_heatmap <- function(sum_data, control_2011, type = "point", period = "2011-2015") {
  # Extract relevant parameters based on type
  if(type == "point") {
    beta_surprise <- sum_data$Mean[sum_data$...1 == "beta_surprise"]
    beta_surprise_int <- sum_data$Mean[sum_data$...1 == "beta_surprise_int"]
  } else {
    beta_surprise <- sum_data$Mean[sum_data$...1 == "beta_serve_speed_surprise"]
    beta_surprise_int <- sum_data$Mean[sum_data$...1 == "beta_serve_speed_surprise_int"]
  }
  
  # Create score mapping dataframe
  score_mapping <- data.frame(
    numeric_score = 0:3,
    tennis_score = c("0", "15", "30", "40")
  )
  
  # Create grid of all possible score combinations
  score_grid <- expand.grid(
    server_score = 0:3,
    receiver_score = 0:3
  )
  
  # Match the control values from control_2011
  effects <- merge(score_grid, control_2011, 
                   by.x = c("server_score", "receiver_score"),
                   by.y = c("server_score", "receiver_score"))
  
  # Calculate effects
  effects$effect <- beta_surprise + beta_surprise_int * effects$control
  
  # Add tennis scores for display
  effects$server_display <- score_mapping$tennis_score[match(effects$server_score, 
                                                             score_mapping$numeric_score)]
  effects$receiver_display <- score_mapping$tennis_score[match(effects$receiver_score, 
                                                               score_mapping$numeric_score)]
  
  # Define color scheme based on period
  if(period == "2011-2015") {
    fill_colors <- scale_fill_gradient2(
      low = "grey70",
      mid = "white",
      high = "#2B5A70",
      midpoint = -0.05,
      limits = c(-0.06, NA)
    )
  } else {
    fill_colors <- scale_fill_gradient2(
      low = "grey70",
      mid = "white",
      high = "#8C4F2B",
      midpoint = -0.05,
      limits = c(-0.06, NA)
    )
  }
  
  # Create the plot
  p <- ggplot(effects, aes(x = server_display, y = receiver_display)) +
    geom_tile(aes(fill = effect)) +
    geom_text(aes(label = sprintf("%.2f", round(effect, 2))), 
              color = 'black', size = 7) +
    fill_colors +
    scale_x_discrete(name = "Server score") +
    scale_y_discrete(name = "Opponent score") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      axis.text = element_text(size=24),
      axis.title = element_text(size=26, face='plain'),
      plot.margin = margin(5, 5, 5, 5),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    ) +
    guides(fill = "none")
  
  return(p)
}
sum_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary.csv')
sum_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary_2016.csv')


control_2011 <- structure(list(server_score = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 
                                                2, 2, 3, 3, 3, 3), receiver_score = c(0L, 1L, 2L, 3L, 0L, 1L, 
                                                                                      2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L), control = c(3.12335906745932, 
                                                                                                                                           3.44958462106214, 2.96925152241357, 1.17835248181394, 2.19194689170917, 
                                                                                                                                           2.82076808399714, 2.90959346184801, 1.53654600454688, 1.19738885557178, 
                                                                                                                                           1.84670368552017, 2.87148854001701, 1.87989350671492, 0.191945652408413, 
                                                                                                                                           0.392891235716067, 0.75629971905, 2.87140558707649)), row.names = c(NA, 
                                                                                                                                                                                                               -16L), class = c("tbl_df", "tbl", "data.frame"))

control_2011 <- structure(list(
  server_score = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), 
  receiver_score = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L), 
  control = c(2.98, 3.30, 2.78, 1.03, 2.07, 2.72, 2.79, 1.41, 1.12, 1.78, 2.79, 1.78, 0.18, 0.38, 0.75, 2.81)
), 
row.names = c(NA, -16L), 
class = c("tbl_df", "tbl", "data.frame"))

# To verify the data matches the heatmap, you can create a visualization with:


control_2016 <- structure(list(server_score = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 
                                                2, 2, 3, 3, 3, 3), receiver_score = c(0L, 1L, 2L, 3L, 0L, 1L, 
                                                                                      2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L), control = c(3.03881341765809, 
                                                                                                                                           3.43443456086246, 3.02332379865473, 1.23851064657218, 2.10223909117661, 
                                                                                                                                           2.77554129844574, 2.93152094629125, 1.60071833751712, 1.12865824237471, 
                                                                                                                                           1.78937418603829, 2.82074647563133, 1.93772012284317, 0.174668164547409, 
                                                                                                                                           0.366891482769779, 0.724275175325183, 2.82296861546947)), row.names = c(NA, 
                                                                                                                                                                                                                   -16L), class = c("tbl_df", "tbl", "data.frame"))


# Usage would be:
# Create all heatmaps
hmap_point_2011 <- create_mcmc_tennis_heatmap(sum_2011, control_2011, "point", "2011-2015")
hmap_point_2016 <- create_mcmc_tennis_heatmap(sum_2016, control_2011, "point", "2016-2022")
hmap_serve_2011 <- create_mcmc_tennis_heatmap(sum_2011, control_2011, "speed", "2011-2015")
hmap_serve_2016 <- create_mcmc_tennis_heatmap(sum_2016, control_2011, "speed", "2016-2022")

require(patchwork)
require(cowplot)
create_combined_visualization <- function(sum_2011, sum_2016, save_path) {
  # Helper function to create single posterior distribution plot

  create_single_posterior <- function(summary_df, effect_type, period) {
    if(effect_type == "point") {
      param_raw <- "beta_surprise_raw"
      param_int <- "beta_surprise_int_raw"
    } else {
      param_raw <- "beta_serve_speed_surprise_raw"
      param_int <- "beta_serve_speed_surprise_int_raw"
    }
    
    # Colors based on period
    if(period=="2011-2015"){
      colors <- c("#2B5A70","#74A4BC" )  # light blue, dark blue
      color_mapping <- setNames(rev(colors), c("Main", "Interaction"))  # added rev()
      
       } else {
      colors <- c("#C1876B", "#8C4F2B")  # light brown, dark brown
      color_mapping <- setNames(colors, c("Main", "Interaction"))
      
      }
    
    # Function to calculate density for one parameter
    get_density_df <- function(data, param, is_interaction = FALSE) {
      param_data <- data %>% 
        dplyr::filter(...1 == param) %>%
        dplyr::select(Mean, StdDev)
      
      if(is_interaction) {
        param_data$Mean <- param_data$Mean
        param_data$StdDev <- param_data$StdDev
      }
      
      x_vals <- seq(-.5, 1.5, length.out = 5000)
      density_vals <- dnorm(x_vals, mean = param_data$Mean, sd = param_data$StdDev)
      
      data.frame(
        x = x_vals,
        y = density_vals,
        Parameter = if(is_interaction) "Interaction" else "Main"
      )
    }
    
    density_df_raw <- get_density_df(summary_df, param_raw, FALSE)
    density_df_int <- get_density_df(summary_df, param_int, TRUE)
    density_df <- rbind(density_df_raw, density_df_int)
    
   # names(colors) <- c(param_raw, param_int)  # Keep same color mapping (light=main, dark=interaction)
    # Reorder factor levels to control legend order
    density_df$Parameter <- factor(density_df$Parameter, 
                                   levels = c("Main", "Interaction"))
    
    p <- ggplot(density_df, aes(x = x, y = y, fill = Parameter)) +
      geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.7) +
      geom_vline(xintercept = 0, color = "black", alpha = 0.5) +
      scale_fill_manual(
        values = color_mapping,
        name = NULL,
        labels = c(
          "Main effect",
          "Interaction with controllability"
        )
      ) +
      labs(y = '', x = '') +
      xlim(-.30, 1.25) +
      scale_x_continuous(limits = c(-.3, 1.25), expand = c(0, 0)) +  # expand = c(0,0) removes padding
      coord_cartesian(clip = "on") +
      theme_minimal() +
      theme(
        text = element_text(size = 22),
        axis.title = element_text(size = 32),
        axis.text = element_text(size = 22),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 30),
        aspect.ratio = 0.5,
        plot.margin = unit(c(0,0,0,0), "cm")  # Add this line to remove plot margins
      )
    return(p)
  }
    modified_heatmap <- function(effects, type, period) {
    if(period == "2011-2015") {
      fill_colors <- scale_fill_gradient2(
        low = "grey70",
        mid = "white",
        high = "#2B5A70",
        midpoint = -0.05,
        limits = c(-0.06, NA)
      )
    } else {
      fill_colors <- scale_fill_gradient2(
        low = "grey70",
        mid = "white",
        high = "#8C4F2B",
        midpoint = -0.05,
        limits = c(-0.06, NA)
      )
    }
    
    create_player_heatmap(effects, type) + 
      fill_colors +
      theme(
        legend.position = "none",
        plot.title = element_blank(),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26, face='plain'),
        plot.margin = margin(5, 5, 5, 5)
      ) +
      guides(fill = "none")
  }
    modified_heatmap <- function(effects, type, period, control_2011) {
      # Define color scheme based on period
      if(period == "2011-2015") {
        fill_colors <- scale_fill_gradient2(
          low = "grey70",
          mid = "white",
          high = "#2B5A70",
          midpoint = -0.05,
          limits = c(-0.06, NA)
        )
      } else {
        fill_colors <- scale_fill_gradient2(
          low = "grey70",
          mid = "white",
          high = "#8C4F2B",
          midpoint = -0.05,
          limits = c(-0.06, NA)
        )
      }
      
      # Create base heatmap using create_tennis_heatmap
      base_plot <- create_tennis_heatmap(effects, type, TRUE, control_2011)
      
      # Add modifications
      final_plot <- base_plot +
        fill_colors +
        theme(
          legend.position = "none",
          plot.title = element_blank(),
          axis.text = element_text(size=24),
          axis.title = element_text(size=26, face='plain'),
          plot.margin = margin(5, 5, 5, 5)
        ) +
        guides(fill = "none")
      
      return(final_plot)
    }
  # Create a separate legend plot
  legend_plot <- create_single_posterior(sum_2011, "point", "2011-2015") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 27),
          legend.title = element_blank())
  legend <- get_legend(legend_plot)
  
  # Create all posterior plots
  p_point_2011 <- create_single_posterior(sum_2011, "point", "2011-2015")
  p_point_2016 <- create_single_posterior(sum_2016, "point", "2016-2022")
  p_serve_2011 <- create_single_posterior(sum_2011, "speed", "2011-2015")
  p_serve_2016 <- create_single_posterior(sum_2016, "speed", "2016-2022")
  
  # # Create all heatmaps
  # hmap_point_2011 <- modified_heatmap(effects_2011[[1]], "point", "2011-2015")
  # hmap_point_2016 <- modified_heatmap(effects_2016[[1]], "point", "2016-2022")
  # hmap_serve_2011 <- modified_heatmap(effects_2011[[1]], "speed", "2011-2015")
  # hmap_serve_2016 <- modified_heatmap(effects_2016[[1]], "speed", "2016-2022")
  # 
  # Apply consistent theme modifications for posterior plots
  posterior_theme_mods <- function(p) {
    p + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      plot.margin = margin(0, 0, -5, 0)
    )
  }
  
  # Apply consistent theme modifications for heatmaps
  heatmap_theme_mods <- function(h) {
    h + theme(plot.margin = margin(-5, 0, 35, 0))
  }
  
  # Apply themes
  p_point_2011 <- posterior_theme_mods(p_point_2011)
  p_point_2016 <- posterior_theme_mods(p_point_2016)
  p_serve_2011 <- posterior_theme_mods(p_serve_2011)
  p_serve_2016 <- posterior_theme_mods(p_serve_2016)
  
  hmap_point_2011 <- heatmap_theme_mods(hmap_point_2011)
  hmap_point_2016 <- heatmap_theme_mods(hmap_point_2016)
  hmap_serve_2011 <- heatmap_theme_mods(hmap_serve_2011)
  hmap_serve_2016 <- heatmap_theme_mods(hmap_serve_2016)
  
  # Create units with consistent width ratios and reduced spacing
  create_unit <- function(posterior, heatmap) {
    (posterior + 
       theme(plot.margin = margin(5, 0, -8, 0))) /
      (heatmap + 
         theme(plot.margin = margin(5, 0, -8, 0))) +
      plot_layout(heights = c(1, 2))
  }
  
  # Create all units
  point_2011_unit <- create_unit(p_point_2011, hmap_point_2011)
  point_2016_unit <- create_unit(p_point_2016, hmap_point_2016)
  serve_2011_unit <- create_unit(p_serve_2011, hmap_serve_2011)
  serve_2016_unit <- create_unit(p_serve_2016, hmap_serve_2016)
  
  # Create column titles with minimal spacing
  title_point <- ggdraw() + 
    draw_label("Effect on p(win point)", 
               fontface = 'bold', 
               size = 30,
               x = 0.5,  # Center horizontally
               y = 0.5) + # Center vertically
    theme(plot.margin = margin(-5, 0, -5, 0))
  
  title_serve <- ggdraw() + 
    draw_label("Effect on serve speed", 
               fontface = 'bold', 
               size = 30,
               x = 0.5,  # Center horizontally
               y = 0.5) + # Center vertically
    theme(plot.margin = margin(-5, 0, -5, 0))
  
  # Create a separate legend plot and save it
  legend_plot_blue <- create_single_posterior(sum_2011, "point", "2011-2015") +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 27),
      legend.title = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  legend_plot_brown <- create_single_posterior(sum_2016, "point", "2016-2022") +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 27),
      legend.title = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  # Extract and save legend separately
  legend_1 <- get_legend(legend_plot_blue)
  legend_2 <- get_legend(legend_plot_brown)
  legend_only_brown <- wrap_elements(legend_2)
  
  legend_only <- wrap_elements(legend_1)
  ggsave(paste0(save_path, "/legend.svg"), 
         legend_only, 
         width = 8,
         height = 2,
         device = "svg")
  ggsave(paste0(save_path, "/legend_brown.svg"), 
         legend_only_brown, 
         width = 8,
         height = 2,
         device = "svg")
  
  
  # Rest of the visualization without legend
  annotation_2011 <- ggdraw() + 
    draw_label("2011-2015", x = 0.75, y = 0.35, hjust = 0.5, vjust=.5,
               fontface = 'bold', size = 34, angle = 90) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  annotation_2016 <- ggdraw() + 
    draw_label("2016-2022", x = 0.75, y = 0.35, hjust = 0.5, vjust=.5, 
               fontface = 'bold', size = 34, angle = 90) +
    theme(plot.margin = margin(0, 0, 0, 0))
  require(patchwork)
  # Create the rows with annotations
  row_2011 <- (annotation_2011 | point_2011_unit | serve_2011_unit) + 
    plot_layout(widths = c(0.1, 1,1)) &
    theme(plot.margin = margin(0, 0, 0, 0))
  
  row_2016 <- (annotation_2016 | point_2016_unit | serve_2016_unit) + 
    plot_layout(widths = c(0.1, 1,1)) &
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Create the column titles row with proper spacing and centering
  col_titles <- (plot_spacer() | title_point | title_serve) +
    plot_layout(widths = c(0.1, 1, 1))
  
  # Combine everything without the legend
  combined_plot <- (
    col_titles /
      plot_spacer()/
      row_2011 /
      plot_spacer() /
      row_2016
  ) + 
    plot_layout(heights = c(0.05, 0.1, 1, 0.05, 1)) +
    plot_annotation(
      theme = theme(
        plot.margin = margin(10, 10, 10, 10)
      )
    )
  
  # Save the main visualization
  ggsave(paste0(save_path, "/combined_visualization.svg"), 
         combined_plot, 
         width = 16,
         height = 17,
         dpi = 400)
  
  
 
  
  return(combined_plot)
}
combined_viz <- create_combined_visualization(
  sum_2011, 
  sum_2016, 
  save_path='/Users/levisolomyak/Desktop/02Phd/tennis_project/paper_figures/05Tennis_results/'
)