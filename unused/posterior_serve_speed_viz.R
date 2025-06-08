# Function to extract serve speed parameters
csv_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_wDrun.csv')
csv_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2016_wDrun.csv')
sum_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary_2011.csv')
sum_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary_2016.csv')
stan_2011 <- readRDS('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/stan_data_2011_2015.rds')
params_2011 <- extract_serve_params(sum_2011)
params_2016 <- extract_serve_params(sum_2016)
get_cumulative_surprise <- function(summary_df) {
  # Extract cumulative surprise values from the Mean column where ...1 contains "cumulative_surprise["
  cumulative_surprise_indices <- grep("cumulative_surprise_server\\[", summary_df$...1)
  cumulative_surprise_player1_indices <- grep("cumulative_surprise_player1\\[", summary_df$...1)
  cumulative_surprise_player2_indices <- grep("cumulative_surprise_player2\\[", summary_df$...1)
  
  # Extract the indices and values
  indices <- as.numeric(gsub(".*\\[(\\d+)\\].*", "\\1", 
                             summary_df$...1[cumulative_surprise_indices]))
  indices2 <- as.numeric(gsub(".*\\[(\\d+)\\].*", "\\1", 
                              summary_df$...1[cumulative_surprise_player1_indices]))
  indices3 <- as.numeric(gsub(".*\\[(\\d+)\\].*", "\\1", 
                              summary_df$...1[cumulative_surprise_player2_indices]))
  
  
  values <- summary_df$Mean[cumulative_surprise_indices]
  value2 <- summary_df$Mean[cumulative_surprise_player1_indices]
  value3 <- summary_df$Mean[cumulative_surprise_player2_indices]
  
  # Create a data frame with index and value
  surprise_df <- data.frame(
    index = indices,
    cumulative_surprise_server = values,
    cumulative_surprise_player1=value2,
    cumulative_surprise_player2=value3
  )
  surprise_df <- surprise_df %>% dplyr::mutate(cumulative_surprise_non_server=if_else(cumulative_surprise_server==cumulative_surprise_player1,cumulative_surprise_player2,cumulative_surprise_player1))
  
  
  return(surprise_df)
}
add_cumulative <- function(full_2011,csv_data){
  # Get the cumulative surprise values
  surprise_df <- get_cumulative_surprise(full_2011)
  
  # Add the cumulative surprise column to csv_data
  # Assuming csv_data has a row index that matches with surprise_df$index
  csv_data$cumulative_surprise_server <- surprise_df$cumulative_surprise_server[match(1:nrow(csv_data), surprise_df$index)]
  csv_data$cumulative_surprise_non_server <- surprise_df$cumulative_surprise_non_server[match(1:nrow(csv_data), surprise_df$index)]
  
  csv_data$cumulative_surprise_player1 <- surprise_df$cumulative_surprise_player1[match(1:nrow(csv_data), surprise_df$index)]
  csv_data$cumulative_surprise_player2 <- surprise_df$cumulative_surprise_player2[match(1:nrow(csv_data), surprise_df$index)]
  return(csv_data)}

csv_2016 <- add_cumulative(sum_2016,csv_2016)
csv_2011 <- add_cumulative(sum_2011,csv_2011)

extract_serve_params <- function(summary_data) {
  # Get match parameters and determine number of matches
  match_params <- summary_data$Mean[grep("^beta_serve_speed_match\\[", summary_data$...1)]
  n_matches <- length(match_params) / 2
  
  params <- list(
    alpha_serve_speed = summary_data$Mean[summary_data$...1 == "alpha_serve_speed"],
    alpha_serve_speed_raw = summary_data$Mean[summary_data$...1 == "alpha_serve_speed_raw"],
    alpha_serve_speed_int = summary_data$Mean[summary_data$...1 == "alpha_serve_speed_int"],
    beta_serve_speed_player = summary_data$Mean[grep("^beta_serve_speed_player\\[", summary_data$...1)],
    beta_serve_speed_player_int_dev = summary_data$Mean[grep("^beta_serve_speed_player_int_dev\\[", summary_data$...1)],
    beta_serve_number_speed_plus_1 = summary_data$Mean[grep("^beta_serve_number_speed_plus_1\\[", summary_data$...1)],
    beta_serve_speed_match = matrix(match_params, nrow=n_matches, ncol=2, byrow=TRUE),
    beta_serve_speed_surprise_server=summary_data$Mean[grep("^beta_serve_speed_surprise_server\\[", summary_data$...1)],
    beta_serve_speed_surprise = summary_data$Mean[summary_data$...1 == "beta_serve_speed_surprise"],
    beta_serve_speed_surprise_int = summary_data$Mean[summary_data$...1 == "beta_serve_speed_surprise_int"],
    sigma_serve_speed = summary_data$Mean[summary_data$...1 == "sigma_serve_speed"]
  )
  return(params)
}
compute_serve_speed <- function(data, params, stan) {
  serve_indices <- stan$is_serve_speed_indices
  
  # Calculate standardization parameters from the actual serve speeds
  speed_mean <- mean(data$Speed_KMH[serve_indices], na.rm=TRUE)
  speed_sd <- sd(data$Speed_KMH[serve_indices], na.rm=TRUE)
  
  # Match indexing for matches
  match_ids <- data$match_id[serve_indices]
  unique_matches <- sort(unique(match_ids))
  match_index <- match(match_ids, unique_matches)
  
  # Compute standardized predictions
  predicted_speeds <- with(params, {
    alpha_serve_speed_raw + 
      (alpha_serve_speed_int + 
         beta_serve_speed_player_int_dev[data$Point_Server_id[serve_indices]]) * 
      data$p1_control_uniform[serve_indices] +
      beta_serve_speed_player[data$Point_Server_id[serve_indices]] +
      (beta_serve_number_speed_plus_1[data$Point_Server_id[serve_indices]] - 1) * 
      (data$ServeNumber[serve_indices] - 1.5) +
      beta_serve_speed_match[match_index, 1] * 
      (2 - data$PointServer[serve_indices]) +
      beta_serve_speed_match[match_index, 2] * 
      (data$PointServer[serve_indices] - 1)
  })
  
  # Add surprise effects if available
  if("cumulative_surprise_server" %in% names(data)) {
    predicted_speeds <- predicted_speeds +
      params$beta_serve_speed_surprise_server * 
      data$cumulative_surprise_server[serve_indices]
  }
  
  # Transform predictions back to km/h scale
  predicted_speeds_kmh <- predicted_speeds * speed_sd + speed_mean
  
  return(list(
    speeds = predicted_speeds_kmh,  # Return already transformed predictions
    indices = serve_indices
  ))
}
create_serve_calibration_data <- function(predicted_speeds, actual_speeds, n_bins = 20) {
  # Transform predicted speeds back to km/h scale
  if (sum(!is.na(predicted_speeds)) < 5) {
    return(NULL)  # Return NULL if not enough data
  }
  
#speed_sd <- sd(actual_speeds, na.rm=TRUE)
 # speed_mean <- mean(actual_speeds, na.rm=TRUE)
  predicted_speeds_kmh <- predicted_speeds# * speed_sd + speed_mean
  
  breaks <- seq(min(predicted_speeds_kmh, na.rm=TRUE), 
                max(predicted_speeds_kmh, na.rm=TRUE), 
                length.out = n_bins + 1)
  bins <- cut(predicted_speeds_kmh, breaks = breaks, include.lowest = TRUE)
  
  df <- data.frame(
    bin_min = breaks[-length(breaks)],
    bin_max = breaks[-1],
    predicted_speed = tapply(predicted_speeds_kmh, bins, mean, na.rm = TRUE),
    actual_speed = tapply(actual_speeds, bins, mean, na.rm = TRUE),
    n_points = tapply(actual_speeds, bins, length)
  )
  
  df <- df[!is.na(df$predicted_speed), ]
  df$se <- tapply(actual_speeds, bins, sd, na.rm = TRUE) / sqrt(df$n_points)
  df$ci_lower <- df$actual_speed - 1.96 * df$se
  df$ci_upper <- df$actual_speed + 1.96 * df$se
  
  return(df)
}

# First, create combined serve speed calibration plot
cal_serve_combined <- rbind(
  transform(cal_2011, period = "2011-2015"),
  transform(cal_2016, period = "2016-2022"))

  
create_combined_serve_plot <- function(data) {
  ggplot(data %>% dplyr::filter(n_points > 200), 
         aes(x = predicted_speed, y = actual_speed, color = period)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 1) +
    geom_point(aes(size = n_points), alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 2, alpha = 0.8) +
    scale_size_continuous(name = "Number of Serves",
                          range = c(2, 8),
                          breaks = c(100, 500, 1000, 5000),
                          labels = scales::comma) +
    scale_color_manual(
      name = "Period",
      values = c("2011-2015" = "#2171B5", "2016-2022" = '#FFB140'),
      guide = guide_legend(override.aes = list(size = 6, alpha = 0.7))
    ) +
    scale_x_continuous(breaks = seq(140, 200, by = 20)) +
    scale_y_continuous(breaks = seq(140, 200, by = 20)) +
    coord_equal() +
    labs(x = "Predicted Serve Speed (km/h)",
         y = "Actual Serve Speed (km/h)",
         title = "Serve Speed") +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 28, hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.position = c(1.15, 0.5),
      legend.justification = c(0, 0.5),
      legend.title = element_text(size = 28),
      legend.text = element_text(size = 24),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    )
}
  # Create the combined serve speed plot
  
# Function to create player-specific calibration

# Function to create calibration plot


# Compute predictions and create calibration data for main datasets
predictions_2011 <- compute_serve_speed(csv_2011, params_2011,stan_2011)
cal_2011 <- create_serve_calibration_data(
  predicted_speeds = predictions_2011$speeds,
  actual_speeds = csv_2011$Speed_KMH[predictions_2011$indices]
)
cal_2011$dataset <- "2011-2015"
# Convert list to data frame before plotting

#p_serve_combined <- create_combined_serve_plot(cal_serve_combined)




predictions_2016 <- compute_serve_speed(csv_2016, params_2016,stan_2016)
cal_2016 <- create_serve_calibration_data(
  predicted_speeds = predictions_2016$speeds,
  actual_speeds = csv_2016$Speed_KMH[predictions_2016$indices]
)
cal_2016$dataset <- "2016-2022"
p2 <- create_serve_cal_plot(cal_2016, "2016-2022",1)

# Create player-specific calibrations
players_2016 <- data.frame(
  id = c(1, 4, 6),
  name = c("Novak Djokovic", "Rafael Nadal", "Roger Federer")
)
players_2011 <- data.frame(
  id = c(1, 8, 3),
  name = c("Novak Djokovic", "Rafael Nadal", "Roger Federer")
)

pt_pop <- (p1 + p2)+
  plot_layout(guides = "collect") +
  theme(legend.position = "right")

# Function to create player-specific calibration data with improved range handling
create_player_calibration <- function(data, predictions, player_id, player_name) {
  # Filter for specific player
  serve_indices <- predictions$indices
  player_serves <- data$Point_Server_id[serve_indices] == player_id
  
  if (sum(player_serves) < 100) {
    return(NULL)
  }
  
  # Get the relevant speeds and serve numbers for this player
  predicted_speeds <- predictions$speeds[player_serves]
  actual_speeds <- data$Speed_KMH[serve_indices][player_serves]
  serve_numbers <- data$ServeNumber[serve_indices][player_serves]
  
  # Remove NA values and zeros
  valid_indices <- !is.na(predicted_speeds) & !is.na(actual_speeds) & actual_speeds > 0
  predicted_speeds <- predicted_speeds[valid_indices]
  actual_speeds <- actual_speeds[valid_indices]
  serve_numbers <- serve_numbers[valid_indices]
  
  if (length(predicted_speeds) < 100) {
    return(NULL)
  }
  
  # Transform predicted speeds to km/h scale
  speed_sd <- sd(actual_speeds, na.rm=TRUE)
  speed_mean <- mean(actual_speeds, na.rm=TRUE)
  predicted_speeds_kmh <- predicted_speeds# * speed_sd + speed_mean
  
  # Create bins for each serve number separately
  n_bins <- 10
  bin_stats_list <- list()
  
  for(serve_num in c(1, 2)) {
    serve_mask <- serve_numbers == serve_num
    if(sum(serve_mask) < 20) next
    
    pred_speeds_serve <- predicted_speeds_kmh[serve_mask]
    actual_speeds_serve <- actual_speeds[serve_mask]
    
    # Use quantile-based breaks to avoid extreme values
    breaks <- seq(
      quantile(pred_speeds_serve, 0.01, na.rm=TRUE),
      quantile(pred_speeds_serve, 0.99, na.rm=TRUE),
      length.out = n_bins + 1
    )
    bins <- cut(pred_speeds_serve, breaks = breaks, include.lowest = TRUE)
    
    # Calculate statistics for each bin
    bin_stats <- data.frame(
      bin = levels(bins),
      n_points = as.vector(table(bins)),
      serve_number = serve_num
    )
    
    bin_stats$bin_min <- breaks[-length(breaks)]
    bin_stats$bin_max <- breaks[-1]
    
    # Calculate means and standard errors
    bin_means <- tapply(pred_speeds_serve, bins, mean, na.rm = TRUE)
    bin_stats$predicted_speed <- as.vector(bin_means)
    
    actual_means <- tapply(actual_speeds_serve, bins, mean, na.rm = TRUE)
    bin_stats$actual_speed <- as.vector(actual_means)
    
    # Calculate standard errors
    bin_sds <- tapply(actual_speeds_serve, bins, sd, na.rm = TRUE)
    bin_stats$se <- as.vector(bin_sds) / sqrt(bin_stats$n_points)
    
    # Calculate confidence intervals
    bin_stats$ci_lower <- bin_stats$actual_speed - 1.96 * bin_stats$se
    bin_stats$ci_upper <- bin_stats$actual_speed + 1.96 * bin_stats$se
    
    bin_stats_list[[serve_num]] <- bin_stats
  }
  
  bin_stats_combined <- do.call(rbind, bin_stats_list)
  bin_stats_combined$player_name <- player_name
  
  return(bin_stats_combined)
}

# Create calibration data for each player in both datasets
player_cal_2011 <- list()
player_cal_2016 <- list()

# 2011-2015 dataset
for (i in 1:nrow(players_2011)) {
  cal_data <- create_player_calibration(
    csv_2011, 
    predictions_2011,
    players_2011$id[i],
    players_2011$name[i]
  )
  if (!is.null(cal_data)) {
    cal_data$dataset <- "2011-2015"
    player_cal_2011[[i]] <- cal_data
  }
}

# 2016-2022 dataset
for (i in 1:nrow(players_2016)) {
  cal_data <- create_player_calibration(
    csv_2016,
    predictions_2016,
    players_2016$id[i],
    players_2016$name[i]
  )
  if (!is.null(cal_data)) {
    cal_data$dataset <- "2016-2022"
    player_cal_2016[[i]] <- cal_data
  }
}

# Combine all calibration data
player_cal_all <- do.call(rbind, c(player_cal_2011, player_cal_2016))

# Modified plot function with thicker stroke for hollow points
create_player_cal_plot <- function(data, player_name) {
  data_filtered <- data %>% 
    dplyr::filter(player_name == !!player_name,
                  n_points >= 20)
  
  # Set wider range for axes (120-220 km/h should cover most tennis serves)
  speed_min <- 140
  speed_max <- 200
  
  ggplot(data_filtered, 
         aes(x = predicted_speed, y = actual_speed, 
             color = dataset, 
             fill = interaction(dataset, serve_number),
             shape = factor(serve_number))) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 1) +
    geom_point(aes(size = n_points), alpha = 0.8, stroke = 2.5) +  # Increased stroke width
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 2, alpha = 0.8) +
    scale_color_manual(values = c("2011-2015" = "#2171B5", "2016-2022" = "#FFB140")) +
    scale_fill_manual(values = c(
      "2011-2015.1" = "#2171B5", 
      "2016-2022.1" = "#FFB140",
      "2011-2015.2" = "white", 
      "2016-2022.2" = "white"
    )) +
    scale_shape_manual(values = c("1" = 19, "2" = 21),
                       labels = c("First Serve", "Second Serve"),
                       name = "Serve Type") +
    scale_size_continuous(name = "Number of Serves",
                          range = c(2, 8),
                          breaks = c(50, 100, 500, 1000),
                          labels = scales::comma) +
    coord_cartesian(xlim = c(speed_min, speed_max),
                    ylim = c(speed_min, speed_max)) +
    scale_x_continuous(breaks = seq(speed_min, speed_max, by = 20)) +
    scale_y_continuous(breaks = seq(speed_min, speed_max, by = 20)) +
    labs(x = "Predicted Serve Speed (km/h)",
         y = "Actual Serve Speed (km/h)") +
    ggtitle(player_name) +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 28, hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    )
}
# Create plots for each player
p_djokovic <- create_player_cal_plot(player_cal_all, "Novak Djokovic")
p_nadal <- create_player_cal_plot(player_cal_all, "Rafael Nadal")
p_federer <- create_player_cal_plot(player_cal_all, "Roger Federer")

# Create a shared legend
legend_data <- data.frame(
  predicted_speed = rep(170, 4),
  actual_speed = rep(170, 4),
  dataset = rep(c("2011-2015", "2016-2022"), each = 2),
  serve_number = rep(c(1, 2), 2),
  n_points = rep(c(100, 300, 500, 1000), 1)
)

legend_plot <- ggplot(legend_data, 
                      aes(x = predicted_speed, y = actual_speed, 
                          color = dataset,
                          fill = interaction(dataset, serve_number),
                          shape = factor(serve_number))) +
  geom_point(aes(size = n_points), stroke = 1.5) +
  guides(
    color = guide_legend("Time Period", override.aes = list(shape = 19, size = 4)),
    size = guide_legend("Number of Serves", override.aes = list(shape = 19)),
    shape = guide_legend("Serve Type", override.aes = list(size = 4)),
    fill = "none"
  ) +
  scale_color_manual(values = c("2011-2015" = "#2171B5", "2016-2022" = "#FFB140")) +
  scale_fill_manual(values = c(
    "2011-2015.1" = "#2171B5", 
    "2016-2022.1" = "#FFB140",
    "2011-2015.2" = "white", 
    "2016-2022.2" = "white"
  )) +
  scale_shape_manual(values = c("1" = 19, "2" = 21),
                     labels = c("First Serve", "Second Serve")) +
  scale_size_continuous(name = "Number of Serves",
                        range = c(2, 8),
                        breaks = c(100, 300, 500, 1000),
                        labels = scales::comma) +
  theme_void()

shared_legend <- get_legend(legend_plot +
                              theme(legend.text = element_text(size = 24),
                                    legend.title = element_text(size = 28)))

player_serve_plots <- p_djokovic + p_nadal + p_federer + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "none")

final_plot <- plot_grid(
  player_serve_plots, shared_legend,
  rel_widths = c(3, 0.7),
  nrow = 1
)


ggsave(
  filename = file.path(filepath, "serve_speed_players.png"),
  plot =final_plot ,
  width = 22,
  height = 10,
  bg='white')

ggsave(
  filename = file.path(filepath, "pp_serve.png"),
  plot = total_pop,
  width = 22,
  height = 12,
  bg='white')


pt_pop <- (p1 + p2)+
  plot_layout(guides = "collect") +
  theme(legend.position = "right")
total_pop <- pt_pop / sub_pop +
  plot_layout(heights = c(1, 0.8))

cal_serve_combined <- rbind(
  transform(cal_2011, period = "2011-2015"),
  transform(cal_2016, period = "2016-2022")
)
p_points_combined <- create_combined_point_plot(cal_combined)
p_serve_combined <- create_combined_serve_plot(cal_serve_combined)

# Top row: population-level plots
top_row <- p_points_combined + p_serve_combined +
  plot_layout(guides = "collect")
ggsave(
  filename = file.path(filepath, "group_level_ppc.png"),
  plot = top_row,
  width = 14,
  height = 9,
  bg = 'white'
)

