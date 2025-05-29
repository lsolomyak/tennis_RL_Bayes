# Required libraries
library(tidyverse)
library(patchwork)
library(cowplot)

# Read data
 csv_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_wDrun.csv')
csv_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2016_wDrun.csv')
sum_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary_2011.csv') 
sum_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/control_uniform/full_summary_2016.csv')
 stan_2011 <- readRDS('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/stan_data_2011_2015.rds')
 params_2011 <- extract_serve_params(sum_2011)
 params_2016 <- extract_serve_params(sum_2016)
 clean_tennis_data <- function(data) {
  cleaned_data <- data %>%
    # Remove 0 or invalid speeds
    dplyr::filter(!is.na(Speed_KMH) & Speed_KMH > 0) %>%
    # Remove any obvious outliers (e.g., impossibly high speeds)
    dplyr::filter(Speed_KMH < 250)  # No serve has ever been recorded above 250 km/h
  
  print(paste("Original rows:", nrow(data)))
  print(paste("Cleaned rows:", nrow(cleaned_data)))
  print("Speed summary after cleaning:")
  print(summary(cleaned_data$Speed_KMH))
  
  return(cleaned_data)
}

# Apply cleaning to both datasets
csv_2011_serve <- clean_tennis_data(csv_2011)
csv_2016_serve <- clean_tennis_data(csv_2016)
# Output directory
filepath <- '/Users/levisolomyak/Desktop/02Phd/tennis_project/paper_figures/05Tennis_results/'

###########################################
# PART 1: POINT WIN PROBABILITY ANALYSIS
###########################################
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

csv_2011 <- add_cumulative(sum_2011,csv_2011)

csv_2016 <- add_cumulative(sum_2016,csv_2016)

# Function to compute win probabilities
compute_win_prob <- function(data, sum_2016) {
  # Extract parameters
  params <- list(
    alpha = sum_2016$Mean[sum_2016$...1 == "alpha_raw"],
    beta_player = sum_2016$Mean[grep("^beta_player\\[", sum_2016$...1)],
    beta_match = sum_2016$Mean[grep("^beta_match\\[", sum_2016$...1)],
    beta_player_int_dev = sum_2016$Mean[grep("^beta_player_int_dev\\[", sum_2016$...1)],
    beta_surprise_player1 = sum_2016$Mean[grep("^beta_surprise_player1\\[", sum_2016$...1)],
    beta_surprise_player2 = sum_2016$Mean[grep("^beta_surprise_player2\\[", sum_2016$...1)]
    # beta_surprise_server = sum_2016$Mean[grep("^beta_surprise_server\\[", sum_2016$...1)]
    # beta_surprise_non_server = sum_2016$Mean[grep("^beta_surprise_non_server\\[", sum_2016$...1)]
    # 
    )
  
  # Compute linear predictor
  linear_pred <- params$alpha + 
    params$beta_player[data$Point_Server_id] - 
    params$beta_player[data$Point_non_Server_id] +
    params$beta_match[as.numeric(as.factor(data$match_id))] * data$player1_served +
    (params$beta_player_int_dev[data$Point_Server_id] - 
       params$beta_player_int_dev[data$Point_non_Server_id]) * data$progress_1minus2_abs
  
  if("cumulative_surprise_server" %in% names(data)) {
    linear_pred <-  linear_pred +
       params$beta_surprise_player1 * data$cumulative_surprise_player1 +
       params$beta_surprise_player2 * data$cumulative_surprise_player2
  }
  
  # Convert to probability
  plogis(linear_pred)
}
compute_player_win_prob <- function(data, sum_2016, player_id) {
  # Print debug info
  cat("Player ID:", player_id, "\n")
  cat("Number of total rows in data:", nrow(data), "\n")
  
  # Extract base parameters 
  params <- list(
    alpha = sum_2016$Mean[sum_2016$...1 == "alpha_raw"],
    beta_player = sum_2016$Mean[grep("^beta_player\\[", sum_2016$...1)],
    beta_match = sum_2016$Mean[grep("^beta_match\\[", sum_2016$...1)],
    beta_player_int_dev = sum_2016$Mean[grep("^beta_player_int_dev\\[", sum_2016$...1)]
  )
  
  # Filter data for serving scenarios
  serving_data <- subset(data, Point_Server_id == player_id)
  cat("Number of serving points:", nrow(serving_data), "\n")
  
  # Filter data for receiving scenarios
  receiving_data <- subset(data, Point_non_Server_id == player_id)
  cat("Number of receiving points:", nrow(receiving_data), "\n")
  
  # Compute probabilities when serving
  serving_prob <- NULL
  if(nrow(serving_data) > 0) {
    # Create match factor for proper indexing
    match_factor <- as.numeric(as.factor(serving_data$match_id))
    cat("Number of unique matches when serving:", length(unique(match_factor)), "\n")
    
    # Extract surprise parameters for just serving points
    serving_surprise_params <- list(
      beta_surprise_player1 = sum_2016$Mean[grep("^beta_surprise_player1\\[", sum_2016$...1)][1:nrow(serving_data)],
      beta_surprise_player2 = sum_2016$Mean[grep("^beta_surprise_player2\\[", sum_2016$...1)][1:nrow(serving_data)]
    )
    
    serving_linear_pred <- params$alpha + 
      params$beta_player[serving_data$Point_Server_id] - 
      params$beta_player[serving_data$Point_non_Server_id] +
      params$beta_match[match_factor] * serving_data$player1_served +
      (params$beta_player_int_dev[serving_data$Point_Server_id] - 
         params$beta_player_int_dev[serving_data$Point_non_Server_id]) * serving_data$progress_1minus2_abs
    
    if("cumulative_surprise_server" %in% names(serving_data)) {
      serving_linear_pred <- serving_linear_pred +
        serving_surprise_params$beta_surprise_player1 * serving_data$cumulative_surprise_player1 +
        serving_surprise_params$beta_surprise_player2 * serving_data$cumulative_surprise_player2
    }
    
    serving_prob <- plogis(serving_linear_pred)
  }
  
  # Compute probabilities when receiving
  receiving_prob <- NULL
  if(nrow(receiving_data) > 0) {
    # Create match factor for proper indexing
    match_factor <- as.numeric(as.factor(receiving_data$match_id))
    
    # Extract surprise parameters for just receiving points
    receiving_surprise_params <- list(
      beta_surprise_player1 = sum_2016$Mean[grep("^beta_surprise_player1\\[", sum_2016$...1)][1:nrow(receiving_data)],
      beta_surprise_player2 = sum_2016$Mean[grep("^beta_surprise_player2\\[", sum_2016$...1)][1:nrow(receiving_data)]
    )
    
    receiving_linear_pred <- params$alpha + 
      params$beta_player[receiving_data$Point_Server_id] - 
      params$beta_player[receiving_data$Point_non_Server_id] +
      params$beta_match[match_factor] * receiving_data$player1_served +
      (params$beta_player_int_dev[receiving_data$Point_Server_id] - 
         params$beta_player_int_dev[receiving_data$Point_non_Server_id]) * receiving_data$progress_1minus2_abs
    
    if("cumulative_surprise_server" %in% names(receiving_data)) {
      receiving_linear_pred <- receiving_linear_pred +
        receiving_surprise_params$beta_surprise_player1 * receiving_data$cumulative_surprise_player1 +
        receiving_surprise_params$beta_surprise_player2 * receiving_data$cumulative_surprise_player2
    }
    
    receiving_prob <- plogis(receiving_linear_pred)
  }
  
  # Create results dataframes
  serving_results <- if(nrow(serving_data) > 0) {
    data.frame(
      probability = serving_prob,
      actual = serving_data$served_and_scored,
      progress = serving_data$progress_1minus2_abs,
      match_id = serving_data$match_id
    )
  } else {
    NULL
  }
  
  receiving_results <- if(nrow(receiving_data) > 0) {
    data.frame(
      probability = receiving_prob,
      actual = ifelse(receiving_data$Point_Server_score == 0, 1, 0),
      progress = receiving_data$progress_1minus2_abs,
      match_id = receiving_data$match_id
    )
  } else {
    NULL
  }
  
  # Return results
  return(list(
    serving = list(
      data = serving_results,
      mean_prob = mean(serving_prob, na.rm = TRUE)
    ),
    receiving = list(
      data = receiving_results,
      mean_prob = mean(receiving_prob, na.rm = TRUE)
    ),
    player_beta = params$beta_player[player_id],
    summary = list(
      n_serving_points = nrow(serving_data),
      n_receiving_points = nrow(receiving_data),
      mean_serving_prob = mean(serving_prob, na.rm = TRUE),
      mean_receiving_prob = mean(receiving_prob, na.rm = TRUE)
    )
  ))
}
compute_player_win_prob <- function(data, sum_2016, player_id) {
  # Print debug info
  cat("Player ID:", player_id, "\n")
  cat("Number of total rows in data:", nrow(data), "\n")
  
  # Extract base parameters 
  params <- list(
    alpha = sum_2016$Mean[sum_2016$...1 == "alpha_raw"],
    beta_player = sum_2016$Mean[grep("^beta_player\\[", sum_2016$...1)],
    beta_match = sum_2016$Mean[grep("^beta_match\\[", sum_2016$...1)],
    beta_player_int_dev = sum_2016$Mean[grep("^beta_player_int_dev\\[", sum_2016$...1)]
  )
  
  # Filter data for serving scenarios
  serving_data <- subset(data, Point_Server_id == player_id)
  cat("Number of serving points:", nrow(serving_data), "\n")
  
  # Filter data for receiving scenarios
  receiving_data <- subset(data, Point_non_Server_id == player_id)
  cat("Number of receiving points:", nrow(receiving_data), "\n")
  
  # Compute probabilities when serving
  serving_prob <- NULL
  if(nrow(serving_data) > 0) {
    match_factor <- as.numeric(as.factor(serving_data$match_id))
    cat("Number of unique matches when serving:", length(unique(match_factor)), "\n")
    
    serving_surprise_params <- list(
      beta_surprise_player1 = sum_2016$Mean[grep("^beta_surprise_player1\\[", sum_2016$...1)][1:nrow(serving_data)],
      beta_surprise_player2 = sum_2016$Mean[grep("^beta_surprise_player2\\[", sum_2016$...1)][1:nrow(serving_data)]
    )
    
    serving_linear_pred <- params$alpha + 
      params$beta_player[serving_data$Point_Server_id] - 
      params$beta_player[serving_data$Point_non_Server_id] +
      params$beta_match[match_factor] * serving_data$player1_served +
      (params$beta_player_int_dev[serving_data$Point_Server_id] - 
         params$beta_player_int_dev[serving_data$Point_non_Server_id]) * serving_data$progress_1minus2_abs
    
    if("cumulative_surprise_server" %in% names(serving_data)) {
      serving_linear_pred <- serving_linear_pred +
        serving_surprise_params$beta_surprise_player1 * serving_data$cumulative_surprise_player1 +
        serving_surprise_params$beta_surprise_player2 * serving_data$cumulative_surprise_player2
    }
    
    serving_prob <- plogis(serving_linear_pred)
  }
  
  # Compute probabilities when receiving
  receiving_prob <- NULL
  if(nrow(receiving_data) > 0) {
    match_factor <- as.numeric(as.factor(receiving_data$match_id))
    
    receiving_surprise_params <- list(
      beta_surprise_player1 = sum_2016$Mean[grep("^beta_surprise_player1\\[", sum_2016$...1)][1:nrow(receiving_data)],
      beta_surprise_player2 = sum_2016$Mean[grep("^beta_surprise_player2\\[", sum_2016$...1)][1:nrow(receiving_data)]
    )
    
    # For receiving, we'll compute server's probability and then take complement
    receiving_linear_pred <- params$alpha + 
      params$beta_player[receiving_data$Point_Server_id] - 
      params$beta_player[receiving_data$Point_non_Server_id] +
      params$beta_match[match_factor] * receiving_data$player1_served +
      (params$beta_player_int_dev[receiving_data$Point_Server_id] - 
         params$beta_player_int_dev[receiving_data$Point_non_Server_id]) * receiving_data$progress_1minus2_abs
    
    if("cumulative_surprise_server" %in% names(receiving_data)) {
      receiving_linear_pred <- receiving_linear_pred +
        receiving_surprise_params$beta_surprise_player1 * receiving_data$cumulative_surprise_player1 +
        receiving_surprise_params$beta_surprise_player2 * receiving_data$cumulative_surprise_player2
    }
    
    # Take complement of probability since we want receiver's perspective
    receiving_prob <- 1 - plogis(receiving_linear_pred)
  }
  
  # Validation checks
  cat("\nValidation Checks:\n")
  if(!is.null(serving_prob)) {
    cat("Serving probabilities - Mean:", mean(serving_prob), 
        "Range:", range(serving_prob)[1], "to", range(serving_prob)[2], "\n")
  }
  if(!is.null(receiving_prob)) {
    cat("Receiving probabilities - Mean:", mean(receiving_prob),
        "Range:", range(receiving_prob)[1], "to", range(receiving_prob)[2], "\n")
  }
  
  # Create results dataframes
  serving_results <- if(nrow(serving_data) > 0) {
    data.frame(
      probability = serving_prob,
      actual = serving_data$served_and_scored,
      progress = serving_data$progress_1minus2_abs,
      match_id = serving_data$match_id
    )
  } else {
    NULL
  }
  
  receiving_results <- if(nrow(receiving_data) > 0) {
    data.frame(
      probability = receiving_prob,
      actual = ifelse(receiving_data$Point_Server_score == 0, 1, 0),
      progress = receiving_data$progress_1minus2_abs,
      match_id = receiving_data$match_id
    )
  } else {
    NULL
  }
  
  # Return results with added validation info
  return(list(
    serving = list(
      data = serving_results,
      mean_prob = mean(serving_prob, na.rm = TRUE)
    ),
    receiving = list(
      data = receiving_results,
      mean_prob = mean(receiving_prob, na.rm = TRUE)
    ),
    player_beta = params$beta_player[player_id],
    summary = list(
      n_serving_points = nrow(serving_data),
      n_receiving_points = nrow(receiving_data),
      mean_serving_prob = mean(serving_prob, na.rm = TRUE),
      mean_receiving_prob = mean(receiving_prob, na.rm = TRUE)
    ),
    validation = list(
      n_matches_serving = if(!is.null(serving_data)) length(unique(serving_data$match_id)) else 0,
      n_matches_receiving = if(!is.null(receiving_data)) length(unique(receiving_data$match_id)) else 0
    )
  ))
}
# Create calibration data function for points
create_calibration_data <- function(predicted_probs, actual_outcomes, n_bins = 20) {
  # Check if there's enough data
  if (sum(!is.na(predicted_probs)) < 5) {
    return(NULL)
  }
  
  # Create breaks based on the range of predicted probabilities
  breaks <- seq(min(predicted_probs, na.rm=TRUE),
                max(predicted_probs, na.rm=TRUE),
                length.out = n_bins + 1)
  
  bins <- cut(predicted_probs, breaks = breaks, include.lowest = TRUE)
  
  df <- data.frame(
    bin_min = breaks[-length(breaks)],
    bin_max = breaks[-1],
    predicted_prob = tapply(predicted_probs, bins, mean, na.rm = TRUE),
    actual_prob = tapply(actual_outcomes, bins, mean, na.rm = TRUE),
    n_points = tapply(actual_outcomes, bins, length)
  )
  
  df <- df[!is.na(df$predicted_prob), ]
  
  actual_prob_original <- df$actual_prob 
  df$se <- sqrt(actual_prob_original * (1-actual_prob_original) / df$n_points)
  df$ci_lower <- pmax(0, actual_prob_original - 1.96 * df$se)
  df$ci_upper <- pmin(1, actual_prob_original + 1.96 * df$se)
  
  return(df)
}
create_calibration_data_for_player <- function(predicted_probs, actual_outcomes, min_points_per_bin = 50) {
  # Check if there's enough data
  if (sum(!is.na(predicted_probs)) < min_points_per_bin) {
    return(NULL)
  }
  
  # Sort data by predicted probabilities
  sorted_data <- data.frame(
    pred = predicted_probs,
    actual = actual_outcomes
  ) %>%
    dplyr::arrange(pred)
  
  # Calculate total number of points
  n_total <- nrow(sorted_data)
  
  # Calculate number of bins based on minimum points per bin
  n_bins <- min(floor(n_total / min_points_per_bin), 6)  # Cap at 10 bins maximum
  
  # Create adaptive bins with roughly equal number of points
  bin_indices <- round(seq(1, n_total, length.out = n_bins + 1))
  
  # Initialize results dataframe
  results <- data.frame(
    bin_min = numeric(n_bins),
    bin_max = numeric(n_bins),
    predicted_prob = numeric(n_bins),
    actual_prob = numeric(n_bins),
    n_points = numeric(n_bins)
  )
  
  # Fill in results for each bin
  for(i in 1:n_bins) {
    bin_data <- sorted_data[bin_indices[i]:bin_indices[i+1], ]
    
    results$bin_min[i] <- min(bin_data$pred)
    results$bin_max[i] <- max(bin_data$pred)
    results$predicted_prob[i] <- mean(bin_data$pred)
    results$actual_prob[i] <- mean(bin_data$actual)
    results$n_points[i] <- nrow(bin_data)
  }
  
  # Calculate confidence intervals
  results$se <- sqrt(results$actual_prob * (1 - results$actual_prob) / results$n_points)
  results$ci_lower <- pmax(0, results$actual_prob - 1.96 * results$se)
  results$ci_upper <- pmin(1, results$actual_prob + 1.96 * results$se)
  
  # Add midpoint for convenience
  results$midpoint <- (results$bin_min + results$bin_max) / 2
  
  return(results)
}

# Helper function to check bin distribution
# Function to create player-specific point calibration
# create_player_calibration <- function(data, player_id, player_name) {
#   player_data <- subset(data, Point_Server_id == player_id | Point_non_Server_id == player_id)
#   cal_data <- create_calibration_data(
#     predicted_probs = player_data$predicted_prob,
#     actual_outcomes = player_data$Point_Server_score
#   )
#   cal_data$player <- player_name
#   return(cal_data)
# }


# Compute predicted probabilities
csv_2011$predicted_prob <- compute_win_prob(csv_2011, sum_2011)
csv_2016$predicted_prob <- compute_win_prob(csv_2016, sum_2016)

# Create calibration data for main datasets
cal_2011 <- create_calibration_data(
  predicted_probs = csv_2011$predicted_prob,
  actual_outcomes = csv_2011$Point_Server_score
)
cal_2011$period <- "2011-2015"

cal_2016 <- create_calibration_data(
  predicted_probs = csv_2016$predicted_prob,
  actual_outcomes = csv_2016$Point_Server_score
)
cal_2016$period <- "2016-2022"

# Combine calibration data
cal_combined <- rbind(cal_2011, cal_2016)

######----- Player calibrations
create_player_calibration <- function(data, sum_params, player_id, player_name) {
  # Get win probabilities using our improved function
  win_probs <- compute_player_win_prob(data, sum_params, player_id)
  
  # Process serving data
  serving_cal <- NULL
  if (!is.null(win_probs$serving$data)) {
    serving_cal <- create_calibration_data_for_player(
      predicted_probs = win_probs$serving$data$probability,
      actual_outcomes = win_probs$serving$data$actual,10
    )
    serving_cal$scenario <- "serving"
  }
  
  # Process receiving data
  receiving_cal <- NULL
  if (!is.null(win_probs$receiving$data)) {
    receiving_cal <- create_calibration_data_for_player(
      predicted_probs = win_probs$receiving$data$probability,
      actual_outcomes = win_probs$receiving$data$actual,10
    )
    receiving_cal$scenario <- "receiving"
  }
  
  # Combine calibrations
  cal_data <- rbind(serving_cal, receiving_cal)
  cal_data$player <- player_name
  
  return(cal_data)
}


create_player_calibrations <- function(data, sum_params, players_df) {
  do.call(rbind, lapply(1:nrow(players_df), function(i) {
    create_player_calibration(
      data = data,
      sum_params = sum_params,
      player_id = players_df$id[i],
      player_name = players_df$name[i]
    )
  }))
}
# Define players
players_2016 <- data.frame(
  id = c(1, 2, 6),
  name = c("Novak Djokovic", "Serena Williams", "Roger Federer")
)

players_2011 <- data.frame(
  id = c(1, 4, 3),
  name = c("Novak Djokovic", "Serena Williams", "Roger Federer")
)
player_calibrations2016 <- create_player_calibrations(
  data = csv_2016,
  sum_params = sum_2016,
  players_df = players_2016
)

# For 2011 data
player_calibrations2011 <- create_player_calibrations(
  data = csv_2011,
  sum_params = sum_2011,
  players_df = players_2011
)

# Combine calibrations with period information
player_calibrations_combined <- rbind(
  transform(player_calibrations2011, period = "2011-2015"),
  transform(player_calibrations2016, period = "2016-2022")
)
# # Create player-specific calibrations
# player_calibrations2016 <- do.call(rbind, lapply(1:nrow(players_2016), function(i) {
#   create_player_calibration(csv_2016, players_2016$id[i], players_2016$name[i])
# }))
# player_calibrations2011 <- do.call(rbind, lapply(1:nrow(players_2011), function(i) {
#   create_player_calibration(csv_2011, players_2011$id[i], players_2011$name[i])
# }))
# 
# # Combine player calibrations
# player_calibrations_combined <- rbind(
#   transform(player_calibrations2011, period = "2011-2015"),
#   transform(player_calibrations2016, period = "2016-2022")
# )

###########################################
# PART 2: SERVE SPEED ANALYSIS
###########################################

# Function to extract serve speed parameters
extract_serve_params <- function(summary_data) {
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

# Function to compute serve speeds
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
      data$progress_1minus2_abs[serve_indices] +
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


# Function to create serve speed calibration data
# Extract parameters and compute predictions
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


predictions_2011 <- compute_serve_speed(csv_2011_serve, params_2011, stan_2011)
predictions_2016 <- compute_serve_speed(csv_2016_serve, params_2016, stan_2016)

# Create serve speed calibration data
cal_serve_2011 <- create_serve_calibration_data(
  predicted_speeds = predictions_2011$speeds,
  actual_speeds = csv_2011_serve$Speed_KMH[predictions_2011$indices]
)
cal_serve_2011$period <- "2011-2015"

cal_serve_2016 <- create_serve_calibration_data(
  predicted_speeds = predictions_2016$speeds,
  actual_speeds = csv_2016_serve$Speed_KMH[predictions_2016$indices]
)
cal_serve_2016$period <- "2016-2022"
# Combine serve calibration data
cal_serve_combined <- rbind(cal_serve_2011, cal_serve_2016)

###########################################
# PART 3: PLOTTING FUNCTIONS
###########################################

# Function to create combined point probability calibration plot
create_combined_point_plot <- function(data) {
  # Filter data to remove points where either the point or error bars would be outside limits
  filtered_data <- data %>% 
    dplyr::filter(n_points > 500,
                  actual_prob * 100 >= 50,
                  actual_prob * 100 <= 70,
                  predicted_prob * 100 > 52,
                  predicted_prob * 100 <= 70,
                  ci_lower * 100 >= 47,
                  ci_upper * 100 <= 70)
  
  ggplot(filtered_data, 
         aes(x = predicted_prob * 100, y = actual_prob * 100, color = period)) +
  #  geom_hline(yintercept = 50, linetype = "dashed", color = "black", alpha = 1) +
 #   geom_vline(xintercept = 50, linetype = "dashed", color = "black", alpha = 1) +
    
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", alpha = 1) +
    geom_point(aes(size = n_points), alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                  width = 0.5, alpha = 0.8) +
    scale_size_continuous(name = "Number of points",
                          range = c(2, 8),
                          breaks = c(1000, 10000, 50000, 100000),
                          labels = scales::comma) +
    scale_color_manual(
      name = "",
      values = c("2011-2015" = "#2171B5", "2016-2022" = '#C1876B'),
      guide = guide_legend(override.aes = list(size = 6, alpha = 0.7,shape=16))
    ) +
    coord_cartesian(xlim = c(50, 67), 
                    ylim = c(50, 67),
                    clip = "on") +
    scale_x_continuous(breaks = seq(50, 65, 5),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    scale_y_continuous(breaks = seq(50, 65, 5),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    labs(x = "Predicted server's p(win point)",
         y = "Actual server's p(win point)") +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.position = c(1.15, 0.5),
      legend.justification = c(0, 0.5),
      legend.title = element_text(size = 24,color='black'),
      legend.text = element_text(size = 21,color='black'),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    )
}
# Function to create combined serve speed calibration plot
create_combined_serve_plot <- function(data) {
  ggplot(data %>% dplyr::filter(n_points > 200), 
         aes(x = predicted_speed, y = actual_speed, color = period)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 1) +
    geom_point(aes(size = n_points), alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 2, alpha = 0.8) +
    scale_size_continuous(name = "Number of serves",
                          range = c(2, 8),
                          breaks = c(100, 500, 1000, 5000),
                          labels = scales::comma) +
    scale_color_manual(
      name = "",
      values = c("2011-2015" = "#2171B5", "2016-2022" = '#C1876B'),
      guide = guide_legend(override.aes = list(size = 6, alpha = 0.7,shape=16))
    ) +
    scale_x_continuous(breaks = seq(140, 200, by = 20),limits=c(130,200)) +
    scale_y_continuous(breaks = seq(140, 200, by = 20),limits=c(130,200)) +
    coord_equal() +
    labs(x = "Predicted serve speed (km/h)",
         y = "Actual serve speed (km/h)") +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.position = c(1.15, 0.5),
      legend.justification = c(0, 0.5),
      legend.title = element_text(size = 24,color='black'),
      legend.text = element_text(size = 21,color='black'),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1
    )
}

create_shared_theme <- function() {
  theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 22, hjust = 0.5),
      axis.text = element_text(size = 16),
      axis.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 22),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      axis.line = element_blank(),
      aspect.ratio = 1,
      legend.position = "right",
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(1.5, "lines"),
      plot.margin = margin(5, 20, 5, 5)
    )
}

# Shared color palette
shared_colors <- c("2011-2015" = "#2171B5", "2016-2022" = "#C1876B")



# Function to create player-specific point probability plots
create_player_point_plot <- function(data, player_name) {
  data_filtered <- data %>%  
    dplyr::filter(
      player == player_name, 
      n_points > 200
    ) %>%
    # Explicitly set scenario order
    dplyr::mutate(scenario = factor(scenario, levels = c("serving", "receiving")))
  
  ggplot(data_filtered, 
         aes(x = predicted_prob * 100, 
             y = actual_prob * 100, 
             color = period,
             shape = scenario,
             fill = period     
         )) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", alpha = 1) +
    # Use fixed size instead of mapping to n_points
    geom_point(size = 6, alpha = 0.7,stroke=3) +
    geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                  width = 0.5, alpha = 0.8) +
    scale_color_manual(values = c("2011-2015" = "#2171B5", "2016-2022" = "#C1876B")) +
    scale_fill_manual(values = c("2011-2015" = "#2171B5", "2016-2022" = "#C1876B")) +
    scale_shape_manual(values = c("serving" = 16, "receiving" = 1),
                       breaks = c("serving", "receiving")) +
    coord_cartesian(xlim = c(30, 75), ylim = c(30, 75)) +
    scale_x_continuous(breaks = seq(30, 70, 10),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    scale_y_continuous(breaks = seq(30, 70, 10),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    labs(x = "", y = "", title = player_name) +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_blank(),
      plot.title = element_text(size = 22, hjust = .5),
      axis.text = element_text(size = 16),
      legend.position = "none",
      legend.text = element_text(size=20),
      legend.title = element_text(size=22),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      axis.line = element_blank(),
      aspect.ratio = 1,
      plot.margin = margin(5, 20, 5, 5)
    )
}

create_player_point_plot_with_legend <- function(data, player_name) {
  # Create base plot
  base_plot <- create_player_point_plot(data, player_name)
  
  # Add legends with correct order and appearance
  base_plot + 
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.margin = margin(0, 0, 0, 0),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          legend.key.size = unit(1.5, "lines")) +  # Standardize legend key size
    guides(
      fill = guide_legend(order = 1, 
                          title = "",
                          override.aes = list(shape = 21,
                                              size = 6,
                                              alpha = 1,stroke=.1)),
      shape = guide_legend(order = 2,
                           title = " ",
                           override.aes = list(size = 6,  # Same size for both shapes
                                               stroke = 0.5,  # Consistent border thickness
                                               color = "black",
                                               fill = "grey50")),
      color = "none"
    )
}

# Create player-specific serve speed calibration function

create_player_serve_cal_plot <- function(data, player_id_2011, player_id_2016, player_name, predictions_2011, predictions_2016, csv_2011, csv_2016) {
  # Helper function to process data for one period
  process_period_data <- function(csv_data, predictions, period, player_id) {
    serve_indices <- predictions$indices
    player_serves <- !is.na(csv_data$Point_Server_id[serve_indices]) & 
      csv_data$Point_Server_id[serve_indices] == player_id
    
    cat(sprintf("\nProcessing %s for player %s (ID: %d)\n", period, player_name, player_id))
    cat("Total serves:", sum(player_serves, na.rm=TRUE), "\n")
    
    # Add raw speed checks before any processing
    cat("\nRaw serve speeds for this player:\n")
    raw_speeds <- csv_data$Speed_KMH[serve_indices][player_serves]
    raw_serve_numbers <- csv_data$ServeNumber[serve_indices][player_serves]
    print(tapply(raw_speeds, raw_serve_numbers, function(x) {
      c(mean = mean(x, na.rm=TRUE), 
        sd = sd(x, na.rm=TRUE), 
        n = sum(!is.na(x)))
    }))
    
    if (sum(player_serves, na.rm=TRUE) < 5) return(NULL)
    
    predicted_speeds <- predictions$speeds[player_serves]
    actual_speeds <- csv_data$Speed_KMH[serve_indices][player_serves]
    serve_numbers <- csv_data$ServeNumber[serve_indices][player_serves]
    
    # Debug info
    cat("First/Second serve distribution:\n")
    print(table(serve_numbers))
    
    # Add predicted speed checks
    cat("\nPredicted serve speeds:\n")
    print(tapply(predicted_speeds, serve_numbers, function(x) {
      c(mean = mean(x, na.rm=TRUE), 
        sd = sd(x, na.rm=TRUE), 
        n = sum(!is.na(x)))
    }))
    
    # Remove NAs and invalid values
    valid_idx <- !is.na(predicted_speeds) & !is.na(actual_speeds) & 
      !is.na(serve_numbers) & actual_speeds > 0
    
    if (sum(valid_idx) < 5) return(NULL)
    
    predicted_speeds <- predicted_speeds[valid_idx]
    actual_speeds <- actual_speeds[valid_idx]
    serve_numbers <- serve_numbers[valid_idx]
    
    # Create separate calibration data for first and second serves
    results <- list()
    for (serve_type in c(1, 2)) {
      serve_mask <- serve_numbers == serve_type
      if (sum(serve_mask) < 5) next
      
      cat(sprintf("\nProcessing serve type %d:\n", serve_type))
      cat("Number of serves:", sum(serve_mask), "\n")
      cat("Mean actual speed:", mean(actual_speeds[serve_mask]), "\n")
      cat("Mean predicted speed:", mean(predicted_speeds[serve_mask]), "\n")
      
      # Create fixed number of bins
      n_bins <- 8
      
      # Create breaks based on predicted speeds
      pred_range <- range(predicted_speeds[serve_mask])
      breaks <- seq(pred_range[1], pred_range[2], length.out = n_bins + 1)
      bins <- cut(predicted_speeds[serve_mask], breaks = breaks, include.lowest = TRUE)
      
      # Calculate bin statistics
      bin_stats <- data.frame(
        bin_min = breaks[-length(breaks)],
        bin_max = breaks[-1]
      )
      
      # Add statistics for each bin
      bin_means <- tapply(predicted_speeds[serve_mask], bins, mean, na.rm = TRUE)
      bin_stats$predicted_speed <- as.vector(bin_means)
      
      actual_means <- tapply(actual_speeds[serve_mask], bins, mean, na.rm = TRUE)
      bin_stats$actual_speed <- as.vector(actual_means)
      
      bin_counts <- tapply(actual_speeds[serve_mask], bins, length)
      bin_stats$n_points <- as.vector(bin_counts)
      
      # Calculate standard errors
      bin_sds <- tapply(actual_speeds[serve_mask], bins, sd, na.rm = TRUE)
      bin_stats$se <- as.vector(bin_sds) / sqrt(bin_stats$n_points)
      
      # Remove any NA rows
      bin_stats <- bin_stats[!is.na(bin_stats$predicted_speed), ]
      
      # Add confidence intervals
      bin_stats$ci_lower <- bin_stats$actual_speed - 1.96 * bin_stats$se
      bin_stats$ci_upper <- bin_stats$actual_speed + 1.96 * bin_stats$se
      bin_stats$serve_number <- serve_type
      
      results[[serve_type]] <- bin_stats
    }
    
    combined <- do.call(rbind, results)
    combined$period <- period
    return(combined)
  }
  
  # Process data for both periods using different IDs
  data_2011 <- process_period_data(csv_2011, predictions_2011, "2011-2015", player_id_2011)
  data_2016 <- process_period_data(csv_2016, predictions_2016, "2016-2022", player_id_2016)
  
  # Combine data
  cal_data <- rbind(data_2011, data_2016)
  if (is.null(cal_data)) return(NULL)
  
  # Calculate player-specific ranges and round to nearest 10
  min_speed <- floor(min(cal_data$predicted_speed, cal_data$actual_speed, na.rm=TRUE) / 10) * 10
  max_speed <- ceiling(max(cal_data$predicted_speed, cal_data$actual_speed, na.rm=TRUE) / 10) * 10
  
  # Create breaks that are divisible by 20 and cover the range
  breaks <- seq(floor(min_speed/20) * 20, ceiling(max_speed/20) * 20, by=20)
  
  # Create plot
  g <- ggplot(cal_data %>% dplyr::filter(n_points >= 5), 
              aes(x = predicted_speed, y = actual_speed, 
                  color = period,
                  shape = factor(serve_number))) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 1) +
    geom_point(size=6, alpha = 0.8, stroke=4) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 2, alpha = 0.8) +
    scale_color_manual(values = c("2011-2015" = "#2171B5", "2016-2022" = "#C1876B"),
                       name = "") +
    scale_shape_manual(values = c("1" = 19, "2" = 1),
                       labels = c("First serve", "Second serve"),
                       name = "") +
    scale_size_continuous(range = c(2, 8)) +
    coord_cartesian(xlim = c(min(breaks), max(breaks)), 
                    ylim = c(min(breaks), max(breaks))) +
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(breaks = breaks) +
    labs(x = "",
         y = "",
         title = player_name) +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 22, hjust = 0.5),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 20),
      axis.text = element_text(size = 16),
      legend.position = "right",
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      aspect.ratio = 1,
      axis.line=element_blank()
    )
  
  g <- g + theme(legend.position = "right",
                 legend.box = "vertical",
                 axis.line = element_blank(),
                 legend.margin = margin(0, 0, 0, 0),
                 axis.text = element_text(size = 16),
                 legend.text = element_text(size = 20, color='black'),
                 legend.title = element_text(size = 22),
                 legend.key.size = unit(1.5, "lines")) +
    guides(
      color = guide_legend(order = 1,
                           title = "",
                           override.aes = list(shape = 19,
                                               size = 7,
                                               alpha = 1,
                                               stroke = 0.5)),
      shape = guide_legend(order = 2,
                           title = "",
                           override.aes = list(size = 7,
                                               stroke = 0.5,
                                               color = "black",
                                               fill = "grey50"))
    )
  
  return(g)
}
# Create individual plots
p_serve_djokovic <- create_player_serve_cal_plot(NULL, 
                                                 player_id_2011 = 1,
                                                 player_id_2016 = 1,
                                                 "Novak Djokovic",
                                                 predictions_2011, predictions_2016,
                                                 csv_2011_serve, csv_2016_serve)

p_serve_serena <- create_player_serve_cal_plot(NULL, 
                                               player_id_2011 = 4,
                                               player_id_2016 = 2,
                                               "Serena Williams",
                                               predictions_2011, predictions_2016,
                                               csv_2011_serve, csv_2016_serve)

p_serve_federer <- create_player_serve_cal_plot(NULL, 
                                                player_id_2011 = 3,
                                                player_id_2016 = 6,
                                                "Roger Federer",
                                                predictions_2011, predictions_2016,
                                                csv_2011_serve, csv_2016_serve)

# Combine plots side by side
library(patchwork)
combined_plots <- p_serve_djokovic + p_serve_serena + p_serve_federer +
  plot_layout(guides = "collect")

# Save the combined plot
ggsave(
  filename = paste0(filepath,"player_serve_speeds.svg"),
  plot = combined_plots,
  width = 15,
  height = 7,
  bg = 'white'
)

ggsave(
  filename = paste0(filepath,"player_serve_speeds.png"),
  plot = combined_plots,
  width = 15,
  height = 7,
  bg = 'white'
)
###########################################
# PART 4: CREATE ALL  subject level PLOTS
###########################################

# Create population-level plots
p_points_combined <- create_combined_point_plot(cal_combined)
p_serve_combined <- create_combined_serve_plot(cal_serve_combined)

# Top row: population-level plots
top_row <- p_points_combined + p_serve_combined +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(1, 20, 1, 20, "pt"),
        legend.box.margin = margin(0, 0, 0, 40, "pt")  # Added 40pt margin to legend box
  )
ggsave(
  filename = file.path(filepath, "group_level_ppc.png"),
  plot = top_row,
  width = 15,
  height = 9,
  bg = 'white'
)
ggsave(
  filename = file.path(filepath, "group_level_ppc.svg"),
  plot = top_row,
  width = 15,
  height = 9,
  bg = 'white'
)
create_shared_theme <- function() {
  theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 22, hjust = 0.5,vjust=-16),
      axis.text = element_text(size = 16),
      axis.title = element_blank(),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 22),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5),
      axis.line = element_blank(),
      aspect.ratio = 1,
      legend.position = "right",
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(1.5, "lines"),
      plot.margin = margin(1, 500, 1, 5)
    )
}

# Shared color palette
shared_colors <- c("2011-2015" = "#2171B5", "2016-2022" = "#C1876B")

# Function to create player point plot
create_player_point_plot <- function(data, player_name) {
  data_filtered <- data %>%  
    dplyr::filter(
      player == player_name, 
      n_points > 200
    ) %>%
    dplyr::mutate(scenario = factor(scenario, levels = c("serving", "receiving")))
  
  ggplot(data_filtered, 
         aes(x = predicted_prob * 100, 
             y = actual_prob * 100, 
             color = period,
             shape = scenario,
             fill = period     
         )) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", alpha = 1) +
    geom_point(size = 6, alpha = 0.7, stroke = 3) +
    geom_errorbar(aes(ymin = ci_lower * 100, ymax = ci_upper * 100),
                  width = 0.5, alpha = 0.8) +
    scale_color_manual(values = shared_colors) +
    scale_fill_manual(values = shared_colors) +
    scale_shape_manual(values = c("serving" = 16, "receiving" = 1)) +
    coord_cartesian(xlim = c(30, 75), ylim = c(30, 75)) +
    scale_x_continuous(breaks = seq(30, 70, 10),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    scale_y_continuous(breaks = seq(30, 70, 10),
                       labels = function(x) paste0(sprintf("%.0f", x), "%")) +
    labs(title = player_name) +
    create_shared_theme()
}

# Function to create player point plot with legend
create_player_point_plot_with_legend <- function(data, player_name) {
  base_plot <- create_player_point_plot(data, player_name)
  
  base_plot + 
    guides(
      fill = guide_legend(order = 1, 
                          title = "",
                          override.aes = list(shape = 21,
                                              size = 6,
                                              alpha = 1,
                                              stroke = 0.1)),
      shape = guide_legend(order = 2,
                           title = " ",
                           override.aes = list(size = 6,
                                               stroke = 0.5,
                                               color = "black",
                                               fill = "grey50")),
      color = "none"
    )
}

# Helper function to process serve calibration data
process_period_data <- function(csv_data, predictions, period, player_id) {
  serve_indices <- predictions$indices
  player_serves <- !is.na(csv_data$Point_Server_id[serve_indices]) & 
    csv_data$Point_Server_id[serve_indices] == player_id
  
  cat(sprintf("\nProcessing %s for player ID: %d\n", period, player_id))
  cat("Total serves:", sum(player_serves, na.rm=TRUE), "\n")
  
  if (sum(player_serves, na.rm=TRUE) < 5) return(NULL)
  
  predicted_speeds <- predictions$speeds[player_serves]
  actual_speeds <- csv_data$Speed_KMH[serve_indices][player_serves]
  serve_numbers <- csv_data$ServeNumber[serve_indices][player_serves]
  
  cat("First/Second serve distribution:\n")
  print(table(serve_numbers))
  
  valid_idx <- !is.na(predicted_speeds) & !is.na(actual_speeds) & 
    !is.na(serve_numbers) & actual_speeds > 0
  
  if (sum(valid_idx) < 5) return(NULL)
  
  predicted_speeds <- predicted_speeds[valid_idx]
  actual_speeds <- actual_speeds[valid_idx]
  serve_numbers <- serve_numbers[valid_idx]
  
  results <- list()
  for (serve_type in c(1, 2)) {
    serve_mask <- serve_numbers == serve_type
    if (sum(serve_mask) < 5) next
    
    cat(sprintf("\nProcessing serve type %d:\n", serve_type))
    cat("Number of serves:", sum(serve_mask), "\n")
    
    n_bins <- 8
    pred_range <- range(predicted_speeds[serve_mask])
    breaks <- seq(pred_range[1], pred_range[2], length.out = n_bins + 1)
    bins <- cut(predicted_speeds[serve_mask], breaks = breaks, include.lowest = TRUE)
    
    bin_stats <- data.frame(
      bin_min = breaks[-length(breaks)],
      bin_max = breaks[-1]
    )
    
    bin_means <- tapply(predicted_speeds[serve_mask], bins, mean, na.rm = TRUE)
    bin_stats$predicted_speed <- as.vector(bin_means)
    
    actual_means <- tapply(actual_speeds[serve_mask], bins, mean, na.rm = TRUE)
    bin_stats$actual_speed <- as.vector(actual_means)
    
    bin_counts <- tapply(actual_speeds[serve_mask], bins, length)
    bin_stats$n_points <- as.vector(bin_counts)
    
    bin_sds <- tapply(actual_speeds[serve_mask], bins, sd, na.rm = TRUE)
    bin_stats$se <- as.vector(bin_sds) / sqrt(bin_stats$n_points)
    
    bin_stats <- bin_stats[!is.na(bin_stats$predicted_speed), ]
    
    bin_stats$ci_lower <- bin_stats$actual_speed - 1.96 * bin_stats$se
    bin_stats$ci_upper <- bin_stats$actual_speed + 1.96 * bin_stats$se
    bin_stats$serve_number <- serve_type
    
    results[[serve_type]] <- bin_stats
  }
  
  combined <- do.call(rbind, results)
  combined$period <- period
  return(combined)
}

# Function to create player serve calibration plot
create_player_serve_cal_plot <- function(data, player_id_2011, player_id_2016, 
                                         player_name, predictions_2011, predictions_2016, 
                                         csv_2011, csv_2016) {
  
  # Process data for both periods using different IDs
  data_2011 <- process_period_data(csv_2011, predictions_2011, "2011-2015", player_id_2011)
  data_2016 <- process_period_data(csv_2016, predictions_2016, "2016-2022", player_id_2016)
  
  # Combine data
  cal_data <- rbind(data_2011, data_2016)
  if (is.null(cal_data)) return(NULL)
  
  # Calculate player-specific ranges and round to nearest 10
  min_speed <- floor(min(cal_data$predicted_speed, cal_data$actual_speed, na.rm=TRUE) / 10) * 10
  max_speed <- ceiling(max(cal_data$predicted_speed, cal_data$actual_speed, na.rm=TRUE) / 10) * 10
  
  # Create breaks that are divisible by 20
  breaks <- seq(floor(min_speed/20) * 20, ceiling(max_speed/20) * 20, by=20)
  
  # Create plot with unified styling
  ggplot(cal_data %>% dplyr::filter(n_points >= 5), 
         aes(x = predicted_speed, 
             y = actual_speed, 
             color = period,
             shape = factor(serve_number))) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                color = "gray50", alpha = 1) +
    geom_point(size = 6, alpha = 0.8, stroke = 4) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 2, alpha = 0.8) +
    scale_color_manual(values = shared_colors,
                       name = "") +
    scale_shape_manual(values = c("1" = 19, "2" = 1),
                       labels = c("First serve", "Second serve"),
                       name = "") +
    coord_cartesian(xlim = c(min(breaks), max(breaks)), 
                    ylim = c(min(breaks), max(breaks))) +
    scale_x_continuous(breaks = breaks) +
    scale_y_continuous(breaks = breaks) +
    labs(title = player_name) +
    create_shared_theme() +
    guides(
      color = guide_legend(order = 1,
                           title = "",
                           override.aes = list(shape = 19,
                                               size = 6,
                                               alpha = 1,
                                               stroke = 0.5)),
      shape = guide_legend(order = 2,
                           title = "",
                           override.aes = list(size = 6,
                                               stroke = 0.5,
                                               color = "black",
                                               fill = "grey50"))
    )
}



# Create individual plots

p_point_djokovic <- create_player_point_plot_with_legend(player_calibrations_combined, "Novak Djokovic")
p_point_nadal <- create_player_point_plot_with_legend(player_calibrations_combined, "Serena Williams")
p_point_federer <- create_player_point_plot_with_legend(player_calibrations_combined, "Roger Federer")

p_serve_djokovic <- create_player_serve_cal_plot(NULL, 
                                                 player_id_2011 = 1,
                                                 player_id_2016 = 1,
                                                 "Novak Djokovic",
                                                 predictions_2011, predictions_2016,
                                                 csv_2011_serve, csv_2016_serve)

p_serve_serena <- create_player_serve_cal_plot(NULL, 
                                               player_id_2011 = 4,
                                               player_id_2016 = 2,
                                               "Serena Williams",
                                               predictions_2011, predictions_2016,
                                               csv_2011_serve, csv_2016_serve)

p_serve_federer <- create_player_serve_cal_plot(NULL, 
                                                player_id_2011 = 3,
                                                player_id_2016 = 6,
                                                "Roger Federer",
                                                predictions_2011, predictions_2016,
                                                csv_2011_serve, csv_2016_serve)

# Combine plots side by side
library(patchwork)
combined_plots <- p_serve_djokovic + p_serve_serena + p_serve_federer +
  plot_layout(guides = "collect")

# First, ensure each individual plot has minimal margins
plot_theme_compact <- theme(
  plot.margin = margin(1, 1, -30, 1, "pt"),
  legend.position = "right",
  legend.box.margin = margin(0, 0, 0, 0),
  legend.margin = margin(0, 0, 0, 0)
)

# Apply compact theme to each row with proper patchwork settings
top_row <- (p_points_combined + p_serve_combined) +
  plot_layout(guides = "collect", widths = c(1, 1)) & 
  plot_theme_compact

middle_row <- (p_point_djokovic +plot_spacer() + p_point_nadal + plot_spacer() +p_point_federer) +
  plot_layout(guides = "collect", widths = c(1,.2, 1,.2, 1)) & 
  plot_theme_compact

bottom_row <- (p_serve_djokovic +plot_spacer() + p_serve_serena +plot_spacer() + p_serve_federer) +
  plot_layout(guides = "collect", widths = c(1,.2, 1,.2, 1)) & 
  plot_theme_compact

# Combine rows with minimal spacing using patchwork
all_ppc <-  middle_row / bottom_row +
  plot_layout(heights = c( 1, 1))

# Save with adjusted dimensions
ggsave(
  filename = file.path(filepath, "point_player_ppc.png"),
  plot = middle_row,
  width = 15,
  height = 7,  # Reduced height to minimize gaps
  bg = 'white',
  dpi = 300,
  units = "in"
)
ggsave(
  filename = file.path(filepath, "serve_player_ppc.png"),
  plot = bottom_row,
  width = 15,
  height = 7,  # Reduced height to minimize gaps
  bg = 'white',
  dpi = 300,
  units = "in"
)

