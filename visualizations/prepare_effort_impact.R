logistic <- function(x) {
  1 / (1 + exp(-x))
}

extract_specific_fits <- function(df){
  summary_file <- read.csv('/Users/levisolomyak/Downloads/full_summary.csv')
  # Create a unique row identifier
  rownames(summary_file) <- summary_file[, 1]  # Set row names from the first column
  summary_file <- summary_file[, -1]  # Remove the first column
  
  #match id was never set for df 
  df$match_id=as.numeric(as.factor(df$match_id))
  
  df$alpha_raw=summary_file$Mean[2]
  add_beta_player_columns <- function(df, summary_file) {
    # Extract the player IDs from the row names in the summary file
    rownames(summary_file) <- gsub("beta_player_raw\\[|\\]", "", rownames(summary_file))
    
    # Convert row names to integers so they match the player IDs in df
    player_ids <- as.integer(rownames(summary_file))
    
    # Create a lookup vector for the mean values of beta_player
    beta_means <- summary_file$Mean
    
    # Create the new columns in the df
    df$beta_player_server <- beta_means[match(df$Point_Server_id, player_ids)]
    df$beta_player_non_server <- beta_means[match(df$Point_non_Server_id, player_ids)]
    df$p_win_point =inv.logit(df$sum_base_predictor)
    
    return(df)
  }
  
  beta_player_means <- summary_file[grep("beta_player_raw\\[", rownames(summary_file)), "Mean"]
  df <- add_beta_player_columns(df, summary_file)
  
  beta_match_raw_means <- summary_file[grep("beta_match_raw\\[", rownames(summary_file)), "Mean"]
  sigma_match <- summary_file[str_detect(rownames(summary_file), "sigma_match"), "Mean"]
  beta_match= beta_match_raw_means * sigma_match 
  beta_match_named <- setNames(beta_match, 1:length(beta_match_raw_means))  # Assuming match_id values start from 1 and go up sequentially
  df$beta_match <- beta_match_named[df$match_id] * df$player1_served
  df$sum_base_predictor=df$alpha_raw+(df$beta_player_server-df$beta_player_non_server)+df$beta_match
  return(df)}



# First, extract the probabilities from base_sum
get_base_probabilities <- function(base_sum) {
  # Extract and sort the rows with sum_base_predictors_save
  base_probs <- base_sum %>%
    dplyr::filter(grepl("sum_base_predictors_save\\[\\d+\\]", ...1)) %>%
    # Extract the index number from the row names
    dplyr::mutate(
      index = as.numeric(stringr::str_extract(...1, "\\d+")),
    ) %>%
    # Sort by index to ensure correct order
    dplyr::arrange(index) %>%
    dplyr::select(Mean) %>%
    pull()
  
  # Convert to probabilities using logistic function
  return(logistic(base_probs))
}

p_win_from_deuce <- function(p) {
  p^2 / (p^2 + (1-p)^2)
}
p_win_game <- function(p_win_point_serving, points_remaining_1, points_remaining_2, tie_breaker, p_not_serving_always=.5) {
  # Diagnostic logging before processing
  # print("Input diagnostics:")
  # print(paste("p_win_point_serving:", paste(head(p_win_point_serving), collapse=", ")))
  # print(paste("points_remaining_1:", paste(head(points_remaining_1), collapse=", ")))
  # print(paste("points_remaining_2:", paste(head(points_remaining_2), collapse=", ")))
  # print(paste("tie_breaker:", paste(head(tie_breaker), collapse=", ")))
  # 
  # Vectorize inputs and validate
  p_point <- pmax(pmin(p_win_point_serving, 1), 0)
  
  # Ensure all inputs are of the same length
  max_length <- max(
    length(p_point), 
    length(points_remaining_1), 
    length(points_remaining_2), 
    length(tie_breaker)
  )
  
  p_point <- rep_len(p_point, max_length)
  points_remaining_1 <- rep_len(points_remaining_1, max_length)
  points_remaining_2 <- rep_len(points_remaining_2, max_length)
  tie_breaker <- rep_len(tie_breaker, max_length)
  
  # Ensure integer inputs for points
  points_remaining_1 <- as.integer(points_remaining_1)
  points_remaining_2 <- as.integer(points_remaining_2)
  
  # Initialize result vector
  result <- numeric(max_length)
  
  # Calculate p_win_from_deuce
  p_win_from_deuce <- p_win_from_deuce(p_point)
  
  # Case 1: Player 1 already won
  player1_won <- points_remaining_1 == 0 & points_remaining_2 >= 2
  result[player1_won] <- 1.0
  
  # Case 2: Player 2 already won
  player2_won <- points_remaining_2 == 0 & points_remaining_1 >= 2
  result[player2_won] <- 0.0
  
  # Case 3: Deuce (equal points remaining)
  deuce_mask <- points_remaining_1 == points_remaining_2 & points_remaining_1 <= 2
  result[deuce_mask] <- p_win_from_deuce[deuce_mask]
  
  # Case 4: Advantage player 1
  adv_p1_mask <- points_remaining_1 == 1 & points_remaining_2 == 2
  result[adv_p1_mask] <- p_point[adv_p1_mask] + (1 - p_point[adv_p1_mask]) * p_win_from_deuce[adv_p1_mask]
  
  # Case 5: Advantage player 2
  adv_p2_mask <- points_remaining_2 == 1 & points_remaining_1 == 2
  result[adv_p2_mask] <- p_point[adv_p2_mask] * p_win_from_deuce[adv_p2_mask]
  
  # Case 6: Other scenarios (use binomial)
  other_mask <- !(deuce_mask | adv_p1_mask | adv_p2_mask | player1_won | player2_won)
  
  # Diagnostic logging for other scenarios
 # print(paste("Number of other scenarios:", sum(other_mask)))
  
  if (any(other_mask)) {
    # Safe binomial calculation for remaining scenarios
    total_points_needed <- points_remaining_1[other_mask] + points_remaining_2[other_mask] - 1
    
    # Detailed validation
    validation_df <- data.frame(
      total_points_needed = total_points_needed,
      points_remaining_1 = points_remaining_1[other_mask],
      points_remaining_2 = points_remaining_2[other_mask],
      p_point = p_point[other_mask]
    )
    
   # print("Validation dataframe:")
    #print(head(validation_df))
    
    # Ensure valid inputs for pbinom
    valid_mask <- total_points_needed > 0 & 
      points_remaining_1[other_mask] > 0 & 
      points_remaining_2[other_mask] > 0 &
      p_point[other_mask] > 0 & 
      p_point[other_mask] < 1
    
  #  print(paste("Number of valid scenarios:", sum(valid_mask)))
    
    if (any(valid_mask)) {
      result[other_mask][valid_mask] <- 1 - pbinom(
        points_remaining_1[other_mask][valid_mask] - 1,
        size = total_points_needed[valid_mask],
        prob = p_point[other_mask][valid_mask]
      )
    }
  }
  
  return(result)
}
p_win_game <- function(p_win_point_serving, points_remaining_1, points_remaining_2, tie_breaker, p_not_serving_always=.5) {
  p_point = p_win_point_serving
  
  # Calculate p_win_from_deuce for each p_win_point element
  p_win_from_deuce <- p_win_from_deuce(p_point)
  
  result <- numeric(length(points_remaining_1))
  
  # Calculate min points based on game type - now properly vectorized
  min_points_to_win <- dplyr::if_else(tie_breaker == 1, 7L, 4L)
  
  # Case 1: Player 1 already won (their remaining points is 0)
  player1_won <- points_remaining_1 == 0 & points_remaining_2 >= 2
  result[player1_won] <- 1.0
  
  # Case 2: Player 2 already won
  player2_won <- points_remaining_2 == 0 & points_remaining_1 >= 2
  result[player2_won] <- 0.0
  
  # Case 3: Deuce (equal points remaining)
  deuce_mask <- points_remaining_1 == points_remaining_2 & points_remaining_1 <= 2
  result[deuce_mask] <- p_win_from_deuce[deuce_mask]
  
  # Case 4: Advantage player 1
  adv_p1_mask <- points_remaining_1 == 1 & points_remaining_2 == 2
  result[adv_p1_mask] <- p_point[adv_p1_mask] + (1 - p_point[adv_p1_mask]) * p_win_from_deuce[adv_p1_mask]
  
  # Case 5: Advantage player 2
  adv_p2_mask <- points_remaining_2 == 1 & points_remaining_1 == 2
  result[adv_p2_mask] <- p_point[adv_p2_mask] * p_win_from_deuce[adv_p2_mask]
  
  # Case 6: Other scenarios (use binomial)
  other_mask <- !(deuce_mask | adv_p1_mask | adv_p2_mask | player1_won | player2_won)
  
  # For remaining points, use binomial distribution
  total_points_needed <- points_remaining_1[other_mask] + points_remaining_2[other_mask] - 1
  
  result[other_mask] <- 1 - pbinom(points_remaining_1[other_mask] - 1,
                                   size = total_points_needed,
                                   prob = p_point[other_mask])
  
  return(result)
}
shift=.03
base_2011 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/base_model/base_2011_sum.csv')
base_2011_p=get_base_probabilities(base_2011)
csv_2011_sum <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/df_critical_checked_2011_2015.csv')
# Calculate control differences for each player
csv_2011_sum$points_remaining_game_server=ifelse(csv_2011_sum$player1_served==1,csv_2011_sum$points_remaining_game1,csv_2011_sum$points_remaining_game2)
csv_2011_sum$points_remaining_game_non_server=ifelse(csv_2011_sum$player1_served==-1,csv_2011_sum$points_remaining_game1,csv_2011_sum$points_remaining_game2)

csv_2011_sum$p_server_win_point <- base_2011_p

csv_2011_sum <- csv_2011_sum %>%
  dplyr::mutate(
    # # P1's control (difference between high and low effort)
    p_server_win_point_more_effort=pmin(p_server_win_point+.03,1),
    p_server_win_point_less_effort=pmax(p_server_win_point-.03,0),
    p_server_more_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_more_effort, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.53, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    
    p_server_less_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_less_effort, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.47, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    
    p1_control =  p_server_more_effort_win_chance-p_server_less_effort_win_chance,
    p1_control=p1_control/sd(p1_control),
    # P2's control (difference between high and low effort)
    p2_control =  p1_control,
    p_server_control=p1_control,
    p_non_server_control=p1_control,
    ####now uniform-point winning
    p_server_win_point_more_effort_uniform=.6420442,
    p_server_win_point_less_effort_uniform=0.5820442,
 
 
    #game winning 
    p_server_more_effort_win_chance=p_win_game(p_server_win_point_more_effort_uniform, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),
    p_server_less_effort_win_chance=p_win_game(p_server_win_point_less_effort_uniform, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),
    # control
    p_server_control_uniform =  p_server_more_effort_win_chance-  p_server_less_effort_win_chance, 
    p_server_control_uniform=p_server_control_uniform/sd(p_server_control_uniform),
    
    # P2's control (difference between high and low effort)
    p_non_server_control_uniform = p_server_control_uniform,
    p1_control_uniform=p_server_control_uniform,
    p2_control_uniform=p_server_control_uniform)
    



#stan_2011 <- readRDS('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/stan_data_2011_2015.rds')
stan_2011$p1_control=csv_2011_sum$p1_control
stan_2011$p2_control=csv_2011_sum$p2_control
stan_2011$p_server_control=csv_2011_sum$p_server_control
stan_2011$p_non_server_control=csv_2011_sum$p_non_server_control

stan_2011_uniform <- stan_2011
stan_2011_uniform$p1_control=csv_2011_sum$p1_control_uniform
stan_2011_uniform$p2_control=csv_2011_sum$p2_control_uniform
stan_2011_uniform$p_server_control=csv_2011_sum$p_server_control_uniform
stan_2011_uniform$p_non_server_control=csv_2011_sum$p_non_server_control_uniform

stan_2011_non_uniform_effort <- toJSON(stan_2011)
write(stan_2011_non_uniform_effort, "~/Desktop/02Phd/tennis_project/data/data_2011_2015_stan_2011_non_uniform_effort.json")
saveRDS(stan_2011,file='/Users/levisolomyak/Desktop/02Phd/tennis_project/data/stan_data_non_uniform_2011_2015.rds')


stan_2011_uniform_effort <- toJSON(stan_2011_uniform)
write(stan_2011_uniform_effort, "~/Desktop/02Phd/tennis_project/data/data_2011_2015_stan_2011_uniform_effort.json")
saveRDS(stan_2011_uniform,file='/Users/levisolomyak/Desktop/02Phd/tennis_project/data/stan_data_2011_2015.rds')

csv_2016 <- data_rep[[2]]
csv_2016$points_remaining_game_server=ifelse(csv_2016$player1_served==1,csv_2016$points_remaining_game1,csv_2016$points_remaining_game2)
csv_2016$points_remaining_game_non_server=ifelse(csv_2016$player1_served==-1,csv_2016$points_remaining_game1,csv_2016$points_remaining_game2)

### ----
csv_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2016_wDrun.csv')
base_2016 <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/base_model/base_2016_sum.csv')
base_2016_p=get_base_probabilities(base_2016)
print('now 2016 data')

csv_2016$p_server_win_point <- base_2016_p
csv_2016 <- csv_2016 %>%
  dplyr::mutate(
    # # P1's control (difference between high and low effort)
    p_server_win_point_more_effort=pmin(p_server_win_point+.03,1),
    p_server_win_point_less_effort=pmax(p_server_win_point-.03,0),
    p_server_more_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_more_effort, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.53, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    
    p_server_less_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_less_effort, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.47, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    
    p1_control =  p_server_more_effort_win_chance-p_server_less_effort_win_chance,
    p1_control=p1_control/sd(p1_control),
    # P2's control (difference between high and low effort)
    p2_control =  p1_control,
    p_server_control=p1_control,
    p_non_server_control=p1_control,
    ####now uniform-point winning
    p_server_win_point= 0.6189279,
    p_server_win_point_more_effort_uniform=0.6489279,
    p_server_win_point_less_effort_uniform=0.5889279,
    #game winning 
    p_server_more_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_more_effort_uniform, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.53, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    p_server_less_effort_win_chance=if_else(tie_breaker==0,p_win_game(p_server_win_point_less_effort_uniform, points_remaining_game_server, points_remaining_game_non_server, tie_breaker),p_win_game(.47, points_remaining_game_server, points_remaining_game_non_server, tie_breaker)),
    # control
    p_server_control_uniform = p_server_more_effort_win_chance - p_server_less_effort_win_chance,

    p_server_control_uniform = p_server_control_uniform / sd(p_server_control_uniform,na.rm=TRUE),
    # P2's control (difference between high and low effort)
    p_non_server_control_uniform =p_server_control_uniform,
    p1_control_uniform=p_server_control_uniform,
    p2_control_uniform=p_server_control_uniform
    
 )
stan_2016$p1_control=csv_2016$p1_control
stan_2016$p2_control=csv_2016$p2_control
stan_2016$p_server_control=csv_2016$p_server_control
stan_2016$p_non_server_control=csv_2016$p_non_server_control
stan_2016_uniform <- toJSON(stan_2016)
write(stan_2016_uniform, "~/Desktop/02Phd/tennis_project/data/data_2016_2022_stan_2016_non_uniform_effort.json")

stan_2016_uniform <- stan_2016
stan_2016_uniform$p1_control=csv_2016$p1_control_uniform
stan_2016_uniform$p2_control=csv_2016$p2_control_uniform
stan_2016_uniform$p_server_control=csv_2016$p_server_control_uniform
stan_2016_uniform$p_non_server_control=csv_2016$p_non_server_control_uniform
stan_2016_uniform_s <- toJSON(stan_2016_uniform)
write(stan_2016_uniform_s, "~/Desktop/02Phd/tennis_project/data/data_2016_2022_stan_2016_uniform_effort.json")




# Print first few rows to verify
head(select(csv_2011_sum , alpha_raw,p1_win_point, p1_win_point_more_effort, p1_win_point_less_effort, 
            p2_win_point_more_effort, p2_win_point_less_effort,p1_more_effort_win_chance,p2_more_effort_win_chance,p1_less_effort_win_chance,p2_less_effort_win_chance,points_remaining_game_server, points_remaining_game_non_server, p1_control, p2_control,p1_control_uniform ,p2_control_uniform ))-> sds

#visualizae
# If you want to see averages instead of individual points:
plot_data_avg <- csv_2011_sum %>%
  dplyr::filter(tie_breaker==0) %>%
  dplyr::group_by(points_remaining_game_server, points_remaining_game_non_server) %>%
  dplyr::summarise(
    server_control = mean(p_server_control, na.rm = TRUE),
    non_server_control = mean(p_non_server_control, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = c(server_control, non_server_control),
    names_to = "control_type",
    values_to = "control"
  ) %>%
  dplyr::mutate(control_type = if_else(control_type == "server_control", 
                                       "Server Control", "Non-Server Control"))
plot_data_avg <- csv_2011_sum %>%
  dplyr::filter(tie_breaker==0) %>%
  dplyr::group_by(points_remaining_game_server, points_remaining_game_non_server) %>%
  dplyr::summarise(
    server_control = mean(p_server_control_uniform, na.rm = TRUE),
    non_server_control = mean(p_non_server_control_uniform, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    cols = c(server_control, non_server_control),
    names_to = "control_type",
    values_to = "control"
  ) %>%
  dplyr::mutate(
    control_type = dplyr::if_else(control_type == "server_control", "Server Control", "Non-Server Control"),
    score_label = paste(points_remaining_game_server, points_remaining_game_non_server, sep = "-")
  ) %>%
  dplyr::filter(points_remaining_game_server < 4, points_remaining_game_non_server < 4)

ggplot(plot_data_avg, 
       aes(x = score_label, y = control, color = control_type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Server Control" = "blue", 
                                "Non-Server Control" = "red")) +
  labs(x = "Score (Server-Receiver)",
       y = "Control",
       color = "Control Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
