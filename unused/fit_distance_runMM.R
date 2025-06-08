library(JuliaCall)
julia_setup()

csv_data <- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_2011_wDrun.csv')
#csv_data<- read_csv('/Users/levisolomyak/Desktop/02Phd/tennis_project/data/df_critical_checked_2011_2015.csv')
load('/Users/levisolomyak/Desktop/02Phd/tennis_project/stan_results/abs_model/summary_2011.rds')
csv_data$serve_again <- csv_data$ServeNumber-1

julia_command('using MixedModels')
julia_command('using DataFrames')
julia_command('using Statistics')
julia_command('using MixedModels') 
julia_command('using DataFrames')
julia_command('using StatsBase')

# Create numeric IDs for players and matches

matches <- unique(csv_data$match_id)
match_map <- data.frame(
  match_id = matches,
  match_num = 1:length(matches)
)
csv_data$match_num <- match_map$match_num[match(csv_data$match_id, match_map$match_id)]

sum_2016 <- read_csv('/Users/levisolomyak/Downloads/full_summary_2016.csv')
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
surprise_df_2016 <- get_cumulative_surprise(sum_2016)

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

# Load the packages
distance_data <- csv_data %>% dplyr::filter(!is.na(P1DistanceRun),!is.na(P2DistanceRun),P1DistanceRun!=0,P2DistanceRun!=0)
# Create the processed dataframe in R
distance_long <- distance_data %>%
  pivot_longer(
    cols = c(P1DistanceRun, P2DistanceRun),
    names_to = "player_type",
    values_to = "distance_run"
  ) %>%
  dplyr::mutate(
    is_player1 = player_type == "P1DistanceRun",
    player_id = if_else(is_player1, numeric_id1, numeric_id2),
    player_served = case_when(
      is_player1 & PointServer == 1 ~ 1,
      !is_player1 & PointServer == 2 ~ 1,
      TRUE ~ -1
    ),
    cumulative_surprise = case_when(
      is_player1 ~ cumulative_surprise_player1,
      !is_player1 ~ cumulative_surprise_player2
    )
  ) %>%
  dplyr::group_by(match_id, player_id) %>%
  dplyr::mutate(
    prev_distance = lag(distance_run),
    mean_prev_distances = lag(cummean(distance_run))
  ) %>%
  ungroup() %>%
  # Remove rows with NA values
  dplyr::filter(!is.na(prev_distance), 
         !is.na(mean_prev_distances),
         !is.na(distance_run),
         !is.na(cumulative_surprise),
         !is.na(progress_1minus2_abs))

# Transfer the data to Julia
julia_assign("data", as.data.frame(distance_long))
julia_command("using MixedModels, DataFrames, Statistics, CategoricalArrays")

# Define and run the model
julia_command('
function fit_distance_model(df)
   data = DataFrame(df)
   
   # Z-score normalize predictors and response
   data.distance_z = (data.distance_run .- mean(data.distance_run)) ./ std(data.distance_run)
   data.cumulative_surprise_z = (data.cumulative_surprise .- mean(data.cumulative_surprise)) ./ std(data.cumulative_surprise)
   data.progress_z = (data.progress_1minus2_abs .- mean(data.progress_1minus2_abs)) ./ std(data.progress_1minus2_abs)
   data.prev_distance_z = (data.prev_distance .- mean(data.prev_distance)) ./ std(data.prev_distance)
   data.mean_prev_distances_z = (data.mean_prev_distances .- mean(data.mean_prev_distances)) ./ std(data.mean_prev_distances)
   
   # Convert categorical variables
   data.player_id = CategoricalArray(data.player_id)
   data.match_id = CategoricalArray(data.match_id)

   
   # Fit model with enhanced random effects structure
   model = fit(MixedModel,
       @formula(distance_z ~ 1 + 
           # Fixed effects
           player_served + 
           mean_prev_distances_z +
           cumulative_surprise_z * progress_z +
           
           # Random effects for player baseline and interactions
           (1 + cumulative_surprise_z * progress_z| player_id) +
           
 
           
           # Random effects for match-specific running effects
           (0 + player_served | match_id)), 
       data)
   
   return model
end')
julia_eval('fit_distance_model(data)')

library(dplyr)
library(lme4)

fit_distance_model_R <- function(data) {
  # Create z-scores for numeric variables
  model_data <- data %>%
    dplyr::mutate(
      distance_z = scale(distance_run),
      cumulative_surprise_z = scale(cumulative_surprise),
      progress_z = scale(progress_1minus2_abs),
      prev_distance_z = scale(prev_distance),
      mean_prev_distances_z = scale(mean_prev_distances),
      # Convert to factors if not already
      player_id = as.factor(player_id),
      match_id = as.factor(match_id)
    )
  
  # Fit the mixed model
  model <- lmer(
    distance_z ~ 1 + 
      # Fixed effects
      player_served +
      mean_prev_distances_z +
      cumulative_surprise_z * progress_z +
      
      # Random effects for player
      (1 + cumulative_surprise_z * progress_z | player_id) +
      
      # Random effects for match
      (0 + player_served | match_id),
    
    data = model_data,
    control = lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 100000)
    )
  )
  
  return(list(
    model = model,
    summary = summary(model),
    ranef = ranef(model),
    fixef = fixef(model),
    vcov = VarCorr(model)
  ))
}

# Fit the model
r_model_results <- fit_distance_model_R(distance_long)

# Print key results
print("Fixed Effects:")
print(r_model_results$fixef)
print("\nRandom Effects Variance Components:")
print(r_model_results$vcov)

# Get confidence intervals
print("\n95% Confidence Intervals for Fixed Effects:")
print(confint(r_model_results$model, method="Wald"))

