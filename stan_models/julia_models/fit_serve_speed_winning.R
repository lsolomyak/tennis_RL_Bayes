library(dplyr)

# Prepare the data 
serve_data <- csv_data %>%
  dplyr::mutate(
    point_won = dplyr::case_when(
      PointServer == 1 & scored1 == 1 ~ 1,  
      PointServer == 2 & scored1 == 0 ~ 1,  
      TRUE ~ 0                              
    ),
    server_id = dplyr::if_else(PointServer == 1, numeric_id1, numeric_id2),
    receiver_id = dplyr::if_else(PointServer == 1, numeric_id2, numeric_id1),
    is_first_serve = (ServeNumber == 1)
  ) %>%
  dplyr::filter(!is.na(Speed_KMH))

# Initialize Julia and load packages
julia_setup()
julia_command("using MixedModels, DataFrames, Statistics, CategoricalArrays")

# Transfer the data to Julia
julia_assign("data", as.data.frame(serve_data))

# Define and run the model
julia_command('
function fit_serve_model(df)
  # Convert to DataFrame
  data = DataFrame(df)
  
  # Z-score normalize serve speed
  data.speed_z = (data.Speed_KMH .- mean(data.Speed_KMH)) ./ std(data.Speed_KMH)
  
  # Convert IDs to categorical
  data.server_id = CategoricalArray(data.server_id)
  data.receiver_id = CategoricalArray(data.receiver_id)
  
  # Fit model
  model = fit(MixedModel,
      @formula(point_won ~ 1 + 
          speed_z +  

          is_first_serve +         # First vs second serve
          (1 + speed_z | server_id)),  # How different players benefit from speed
      data,
      Binomial())
  
  return model
end')

# Run the model
fit <- julia_eval('fit_serve_model(data)')