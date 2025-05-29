library(rstan)
library(rstudioapi) 
setwd("~/Desktop/02Phd/tennis_project/")
source("~/Desktop/02Phd/tennis_project/erans/set_data.R")

progress_type = "game"  # "match" or "game"
progress_effect = "int_abs"  # "int_signed" or "int_mixed" or "int_abs"
years = 2011:2015
data_rep = set_data(progress_type = progress_type, progress_effect = progress_effect, years = years) 

json_data <- toJSON(data)
write(json_data, "~/Desktop/02Phd/tennis_project/data/data_2011_2015_critical_overall.json")
write(json_data, "/Users/levisolomyak/Desktop/02Phd/tennis_project/data/data_5p.json")

iterations = 3000
warmup = 1000
chains = 5
cores = chains + 1
rstan_options(auto_write = F)
start_time <- Sys.time()
model = paste0("surprise_serve_lr_progress_", progress_effect, ".stan")

m <- rstan::stan(file = model, data = data,
                                 iter = iterations,
                                 warmup =warmup,
                                 chains =chains,
                                 cores = cores,
                                 control=list(max_treedepth=8, adapt_delta = 0.80),
                                 pars = c("beta_surprise_int_player_dev",  "beta_surprise_player_dev",  "beta_serve_speed_surprise_int_player_dev", "beta_serve_number_speed_plus_1", "beta_serve_speed_surprise_server", "beta_surprise_player1","beta_surprise_player1","beta_surprise_player2", "surprise","p","cumulative_surprise_player2","cumulative_surprise_player1","cumulative_surprise_server","sum_base_predictors","sum_base_predictors_serve_speed"),
                                 include = FALSE)

save(m, file = paste0("m_",progress_effect, "_",progress_type, "_", years[1], "to", tail(years,n=1), ".Rdata"))
end_time <- Sys.time()
print(end_time - start_time)
        

