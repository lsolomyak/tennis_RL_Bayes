data {
  int<lower=0> n_serve_speed;
  int<lower=0> n_players;
  int Point_Server_id_serve_speed[n_serve_speed];
  vector[n_serve_speed] progress_server;
  vector[n_serve_speed] ServeNumber;
  real serve_speed[n_serve_speed];
  int<lower=0> PointServer[n_serve_speed];
}

parameters {
  real alpha_serve_speed_raw;
  real alpha_serve_speed_int;
  real<lower=0> sigma_serve_speed;
  
  vector[n_players] beta_serve_speed_player_raw;
  real<lower=0> sigma_beta_serve_speed_player;
  vector[n_players] beta_serve_speed_player_int_raw;
  real<lower=0> sigma_beta_serve_speed_player_int;
  
  real beta_serve_number_speed_plus_1_raw;
  vector[n_players] beta_serve_number_speed_plus_1_player_raw;
  real<lower=0> sigma_beta_serve_number_speed_plus_1_player;
  
  real beta_serve_speed_surprise_raw;
  real beta_serve_speed_surprise_int_raw;
  vector[n_players] beta_serve_speed_surprise_player_raw;
  vector[n_players] beta_serve_speed_surprise_int_player_raw;
  real<lower=0> sigma_beta_serve_speed_surprise_player;
  real<lower=0> sigma_beta_serve_speed_surprise_int_player;
  
  real<lower = 0, upper = 1> learning_rate_mean;
  real<lower = 0> learning_rate_concentration_minus2;
  vector<lower = 0, upper = 1>[n_players] learning_rate;
}

transformed parameters {
  vector[n_serve_speed] surprise;
  vector[n_serve_speed] cumulative_surprise_server;
  vector[n_serve_speed] sum_base_predictors_serve_speed;
  vector[n_players] beta_serve_speed_player = beta_serve_speed_player_raw * sigma_beta_serve_speed_player;
  vector[n_players] beta_serve_speed_player_int = beta_serve_speed_player_int_raw * sigma_beta_serve_speed_player_int;
  vector[n_players] beta_serve_number_speed_plus_1 = rep_vector(beta_serve_number_speed_plus_1_raw, n_players) + beta_serve_number_speed_plus_1_player_raw * sigma_beta_serve_number_speed_plus_1_player;
  vector[n_players] beta_serve_speed_surprise_player = beta_serve_speed_surprise_player_raw * sigma_beta_serve_speed_surprise_player;
  vector[n_players] beta_serve_speed_surprise_int_player = beta_serve_speed_surprise_int_player_raw * sigma_beta_serve_speed_surprise_int_player;
  
  sum_base_predictors_serve_speed = alpha_serve_speed_raw + 
                                    (alpha_serve_speed_int + beta_serve_speed_player_int[Point_Server_id_serve_speed]) .* progress_server + 
                                    beta_serve_speed_player[Point_Server_id_serve_speed] + 
                                    (beta_serve_number_speed_plus_1[Point_Server_id_serve_speed] - 1) .* (ServeNumber - 1.5);
  
  // Compute surprise and cumulative surprise
  surprise = serve_speed - sum_base_predictors_serve_speed;
  cumulative_surprise_server[1] = 0;
  for (i in 2:n_serve_speed) {
    cumulative_surprise_server[i] = cumulative_surprise_server[i-1] * (1 - learning_rate[Point_Server_id_serve_speed[i-1]]) + 
                                    surprise[i-1] * learning_rate[Point_Server_id_serve_speed[i-1]];
  }
}

model {
  // Priors
  target += std_normal_lpdf(alpha_serve_speed_raw);
  target += normal_lpdf(alpha_serve_speed_int | 0, 0.1);
  target += std_normal_lpdf(beta_serve_speed_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_player | 1, 10);
  target += std_normal_lpdf(beta_serve_speed_player_int_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_player_int | 1, 10);
  target += std_normal_lpdf(beta_serve_number_speed_plus_1_raw);
  target += std_normal_lpdf(beta_serve_number_speed_plus_1_player_raw);
  target += gamma_lpdf(sigma_beta_serve_number_speed_plus_1_player | 1, 10);
  target += normal_lpdf(beta_serve_speed_surprise_raw | 0, 0.1);
  target += normal_lpdf(beta_serve_speed_surprise_int_raw | 0, 0.1);
  target += std_normal_lpdf(beta_serve_speed_surprise_player_raw);
  target += std_normal_lpdf(beta_serve_speed_surprise_int_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_surprise_player | 1, 10);
  target += gamma_lpdf(sigma_beta_serve_speed_surprise_int_player | 1, 10);
  target += beta_lpdf(learning_rate_mean | 1, 1);
  target += gamma_lpdf(learning_rate_concentration_minus2 | 1, 10);
  target += beta_lpdf(learning_rate | learning_rate_mean * (learning_rate_concentration_minus2 + 2), 
                                      (1 - learning_rate_mean) * (learning_rate_concentration_minus2 + 2));
  
  // Likelihood
  target += normal_lpdf(serve_speed | sum_base_predictors_serve_speed + 
                                      (beta_serve_speed_surprise_raw + beta_serve_speed_surprise_player[Point_Server_id_serve_speed] + 
                                      (beta_serve_speed_surprise_int_raw + beta_serve_speed_surprise_int_player[Point_Server_id_serve_speed]) .* progress_server) .* 
                                      cumulative_surprise_server, 
                                      sigma_serve_speed);
}

generated quantities {
  vector[n_serve_speed] log_lik;
  real alpha_serve_speed = alpha_serve_speed_raw + mean(beta_serve_speed_player_raw) * sigma_beta_serve_speed_player;
  real beta_serve_speed_surprise = beta_serve_speed_surprise_raw + mean(beta_serve_speed_surprise_player);
  real beta_serve_speed_surprise_int = beta_serve_speed_surprise_int_raw + mean(beta_serve_speed_surprise_int_player);
  
  for (n in 1:n_serve_speed) {
    log_lik[n] = normal_lpdf(serve_speed[n] | sum_base_predictors_serve_speed[n] + 
                                              (beta_serve_speed_surprise_raw + beta_serve_speed_surprise_player[Point_Server_id_serve_speed[n]] + 
                                              (beta_serve_speed_surprise_int_raw + beta_serve_speed_surprise_int_player[Point_Server_id_serve_speed[n]]) * progress_server[n]) * 
                                              cumulative_surprise_server[n], 
                                              sigma_serve_speed);
  }
}