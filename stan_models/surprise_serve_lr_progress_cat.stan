data {
  int<lower=0> N;
  int<lower=0> n_serve_speed;
  int<lower=0> n_players;
  int<lower=0> n_matches;
  int<lower=0> n_serve_speed_matches;
  int Point_Server_id[N];
  int Point_Server_id_serve_speed[n_serve_speed];
  int Point_non_Server_id[N];
  int<lower=0> id1[N];
  int<lower=0> id2[N];
  int<lower=0> match_id[N];
  int<lower=0> serve_speed_match_id[n_serve_speed];
  int<lower=0,upper=1> y[N];
  vector<lower=-1,upper=1>[N] player1_served;
  vector<lower = -1, upper=1>[N] sign;
  int<lower=1> progress_1minus2[N];
  int<lower=1> progress_2minus1[N];
  int<lower=1> progress_server[n_serve_speed];
  int<lower=0> is_serve_speed_indices[n_serve_speed];
  int<lower=0> n_players_with_serve_speed;
  vector[N] ServeNumber;
  real serve_speed[n_serve_speed];
  int<lower=0> PointServer[N];
  int<lower=1> n_categories;
}

parameters {
  real alpha_raw;
  vector[n_categories] alpha_serve_speed_raw;
  real beta_serve_number_speed_plus_1;
  real<lower=0> sigma_serve_speed;
  
  vector[n_players] beta_player_raw;
  real<lower=0> sigma_beta_player;
  
  vector[n_players_with_serve_speed] beta_serve_speed_player_raw;
  real<lower=0> sigma_beta_serve_speed_player;
  
  vector[n_matches] beta_match_raw;
  real<lower=0> sigma_match;
  
  matrix[n_serve_speed_matches,2] beta_serve_speed_match_raw;
  real<lower=0> sigma_serve_speed_match;
  
  vector[n_categories] beta_surprise_raw;
  vector[n_players] beta_surprise_player_raw;
  real<lower=0> sigma_beta_surprise_player;
  
  vector[n_categories] beta_serve_speed_surprise_raw;
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_player_raw;
  real<lower=0> sigma_beta_serve_speed_surprise_player;
  
  real<lower = 0, upper = 1> learning_rate_mean;
  real<lower = 0> learning_rate_concentration_minus2;
  vector<lower = 0, upper = 1>[n_players] learning_rate;
}

transformed parameters {
  vector[N] surprise;
  vector[N] cumulative_surprise_player1;
  vector[N] cumulative_surprise_player2;
  vector[N] cumulative_surprise_server;
  vector<lower = 0, upper = 1>[N] p;
  real alpha = alpha_raw + mean(beta_player_raw)*sigma_beta_player;
  vector[n_players] beta_player = (beta_player_raw - mean(beta_player_raw))*sigma_beta_player;
  vector[n_matches] beta_match = beta_match_raw*sigma_match;
  matrix[n_serve_speed_matches,2] beta_serve_speed_match = beta_serve_speed_match_raw*sigma_serve_speed_match;
  vector[n_players_with_serve_speed] beta_serve_speed_player = beta_serve_speed_player_raw*sigma_beta_serve_speed_player;
  vector[N] sum_base_predictors = alpha + beta_player[Point_Server_id] - beta_player[Point_non_Server_id] + beta_match[match_id].*player1_served;
  vector[n_serve_speed] sum_base_predictors_serve_speed = alpha_serve_speed_raw[progress_server] + beta_serve_speed_player[Point_Server_id_serve_speed] + (beta_serve_number_speed_plus_1 - 1) * (ServeNumber[is_serve_speed_indices]-1.5)  + beta_serve_speed_match[serve_speed_match_id,1].*(2 - to_vector(PointServer[is_serve_speed_indices]))  + beta_serve_speed_match[serve_speed_match_id,2].*(to_vector(PointServer[is_serve_speed_indices]) - 1);
  vector[N] beta_surprise_player1 = beta_surprise_raw[progress_1minus2] + beta_surprise_player_raw[id1]*sigma_beta_surprise_player;
  vector[N] beta_surprise_player2 = beta_surprise_raw[progress_2minus1] + beta_surprise_player_raw[id2]*sigma_beta_surprise_player;
  vector[n_serve_speed] beta_serve_speed_surprise_server = beta_serve_speed_surprise_raw[progress_server] + beta_serve_speed_surprise_player_raw[Point_Server_id_serve_speed]*sigma_beta_serve_speed_surprise_player;
  
  // compute surprise
  p = inv_logit(sum_base_predictors);
  surprise = -(to_vector(y) .* log(p) + (1 - to_vector(y)) .* log(1 - p));
  surprise = sign .* surprise; 
  cumulative_surprise_player1[1] = 0;
  cumulative_surprise_player2[1] = 0;
  cumulative_surprise_server[1] = 0;
  for (i in 2:N){
    if (match_id[i] != match_id[i-1]){
       cumulative_surprise_player1[i] = 0;
       cumulative_surprise_player2[i] = 0;
       cumulative_surprise_server[i] = 0;
    }
    else {
      cumulative_surprise_player1[i] =  cumulative_surprise_player1[i-1]* (1-learning_rate[id1[i-1]]) + surprise[i-1]*learning_rate[id1[i-1]];
      cumulative_surprise_player2[i] =  cumulative_surprise_player2[i-1]* (1-learning_rate[id2[i-1]]) + surprise[i-1]*learning_rate[id2[i-1]];
      if (player1_served[i] != player1_served[i-1]){
        cumulative_surprise_player1[i] = -cumulative_surprise_player1[i];
        cumulative_surprise_player2[i] = -cumulative_surprise_player2[i];
      }
      if (PointServer[i] == 1) cumulative_surprise_server[i] = cumulative_surprise_player1[i];
      else if (PointServer[i] == 2) cumulative_surprise_server[i] = cumulative_surprise_player2[i];
    }
  }

}

model {
  target += std_normal_lpdf(alpha);
  
  target += std_normal_lpdf(beta_player_raw);
  target += gamma_lpdf(sigma_beta_player|1,1);
  
  target += std_normal_lpdf(beta_match_raw);
  target += gamma_lpdf(sigma_match|1,1);
  
  target += normal_lpdf(beta_surprise_raw|0,0.1);
  target += std_normal_lpdf(beta_surprise_player_raw);
  target += gamma_lpdf(sigma_beta_surprise_player|1,1);
  
  target += beta_lpdf(learning_rate_mean|9,1);
  target += gamma_lpdf(learning_rate_concentration_minus2|1,1);
  target += beta_lpdf(learning_rate| learning_rate_mean*(learning_rate_concentration_minus2+2), (1-learning_rate_mean)*(learning_rate_concentration_minus2+2));
  
  target += bernoulli_logit_lpmf(y | sum_base_predictors 
                                  + beta_surprise_player1 .* cumulative_surprise_player1 
                                  + beta_surprise_player2 .* cumulative_surprise_player2
                                );
                                
  target += normal_lpdf(serve_speed | sum_base_predictors_serve_speed 
                                  + beta_serve_speed_surprise_server .* cumulative_surprise_server[is_serve_speed_indices]
                                  , sigma_serve_speed
                                );
                                
  target += normal_lpdf(beta_serve_speed_surprise_raw|0,0.1);
  target += std_normal_lpdf(beta_serve_speed_surprise_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_surprise_player|1,1);

  target += std_normal_lpdf(alpha_serve_speed_raw);
  target += std_normal_lpdf(beta_serve_speed_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_player|1,1);
  
  target += std_normal_lpdf(to_vector(beta_serve_speed_match_raw));
  target += gamma_lpdf(sigma_serve_speed_match|1,1);
  
  target += std_normal_lpdf(beta_serve_number_speed_plus_1);
  target += gamma_lpdf(sigma_serve_speed|1,1);
}

generated quantities{
  vector[n_categories] beta_surprise = beta_surprise_raw + mean(beta_surprise_player_raw*sigma_beta_surprise_player);
  vector[n_categories] beta_serve_speed_surprise = beta_serve_speed_surprise_raw + mean(beta_serve_speed_surprise_player_raw*sigma_beta_serve_speed_surprise_player);
  vector[n_categories] alpha_serve_speed = alpha_serve_speed_raw + mean(beta_serve_speed_player_raw)*sigma_beta_serve_speed_player;
  vector[n_players] beta_surprise_player = beta_surprise_player_raw*sigma_beta_surprise_player;
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_player = beta_serve_speed_surprise_player_raw*sigma_beta_serve_speed_surprise_player;
}

