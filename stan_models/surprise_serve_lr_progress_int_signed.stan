data {
  int<lower=0> n_score;
  int<lower=0> n_serve_speed;
  int<lower=0> n_players;
  int<lower=0> n_players_enough;
  int players_enough_indices[n_players_enough];
  int<lower=0> n_matches;
  int<lower=0> n_serve_speed_matches;
  int Point_Server_id[n_score];
  int Point_Server_id_serve_speed[n_serve_speed];
  int Point_non_Server_id[n_score];
  int<lower=0> id1[n_score];
  int<lower=0> id2[n_score];
  int<lower=0> match_id[n_score];
  int<lower=0> serve_speed_match_id[n_serve_speed];
  int<lower=0,upper=1> y[n_score];
  vector<lower=-1,upper=1>[n_score] player1_served;
  vector<lower = -1, upper=1>[n_score] sign;
  vector[n_score] progress_1minus2;
  vector[n_serve_speed] progress_server;
  int<lower=0> is_serve_speed_indices[n_serve_speed];
  int<lower=0> n_players_with_serve_speed;
  int<lower=0> n_players_with_serve_speed_enough;
  int players_with_serve_speed_enough_indices[n_players_with_serve_speed_enough];
  vector[n_score] ServeNumber;
  real serve_speed[n_serve_speed];
  int<lower=0> PointServer[n_score];
}

transformed data {
  int N = n_score + n_serve_speed;
}


parameters {
  real alpha_raw;
  real alpha_serve_speed_raw;
  real alpha_serve_speed_int;
  real<lower=0> sigma_serve_speed;
  
  vector[n_players] beta_player_raw;
  real<lower=0> sigma_beta_player;
  vector[n_players_enough] beta_player_int_raw;
  real<lower=0> sigma_beta_player_int;
  
  vector[n_players_with_serve_speed] beta_serve_speed_player_raw;
  real<lower=0> sigma_beta_serve_speed_player;
  vector[n_players_with_serve_speed_enough] beta_serve_speed_player_int_raw;
  real<lower=0> sigma_beta_serve_speed_player_int;
  
  real beta_serve_number_speed_plus_1_raw;
  vector[n_players_with_serve_speed_enough] beta_serve_number_speed_plus_1_player_raw;
  real<lower=0> sigma_beta_serve_number_speed_plus_1_player;
  
  vector[n_matches] beta_match_raw;
  real<lower=0> sigma_match;
  
  matrix[n_serve_speed_matches,2] beta_serve_speed_match_raw;
  real<lower=0> sigma_serve_speed_match;

  real beta_surprise_raw;
  real beta_surprise_int_raw;
  vector[n_players_enough] beta_surprise_player_raw;
  vector[n_players_enough] beta_surprise_int_player_raw;
  real<lower=0> sigma_beta_surprise_player;
  real<lower=0> sigma_beta_surprise_int_player;
  
  real beta_serve_speed_surprise_raw;
  real beta_serve_speed_surprise_int_raw;
  vector [n_players_with_serve_speed_enough] beta_serve_speed_surprise_int_player_raw;
  vector[n_players_with_serve_speed_enough] beta_serve_speed_surprise_player_raw;
  real<lower=0> sigma_beta_serve_speed_surprise_player;
  real<lower=0> sigma_beta_serve_speed_surprise_int_player;
  
  real<lower = 0, upper = 1> learning_rate_mean;
  real<lower = 0> learning_rate_concentration_minus2;
  vector<lower = 0, upper = 1>[n_players] learning_rate;
}

transformed parameters {
  vector[n_score] surprise;
  vector[n_score] cumulative_surprise_player1;
  vector[n_score] cumulative_surprise_player2;
  vector[n_score] cumulative_surprise_server;
  vector<lower = 0, upper = 1>[n_score] p;
  real alpha = alpha_raw + mean(beta_player_raw)*sigma_beta_player;
  vector[n_players] beta_player = (beta_player_raw - mean(beta_player_raw))*sigma_beta_player;
  vector[n_matches] beta_match = beta_match_raw*sigma_match;
  matrix[n_serve_speed_matches,2] beta_serve_speed_match = beta_serve_speed_match_raw*sigma_serve_speed_match;
  vector[n_players_with_serve_speed] beta_serve_speed_player = beta_serve_speed_player_raw*sigma_beta_serve_speed_player;
  vector[n_players_with_serve_speed] beta_serve_number_speed_plus_1 = rep_vector(beta_serve_number_speed_plus_1_raw, n_players_with_serve_speed);
  vector[n_score] sum_base_predictors;
  vector[n_serve_speed] sum_base_predictors_serve_speed; 
  vector[n_players] beta_player_int_dev = rep_vector(0, n_players);
  vector[n_players] beta_surprise_player_dev = rep_vector(0, n_players);
  vector[n_players] beta_surprise_int_player_dev = rep_vector(0, n_players);
  vector[n_score] beta_surprise_player1;
  vector[n_score] beta_surprise_player2; 
  vector[n_players_with_serve_speed] beta_serve_speed_player_int_dev = rep_vector(0,n_players_with_serve_speed);
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_int_player_dev = rep_vector(0,n_players_with_serve_speed);
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_player_dev = rep_vector(0,n_players_with_serve_speed);
  vector[n_serve_speed] beta_serve_speed_surprise_server;
  beta_player_int_dev[players_enough_indices] = beta_player_int_raw*sigma_beta_player_int;
  sum_base_predictors = alpha + beta_player[Point_Server_id] - beta_player[Point_non_Server_id] + beta_match[match_id].*player1_served + (beta_player_int_dev[Point_Server_id] - beta_player_int_dev[Point_non_Server_id]).*progress_1minus2;
  beta_serve_number_speed_plus_1[players_with_serve_speed_enough_indices] = beta_serve_number_speed_plus_1[players_with_serve_speed_enough_indices] + beta_serve_number_speed_plus_1_player_raw*sigma_beta_serve_number_speed_plus_1_player;
  beta_serve_speed_player_int_dev[players_with_serve_speed_enough_indices] = beta_serve_speed_player_int_raw*sigma_beta_serve_speed_player_int;
  sum_base_predictors_serve_speed = alpha_serve_speed_raw + (alpha_serve_speed_int + beta_serve_speed_player_int_dev[Point_Server_id_serve_speed]).*progress_server + beta_serve_speed_player[Point_Server_id_serve_speed] + (beta_serve_number_speed_plus_1[Point_Server_id_serve_speed] - 1) .* (ServeNumber[is_serve_speed_indices]-1.5)  + beta_serve_speed_match[serve_speed_match_id,1].*(2 - to_vector(PointServer[is_serve_speed_indices]))  + beta_serve_speed_match[serve_speed_match_id,2].*(to_vector(PointServer[is_serve_speed_indices]) - 1);
  
  beta_surprise_player_dev[players_enough_indices] = beta_surprise_player_raw*sigma_beta_surprise_player;
  beta_surprise_int_player_dev[players_enough_indices] = beta_surprise_int_player_raw*sigma_beta_surprise_int_player;
  beta_surprise_player1 = beta_surprise_raw + beta_surprise_player_dev[id1] + (beta_surprise_int_raw + beta_surprise_int_player_dev[id1]).*progress_1minus2;
  beta_surprise_player2 = beta_surprise_raw + beta_surprise_player_dev[id2] + (beta_surprise_int_raw + beta_surprise_int_player_dev[id2]).*progress_1minus2;
  beta_serve_speed_surprise_int_player_dev[players_with_serve_speed_enough_indices] = beta_serve_speed_surprise_int_player_raw*sigma_beta_serve_speed_surprise_int_player;
  beta_serve_speed_surprise_player_dev[players_with_serve_speed_enough_indices] = beta_serve_speed_surprise_player_raw*sigma_beta_serve_speed_surprise_player;
  beta_serve_speed_surprise_server = beta_serve_speed_surprise_raw + beta_serve_speed_surprise_player_dev[Point_Server_id_serve_speed] + (beta_serve_speed_surprise_int_raw+beta_serve_speed_surprise_int_player_dev[Point_Server_id_serve_speed]).*progress_server;
  
  // compute surprise
  p = inv_logit(sum_base_predictors);
  surprise = -(to_vector(y) .* log(p) + (1 - to_vector(y)) .* log(1 - p));
  surprise = sign .* surprise; 
  cumulative_surprise_player1[1] = 0;
  cumulative_surprise_player2[1] = 0;
  cumulative_surprise_server[1] = 0;
  for (i in 2:n_score){
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
  target += gamma_lpdf(sigma_beta_player|1,10);
  target += std_normal_lpdf(beta_player_int_raw);
  target += gamma_lpdf(sigma_beta_player_int|1,10);
  
  target += std_normal_lpdf(beta_match_raw);
  target += gamma_lpdf(sigma_match|1,10);
  
  target += normal_lpdf(beta_surprise_raw|0,0.1);
  target += normal_lpdf(beta_surprise_int_raw|0,0.1);
  target += std_normal_lpdf(beta_surprise_player_raw);
  target += std_normal_lpdf(beta_surprise_int_player_raw);
  target += gamma_lpdf(sigma_beta_surprise_player|1,10);
  target += gamma_lpdf(sigma_beta_surprise_int_player|1,10);
  
  target += beta_lpdf(learning_rate_mean|1,1);
  target += gamma_lpdf(learning_rate_concentration_minus2|1,10);
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
  target += normal_lpdf(beta_serve_speed_surprise_int_raw|0,0.1);
  target += std_normal_lpdf(beta_serve_speed_surprise_player_raw);
  target += std_normal_lpdf(beta_serve_speed_surprise_int_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_surprise_player|1,10);
  target += gamma_lpdf(sigma_beta_serve_speed_surprise_int_player|1,10);

  target += std_normal_lpdf(alpha_serve_speed_raw);
  target += normal_lpdf(alpha_serve_speed_int|0,0.1);
  target += std_normal_lpdf(beta_serve_speed_player_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_player|1,10);
  target += std_normal_lpdf(beta_serve_speed_player_int_raw);
  target += gamma_lpdf(sigma_beta_serve_speed_player_int|1,10);
  
  target += std_normal_lpdf(to_vector(beta_serve_speed_match_raw));
  target += gamma_lpdf(sigma_serve_speed_match|1,10);
  
  
  target += std_normal_lpdf(beta_serve_number_speed_plus_1_raw);
  target += std_normal_lpdf(beta_serve_number_speed_plus_1_player_raw);
  target += gamma_lpdf(sigma_beta_serve_number_speed_plus_1_player|1,10);
  target += gamma_lpdf(sigma_serve_speed|1,10);
}

generated quantities{
  vector[N] log_lik;
  vector[n_players_with_serve_speed] beta_serve_number_speed_player = rep_vector(beta_serve_number_speed_plus_1_raw - 1, n_players_with_serve_speed);
  real beta_serve_number_speed;
  real beta_surprise = beta_surprise_raw + mean(beta_surprise_player_dev);
  vector[n_players] beta_surprise_int_player = beta_surprise_int_raw + beta_surprise_int_player_dev;
  real beta_surprise_int = mean(beta_surprise_int_player);
  real beta_serve_speed_surprise = beta_serve_speed_surprise_raw + mean(beta_serve_speed_surprise_player_dev);
  real beta_serve_speed_surprise_int = beta_serve_speed_surprise_int_raw + mean(beta_serve_speed_surprise_int_player_dev);
  real alpha_serve_speed = alpha_serve_speed_raw + mean(beta_serve_speed_player_raw)*sigma_beta_serve_speed_player;
  vector[n_players] beta_surprise_player = beta_surprise_player_dev + beta_surprise_raw;
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_player = beta_serve_speed_surprise_raw + beta_serve_speed_surprise_player_dev;
  vector[n_players_with_serve_speed] beta_serve_speed_surprise_int_player = beta_serve_speed_surprise_int_raw + beta_serve_speed_surprise_int_player_dev;
  beta_serve_number_speed_player[players_with_serve_speed_enough_indices] = beta_serve_number_speed_player[players_with_serve_speed_enough_indices]  + beta_serve_number_speed_plus_1_player_raw*sigma_beta_serve_number_speed_plus_1_player;
  beta_serve_number_speed = mean(beta_serve_number_speed_player);
  for (n in 1:n_score) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | sum_base_predictors[n] 
                                  + beta_surprise_player1[n] .* cumulative_surprise_player1[n] 
                                  + beta_surprise_player2[n] .* cumulative_surprise_player2[n]
                                );
  }
  for (n in (n_score+1):N){
     log_lik[n] = normal_lpdf(serve_speed[n-n_score] | sum_base_predictors_serve_speed[n-n_score] 
                                  + beta_serve_speed_surprise_server[n-n_score] .* cumulative_surprise_server[is_serve_speed_indices[n-n_score]]
                                  , sigma_serve_speed
                                );
  }
}

