data {
  int<lower=0> N;
  int<lower=0> n_players;
  int<lower=0> n_matches;
  int Point_Server_id[N];
  int Point_non_Server_id[N];
  int<lower=0> id1[N];
  int<lower=0> id2[N];
  int<lower=0> match_id[N];
  int<lower=0,upper=1> y[N];
  vector<lower=-1,upper=1>[N] player1_served;
  vector<lower = -1, upper=1>[N] sign;
  vector[N] progress_1minus2;
}

parameters {
  real alpha;
  
  vector[n_players] beta_player_raw;
  real<lower=0> sigma_beta_player;
  
  vector[n_matches] beta_match_raw;
  real<lower=0> sigma_match;
  
  real beta_surprise_raw;
  vector[n_players] beta_surprise_player_raw;
  real<lower=0> sigma_beta_surprise_player;
  
  real beta_surpriseXprogress_raw;
  vector[n_players] beta_surpriseXprogress_player_raw;
  real<lower=0> sigma_beta_surpriseXprogress_player;
  
  real<lower = 0, upper = 1> decay_mean_raw;
  real<lower = 0> decay_concentration_minus2;
  vector<lower = 0, upper = 1>[n_players] decay;
}

transformed parameters {
  vector[N] surprise;
  vector[n_matches] beta_match_updated;
  vector[N] cumulative_surprise_player1;
  vector[N] cumulative_surprise_player2;
  vector<lower = 0, upper = 1>[N] p;
  vector[N] sum_base_predictors;
  
  beta_match_updated[match_id] = (beta_match_raw[match_id]).*player1_served*sigma_match;
  sum_base_predictors = alpha + beta_player_raw[Point_Server_id]*sigma_beta_player - beta_player_raw[Point_non_Server_id]*sigma_beta_player + beta_match_updated[match_id];
  p = inv_logit(sum_base_predictors);
  surprise = -(to_vector(y) .* log(p) + (1 - to_vector(y)) .* log(1 - p));
  surprise = sign .* surprise; //vector of -1 if y==0 and 1 if y==1
  cumulative_surprise_player1[1] = 0;
  cumulative_surprise_player2[1] = 0;
  for (i in 2:N){
    if (match_id[i] != match_id[i-1]){
       cumulative_surprise_player1[i] = 0;
       cumulative_surprise_player2[i] = 0;
    } else {
      cumulative_surprise_player1[i] =  cumulative_surprise_player1[i-1]* decay[id1[i-1]] + surprise[i-1];
      cumulative_surprise_player2[i] =  cumulative_surprise_player2[i-1]* decay[id2[i-1]] + surprise[i-1];
      if (player1_served[i] != player1_served[i-1]){
        cumulative_surprise_player1[i] = -cumulative_surprise_player1[i];
        cumulative_surprise_player2[i] = -cumulative_surprise_player2[i];
      }
    }
  }
}

model {
  target += std_normal_lpdf(alpha);
  
  target += std_normal_lpdf(beta_player_raw);
  target += gamma_lpdf(sigma_beta_player|1,1);
  
  target += std_normal_lpdf(beta_match_raw);
  target += gamma_lpdf(sigma_match|1,1);
  
  target += std_normal_lpdf(beta_surprise_raw);
  target += std_normal_lpdf(beta_surprise_player_raw);
  target += gamma_lpdf(sigma_beta_surprise_player|1,1);
  
  target += std_normal_lpdf(beta_surpriseXprogress_raw);

  target += beta_lpdf(decay_mean_raw|9,1);
  target += gamma_lpdf(decay_concentration_minus2|1,1);
  target += beta_lpdf(decay| decay_mean_raw*(decay_concentration_minus2+2), (1-decay_mean_raw)*(decay_concentration_minus2+2));
  
  target += bernoulli_logit_lpmf(y | sum_base_predictors 
                                  + (beta_surprise_raw + beta_surprise_player_raw[id1]*sigma_beta_surprise_player + (beta_surpriseXprogress_raw + beta_surpriseXprogress_player_raw[id1]*sigma_beta_surpriseXprogress_player).*progress_1minus2).* cumulative_surprise_player1 
                                  + (beta_surprise_raw+beta_surprise_player_raw[id2]*sigma_beta_surprise_player + (beta_surpriseXprogress_raw + beta_surpriseXprogress_player_raw[id2]*sigma_beta_surpriseXprogress_player).*-progress_1minus2).* cumulative_surprise_player2
                                );
}

generated quantities{
  vector[n_players] beta_player = (beta_player_raw - mean(beta_player_raw))*sigma_beta_player;
  vector[n_matches] beta_match = beta_match_raw*sigma_match;
  
  real beta_surprise = beta_surprise_raw + mean(beta_surprise_player_raw*sigma_beta_surprise_player);
  vector[n_players] beta_surprise_player = beta_surprise_raw + beta_surprise_player_raw*sigma_beta_surprise_player;
  
  real beta_surpriseXprogress = beta_surpriseXprogress_raw + mean(beta_surpriseXprogress_player_raw*sigma_beta_surpriseXprogress_player);
  vector[n_players] beta_surpriseXprogress_player = beta_surpriseXprogress_raw + beta_surpriseXprogress_player_raw*sigma_beta_surpriseXprogress_player;

  real decay_mean = decay_mean_raw + mean(decay);
}

