data {
  int<lower=0> n_score;
  int<lower=0> n_players;
  int<lower=0> n_players_enough;
  array[n_players_enough] int players_enough_indices;
  int<lower=0> n_matches;
  array[n_score] int Point_Server_id;
  array[n_score] int Point_non_Server_id;
  array[n_score] int<lower=0> id1;
  array[n_score] int<lower=0> id2;
  array[n_score] int<lower=0> match_id;
  array[n_score] int<lower=0,upper=1> y;
  array[n_score] real<lower=-1,upper=1> player1_served;
}

parameters {
  real alpha_raw;
  array[n_players] real beta_player_raw;
  real<lower=0> sigma_beta_player;
  array[n_matches] real beta_match_raw;
  real<lower=0> sigma_match;
}

transformed parameters {
  array[n_score] real<lower=0,upper=1> p;
  real alpha = alpha_raw + mean(beta_player_raw)*sigma_beta_player;
  vector[n_players] beta_player = (to_vector(beta_player_raw) - mean(beta_player_raw))*sigma_beta_player;
  vector[n_matches] beta_match = to_vector(beta_match_raw)*sigma_match;
  vector[n_score] sum_base_predictors;

  sum_base_predictors = alpha + beta_player[Point_Server_id] - beta_player[Point_non_Server_id] 
    + beta_match[match_id] .* to_vector(player1_served);

}

model {
  // Priors
  target += std_normal_lpdf(alpha_raw);
  target += std_normal_lpdf(to_vector(beta_player_raw));
  target += gamma_lpdf(sigma_beta_player|1,10);
  target += std_normal_lpdf(to_vector(beta_match_raw));
  target += gamma_lpdf(sigma_match|1,10);

  // Likelihood
  target += bernoulli_logit_lpmf(y | sum_base_predictors);
}

generated quantities {
  array[n_score] real sum_base_predictors_save;
  real mean_base;

  sum_base_predictors_save = to_array_1d(sum_base_predictors);
  mean_base = mean(sum_base_predictors);

}