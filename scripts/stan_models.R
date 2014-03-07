

elo_chess_noprobs = "
data {
  int<lower=1> ngames;
  int<lower=1> nteams;
  int<lower=1> wteam[ngames];
  int<lower=1> lteam[ngames];
  int<lower=-1,upper=1> whome[ngames];
}
parameters {
  real<lower=0.001> sigma_theta;
  real theta[nteams];
  real homecourt;
}

transformed parameters {
  real<lower=0,upper=1> delta[ngames];
  for (i in 1:ngames) { 
    delta[i] <- normal_cdf(theta[wteam[i]]-theta[lteam[i]]+homecourt*whome[i], 0, 1); 
  }
}
model {
  for (t in 1:nteams) { theta[t] ~ normal(0,sigma_theta); }
  for (i in 1:ngames) { 1 ~ bernoulli(delta[i]);  }
}
"



elo_chess = "
data {
  int<lower=1> ngames;
  int<lower=1> nteams;
  int<lower=1> nprobs;
  int<lower=1> wteam[ngames];
  int<lower=1> lteam[ngames];
  int<lower=1> team1[nprobs];
  int<lower=1> team2[nprobs];
  int<lower=-1,upper=1> whome[ngames];
}
parameters {
  real<lower=0.001> sigma_theta;
  real theta[nteams];
  real homecourt;
}

transformed parameters {
  real<lower=0,upper=1> delta[ngames];
  for (i in 1:ngames) { 
    delta[i] <- normal_cdf(theta[wteam[i]]-theta[lteam[i]]+homecourt*whome[i], 0, 1); 
  }
}
model {
  for (t in 1:nteams) { theta[t] ~ normal(0,sigma_theta); }
  for (i in 1:ngames) { 1 ~ bernoulli(delta[i]);  }
}
generated quantities {
  real<lower=0,upper=1> prob[nprobs];
  for (i in 1:nprobs) { prob[i] <- normal_cdf(theta[team1[i]]-theta[team2[i]],0,1); }
}
"


## Predictor

predictor_noprobs = "
data {
  int<lower=1> ngames;
  int<lower=1> nteams;
  int<lower=1> wteam[ngames];
  int<lower=1> lteam[ngames];
  int<lower=1> wteam_score[ngames];
  int<lower=1> lteam_score[ngames];
  int<lower=-1,upper=1> whome[ngames];
}
transformed data {
  int diff[ngames];
  for (i in 1:ngames) { diff[i] <- wteam_score[i] - lteam_score[i]; }
}
parameters {
  real<lower=0.001> sigma_theta;
  real<lower=0.001, upper=50> sigma;
  real theta[nteams];
  real homecourt;
}
model {
  for (t in 1:nteams) { theta[t] ~ normal(0,sigma_theta); }
  for (i in 1:ngames) { diff[i] ~ normal(theta[wteam[i]]-theta[lteam[i]]+homecourt*whome[i], sigma);  }
}
"



predictor = "
data {
  int<lower=1> ngames;
  int<lower=1> nteams;
  int<lower=1> nprobs;
  int<lower=1> wteam[ngames];
  int<lower=1> lteam[ngames];
  int<lower=1> wteam_score[ngames];
  int<lower=1> lteam_score[ngames];
  int<lower=1> team1[nprobs];
  int<lower=1> team2[nprobs];
  int<lower=-1,upper=1> whome[ngames];
}
transformed data {
  int diff[ngames];
  for (i in 1:ngames) { diff[i] <- wteam_score[i] - lteam_score[i]; }
}
parameters {
  real<lower=0.001> sigma_theta;
  real<lower=0.001, upper=50> sigma;
  real theta[nteams];
  real homecourt;
}
model {
  for (t in 1:nteams) { theta[t] ~ normal(0,sigma_theta); }
  for (i in 1:ngames) { diff[i] ~ normal(theta[wteam[i]]-theta[lteam[i]]+homecourt*whome[i], sigma);  }
}
generated quantities {
  real<lower=0,upper=1> prob[nprobs];
  for (i in 1:nprobs) { prob[i] <- normal_cdf(theta[team1[i]]-theta[team2[i]],0,sigma); }
}
"
