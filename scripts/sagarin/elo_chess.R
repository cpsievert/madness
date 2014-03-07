library(rstan)
library(plyr)
library(reshape2)

# Season data
d = read.csv("../raw/regular_season_results.csv")
d$wteam = factor(d$wteam)
d$lteam = factor(d$lteam)
stopifnot(all.equal(levels(d$wteam), levels(d$lteam)))

# Submission data
t = read.csv("../raw/sample_submission.csv")
t$season = factor(substr(t$id,1,1))
t$team1  = factor(substr(t$id,3,5), levels(d$wteam))
t$team2  = factor(substr(t$id,7,9), levels(d$wteam))

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
"

# Pre-compiled model
compiled_elo_chess = stan_model(model_code=elo_chess_noprobs)

# Run MCMC for submission seasons
mcmc_elo_chess = 
dlply(d, .(season), function(x) {
  tourney = t[t$season==as.character(unique(x$season)),]
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
             wteam = as.numeric(x$wteam),
             lteam = as.numeric(x$lteam),
             whome = (x$wloc=="H") - (x$wloc=="A"))

  m = sampling(compiled_elo_chess, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","homecourt"))
  return(m)
})




save.image("elo_chess.RData")

keep = c(1,12,2,4:9)
elo_chess_summary = ldply(mcmc_elo_chess, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
elo_chess_summary$id = levels(d$wteam)
write.csv(elo_chess_summary[,keep],
          file="../../data/team_statistics/elo_chess.csv", 
          row.names=F)



