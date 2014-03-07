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
"


compiled_predictor = stan_model(model_code=predictor)

mcmc_predictor = 
dlply(d, .(season), function(x) {
  tourney = t[t$season==as.character(unique(x$season)),]
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
             wteam = as.numeric(x$wteam),
             lteam = as.numeric(x$lteam),
             wteam_score = x$wscore,
             lteam_score = x$lscore,
             whome = (x$wloc=="H") - (x$wloc=="A"))
    
  m = sampling(compiled_predictor, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","sigma","homecourt"))
  return(m)
})




save.image("predictor.RData")

keep = c(1,12,2,4:9)
predictor_summary = ldply(mcmc_predictor, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
predictor_summary$id = levels(d$wteam)
write.csv(predictor_summary[,keep],
          file="predictor.csv", 
          row.names=F)


