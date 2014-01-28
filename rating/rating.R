library(rstan)
library(plyr)
library(reshape2)


d = read.csv("../raw/regular_season_results.csv")

d$wteam = factor(d$wteam)
d$lteam = factor(d$lteam)

stopifnot(all.equal(levels(d$wteam), levels(d$lteam)))

# Quick check for number of teams in each season
ddply(d, .(season), summarize,
      n_wteam = length(unique(wteam)),
      n_lteam = length(unique(lteam)))

# Team records
tmp = ddply(d, .(season, wteam), summarize, n_wins = length(wscore))

m = melt(d[,c("season","wteam","lteam")], id.var="season")
m$one = 1
tmp= ddply(m, .(season, variable, value), summarize,
           n = length(one))
dd = dcast(tmp, variable~season+value~variable)


elo_chess = "
data {
  int<lower=1> n;
  int<lower=1> nteams;
  int<lower=1> wteam[n];
  int<lower=1> lteam[n];
}
parameters {
  real<lower=0.001> sigma_theta;
  real theta[nteams];
}

transformed parameters {
  real<lower=0,upper=1> delta[n];
  for (i in 1:n) { 
    delta[i] <- normal_cdf(theta[wteam[i]]-theta[lteam[i]], 0, 1);
  }
}
model {
  for (t in 1:nteams) {
    theta[t] ~ normal(0,sigma_theta);
  }
  for (i in 1:n) {
    1 ~ bernoulli(delta[i]);
  }
}
"

# Test using season Q
dQ = d[d$season=="Q",]
dQ$wteam = factor(dQ$wteam)
dQ$lteam = factor(dQ$lteam)

dat = list(n = nrow(dQ),
           nteams = nlevels(dQ$wteam),
           wteam = as.numeric(dQ$wteam),
           lteam = as.numeric(dQ$lteam))

m = stan(model_code = elo_chess, data=dat, iter = 1e3, verbose=FALSE, 
         pars = c("theta","sigma_theta"))
print(m)
         
