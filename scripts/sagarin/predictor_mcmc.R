library(rstan)
library(plyr)

source("read_regular_season_results.R")

compiled_model = stan_model(file="predictor_model.txt")

# Run MCMC for all seasons
mcmc = 
dlply(regular_season, .(season), function(x) {
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
             wteam  = as.numeric(x$wteam),
             lteam  = as.numeric(x$lteam),
             wteam_score = x$wscore,
             lteam_score = x$lscore,
             whome = (x$wloc=="H") - (x$wloc=="A"))
    
  m = sampling(compiled_model, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","sigma","homecourt"))
  return(m)
})

save.image("predictor_mcmc.RData")




