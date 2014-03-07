library(rstan)
library(plyr)

source("read_regular_season_results.R")

compiled_predictor = stan_model(file="predictor_model.txt")

mcmc_predictor = 
dlply(regular_season, .(season), function(x) {
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

save.image("predictor_mcmc.RData")

keep = c(1,12,2,4:9)
predictor_summary = ldply(mcmc_predictor, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
predictor_summary$id = levels(d$wteam)
write.csv(predictor_summary[,keep],
          file="../../data/team_statistics/predictor.csv", 
          row.names=F)


