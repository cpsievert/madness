library(rstan)
library(plyr)

source("read_regular_season_results.R")

compiled_model = stan_model(file="elo_chess_model.txt")

# Run MCMC for all seasons
mcmc = 
dlply(d, .(season), function(x) {
  tourney = t[t$season==as.character(unique(x$season)),]
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
             wteam = as.numeric(x$wteam),
             lteam = as.numeric(x$lteam),
             whome = (x$wloc=="H") - (x$wloc=="A"))

  m = sampling(compiled_model, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","homecourt"))
  return(m)
})


save.image("elo_chess_mcmc.RData")

keep = c(1,12,2,4:9)
summary = ldply(mcmc, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
summary$id = levels(d$wteam)
write.csv(summary[,keep],
          file="../../data/team_statistics/elo_chess.csv", 
          row.names=F)



