library(rstan)
library(plyr)

source("read_regular_season_results.R")

# Pre-compiled model
compiled_elo_chess = stan_model(file="elo_chess_model.txt")

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



