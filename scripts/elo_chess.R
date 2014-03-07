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

# Keep only seasons needed for submission
#d = d[d$season %in% levels(t$season),]


source("stan_models.R")

# Pre-compiled model
compiled_elo_chess = stan_model(model_code=elo_chess_noprobs)

# Run MCMC for submission seasons
mcmc_elo_chess = 
dlply(d, .(season), function(x) {
  tourney = t[t$season==as.character(unique(x$season)),]
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
#             nprobs = nrow(tourney),
             wteam = as.numeric(x$wteam),
             lteam = as.numeric(x$lteam),
#             team1 = as.numeric(tourney$team1),
#             team2 = as.numeric(tourney$team2),
             whome = (x$wloc=="H") - (x$wloc=="A"))

  m = sampling(compiled_elo_chess, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","homecourt")) # put prob back in
  return(m)
})





compiled_predictor = stan_model(model_code=predictor_noprobs)

mcmc_predictor = 
dlply(d, .(season), function(x) {
  tourney = t[t$season==as.character(unique(x$season)),]
  dat = list(ngames = nrow(x),
             nteams = nlevels(x$wteam),
#             nprobs = nrow(tourney),
             wteam = as.numeric(x$wteam),
             lteam = as.numeric(x$lteam),
             wteam_score = x$wscore,
             lteam_score = x$lscore,
#             team1 = as.numeric(tourney$team1),
#             team2 = as.numeric(tourney$team2),
             whome = (x$wloc=="H") - (x$wloc=="A"))
    
  m = sampling(compiled_predictor, data=dat, verbose=FALSE, 
               pars = c("theta","sigma_theta","sigma","homecourt")) # put prob back in
  return(m)
})




save.image("ratings.RData")

keep = c(1,12,2,4:9)
elo_chess_summary = ldply(mcmc_elo_chess, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
elo_chess_summary$id = levels(d$wteam)
write.csv(elo_chess_summary[,keep],file="elo_chess_summary.csv", row.names=F)

predictor_summary = ldply(mcmc_predictor, 
  function(x) as.data.frame(summary(x)$summary[1:nlevels(d$wteam),]))
predictor_summary$id = levels(d$wteam)
write.csv(predictor_summary[,keep],file="predictor_summary.csv", row.names=F)


