library(plyr)
library(reshape2)
library(rstan)

load(paste(model,"_mcmc.RData",sep=""))

# Only create matchup statistics for possible tournament matchups
tmp = read.csv("../../data/sample_submission_all_seasons.csv")
submission = colsplit(as.character(tmp$id), "_", c("season","lowerid","upperid"))

# Create my id
source("read_regular_season_results.R")
submission$my_lowerid = match(submission$lowerid, levels(regular_season$wteam))
submission$my_upperid = match(submission$upperid, levels(regular_season$wteam))

# Assign name attribute to list elements for access in ldply
for(i in 1:length(mcmc)){
  attr(mcmc[[i]],"name") <- names(mcmc)[i]
}


