library(plyr)
library(reshape2)

# Reads in a result file and creates a data.frame similar to the submission
# format with id in the first column and a binary second column which is 
# 1 if the lower seeded team won and 0 otherwise
# the pred is the probability of the lower seeded team winning, right?
result = function(res_file) {
  d = read.csv(res_file)
  result = d$wteam < d$lteam
  tmp = data.frame(season  = d$season,
                   lowerid = pmin(d$wteam,d$lteam),
                   upperid = pmax(d$wteam,d$lteam))
  tmp$result = tmp$lowerid==d$wteam
  tmp
}

# A function to calculate the log-loss for results y and pred's yhat
logLoss = function(y,yhat) {
  -mean(y*log(yhat)+(1-y)*log(1-yhat))
}

# Take a prediction file and a result file and create the log-loss for each season
calculate_logLoss = function(pred_file, res_file) {
  result   = result(res_file)
  preds    = read.csv(pred_file)
  tmp      = colsplit(preds$id,"_",c("season","lowerid","upperid"))
  tmp$pred = preds$pred
  
  d = merge(subset(tmp, season != "S"), result, all=TRUE)
  ddply(d, .(season), function(x) {
    data.frame(logLoss = logLoss(x$pred, x$result))
  }, .progress="text")
}

calculate_logLoss("../data/submission/elo_chess.csv",
                  "../data/raw/tourney_results.csv")