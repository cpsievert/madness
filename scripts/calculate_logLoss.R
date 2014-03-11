library(plyr)


result = function(res_file) {
  d = read.csv(res_file)
  result = d$wteam < d$lteam
  id = paste(d$season, "_",
             ifelse(result, d$wteam, d$lteam), "_",
             ifelse(result, d$lteam, d$wteam), sep="")
  data.frame(id=id,result=result)
}

logLoss = function(y,yhat) {
  -mean(y*log(yhat)+(1-y)*log(1-yhat))
}

calculate_logLoss = function(pred_file, res_file) {
  result = result(res_file)
  preds = read.csv(pred_file)
  d = merge(preds, result, all=TRUE)
  ddply(d, .(season), function(x) {
    logLoss(x$preds, x$result)
  })
}

calculate_logLoss("../data/matchup_statistics/elo_chess.csv",
                  "../data/raw/tourney_results.csv")