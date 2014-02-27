tRes = read.csv("/Users/Nick/Documents/MarchMadness/tourney_results.csv", stringsAsFactors = FALSE)
testSub1 = read.csv("/Users/Nick/Documents/MarchMadness/submit_0127LS.csv", stringsAsFactors = FALSE)
testSub2 = read.csv("/Users/Nick/Documents/MarchMadness/elo_chess_nohomecourt.csv", stringsAsFactors = FALSE)
testSub3 = read.csv("/Users/Nick/Documents/MarchMadness/Ken Pom Win Probabilities.csv", stringsAsFactors = FALSE)
logLoss(testSub1, tRes)
logLoss(testSub2, tRes)
logLoss(testSub3, tRes)

logLoss = function(preds, tourneyResults)
{
  split = strsplit(as.character(preds[,1]), "_")
  preds2 = data.frame(matrix(unlist(split), ncol = 3, byrow = TRUE))
  seasons = as.character(unique(preds2[,1]))
  preds2 = cbind(preds2, preds[,2])
  score = 0
  counter = 0
  
  for(i in 1:length(seasons))
  {
    thisSeason = subset(tourneyResults, season == seasons[i])
    thisPreds = subset(preds2, X1 == seasons[i])
    wSmaller = thisSeason$wteam < thisSeason$lteam
    print(as.numeric(wSmaller))
    for(j in 1:dim(thisSeason)[1])
    {
      thisSeason$prob[j] = thisPreds[which((thisPreds[,2] == thisSeason$wteam[j] & thisPreds[,3] == thisSeason$lteam[j]) | (thisPreds[,2] == thisSeason$lteam[j] & thisPreds[,3] == thisSeason$wteam[j])),4]
    }
    print(thisSeason$prob)
    logLosses = wSmaller*log(thisSeason$prob) + (1-wSmaller)*log(1-thisSeason$prob)
    score = score + sum(logLosses)
    counter = counter + length(thisSeason$prob)
  }
  score = -(1/counter) * score
  return(score)
}