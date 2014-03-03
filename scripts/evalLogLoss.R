# Nick Berry
# 2/25/14
# evalLogLoss.R

tourneyRes = read.csv("/Users/Nick/Documents/MarchMadness/tourney_results.csv", stringsAsFactors = FALSE)
testSub1 = read.csv("/Users/Nick/Documents/MarchMadness/submit_0127LS.csv", stringsAsFactors = FALSE)
testSub2 = read.csv("/Users/Nick/Documents/MarchMadness/elo_chess_nohomecourt.csv", stringsAsFactors = FALSE)
testSub3 = read.csv("/Users/Nick/Documents/MarchMadness/Ken Pom Win Probabilities.csv", stringsAsFactors = FALSE)
logLoss(testSub1, tourneyRes)
logLoss(testSub2, tourneyRes)
logLoss(testSub3, tourneyRes)


# This function takes in a dataframe in the same style as the kaggle site. Two columns.
# The first is the game identifier in the form `N_501_502` with the first letter being the 
# season, the first number being the smaller team id, and the second number being the larger
# team id. The second column is the probability of the first team beating the second team.

# The function allows you to enter any subset of the last 18 seasons that you want as long
# as you enter entire seasons (no half seasons). Make sure that the tourneyResults dataframe
# is read directly from the tourney_results.csv file provided by kaggle. Above are some of the
# test cases I was using to test it. Its not super efficient, but it runs in a second or two.

logLoss = function(preds, tourneyResults)
{
  tourneyResults = subset(tourneyResults, daynum >= 136)
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
    for(j in 1:dim(thisSeason)[1])
    {
      thisSeason$prob[j] = thisPreds[which((thisPreds[,2] == thisSeason$wteam[j] & thisPreds[,3] == thisSeason$lteam[j]) | (thisPreds[,2] == thisSeason$lteam[j] & thisPreds[,3] == thisSeason$wteam[j])),4]
    }
    logLosses = wSmaller*log(thisSeason$prob) + (1-wSmaller)*log(1-thisSeason$prob)
    score = score + sum(logLosses)
    counter = counter + length(thisSeason$prob)
  }
  score = -(1/counter) * score
  return(score)
}