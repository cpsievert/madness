
# This function takes in a dataframe in the same style as the kaggle site. Two columns.
# The first is the game identifier in the form `N_501_502` with the first letter being the 
# season, the first number being the smaller team id, and the second number being the larger
# team id. The second column is the probability of the first team beating the second team.

# The function allows you to enter any subset of the last 18 seasons that you want as long
# as you enter entire seasons (no half seasons). Make sure that the tourneyResults dataframe
# is read directly from the tourney_results.csv file provided by kaggle. Above are some of the
# test cases I was using to test it. Its not super efficient, but it runs in a second or two.

# The seasons parameter allows you to specify a vector of seasons (in letter format) that you
# want the logLoss score for. So seasons = c("A","B","C","F","M") would only run include the 
# scores for those particular years.

logLoss = function(preds, tourneyResults, seasons = LETTERS[1:18])
{
  seasons = toupper(seasons)
  tourneyResults = subset(tourneyResults, daynum >= 136)
  split = strsplit(as.character(preds[,1]), "_")
  preds2 = data.frame(matrix(unlist(split), ncol = 3, byrow = TRUE))
  seasons = seasons[which(seasons %in% as.character(unique(preds2[,1])))]
  cat("Evaluating for seasons: ", seasons, "\n\n")
  preds2 = cbind(preds2, preds[,2])
  score = 0
  counter = 0
  if(length(which(preds2[,4] == 0)) > 0)
    preds2[which(preds2[,4] == 0),4] = .0000001
  if(length(which(preds2[,4] == 1)) > 0)
    preds2[which(preds2[,4] == 1),4] = .9999999
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
