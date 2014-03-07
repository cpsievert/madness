# Nick Berry
# 2/25/14
# evalLogLoss.R

source("logLoss.R")

tourneyRes = read.csv("/Users/Nick/Documents/MarchMadness/tourney_results.csv", stringsAsFactors = FALSE)
testSub1 = read.csv("/Users/Nick/Documents/MarchMadness/submit_0127LS.csv", stringsAsFactors = FALSE)
testSub2 = read.csv("/Users/Nick/Documents/MarchMadness/elo_chess_nohomecourt.csv", stringsAsFactors = FALSE)
testSub3 = read.csv("/Users/Nick/Documents/MarchMadness/Ken Pom Win Probabilities.csv", stringsAsFactors = FALSE)
logLoss(testSub1, tourneyRes)
logLoss(testSub2, tourneyRes)
logLoss(testSub3, tourneyRes)

