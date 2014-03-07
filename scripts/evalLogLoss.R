# Nick Berry
# 2/25/14
# evalLogLoss.R

source("logLoss.R")

# Path now relative to scripts directory (where this file is located)
tourneyRes = read.csv("../data/raw/tourney_results.csv", stringsAsFactors = FALSE)

testSub1 = read.csv("../data/raw/submit_0127LS.csv", stringsAsFactors = FALSE)
testSub2 = read.csv("../data/raw/elo_chess_nohomecourt.csv", stringsAsFactors = FALSE)
testSub3 = read.csv("../data/raw/Ken Pom Win Probabilities.csv", stringsAsFactors = FALSE)

logLoss(testSub1, tourneyRes)
logLoss(testSub2, tourneyRes)
logLoss(testSub3, tourneyRes)

