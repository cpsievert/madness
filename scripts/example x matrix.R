# Assumes you are in the scripts directory (where this file is supposed to be located)
source("makeX.R")
source("make_test_data.R")


# Files with team statistics
# Want something like this
# ls = system("ls ../data/team_statistics/*.csv", TRUE)
pred  <- read.csv("../data/team_statistics/predictor.csv")
elo   <- read.csv("../data/team_statistics/elo_chess.csv")
ls    <- read.csv("../data/team_statistics/TeamStats_LSModelEstimates.csv")
tstat <- read.csv("../data/team_statistics/regular_season_data.csv")

xmat <- makeX(read.csv("../data/raw/tourney_results.csv"), 
              tstat, pred, elo, ls)

#make sure all columns except lowwin are numeric
xmat[,1] <- as.factor(xmat[,1])
xmat[,-1] <- data.matrix(xmat[,-1])


preds <- make_test_data(read.csv("../data/raw/sample_submission.csv"),
                        pred)
#again make sure columns are all numeric
preds <- as.data.frame(data.matrix(preds))


# boosting example
# library(mboost)
# library(party)
# tree <- bas.lackboost(lowwin ~ ., data=xmat, family=Binomial(link="logit"))
# samp[,2]<- predict(tree, preds, type="response")
