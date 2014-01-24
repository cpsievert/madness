#setwd("~/Desktop/github/local/madness/raw")
res <- read.csv("regular_season_results.csv", header=TRUE)
seas <- read.csv("seasons.csv", header=TRUE)
teams <- read.csv("teams.csv", header=TRUE)
resR <- subset(res, season == "R")

library(lubridate)
#figure out last date of regular season game for 2012-13 season
last.seas <- seas[seas$season == "R", ]
last.game <- resR[which.max(resR$daynum),]
as.Date(last.seas$dayzero, format="%m/%d/%Y") + days(last.game$daynum)
teams[teams$id %in% c(last.game$wteam, last.game$lteam),]
#Verify that Miami beat Florida on March 17th, 2013!
browseURL("http://en.wikipedia.org/wiki/2013_ACC_Men%27s_Basketball_Tournament")
