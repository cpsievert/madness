require(plyr)
require(reshape2)

# Extract teams in the tournament
tourney_results = read.csv("../data/raw/tourney_results.csv")

tourney_teams = ddply(tourney_results, .(season), function(x) {
  teams = sort(unique(c(x$wteam,x$lteam)))
  nteams = length(teams)
  lowerid = upperid = list()
  for (i in 1:(nteams-1)) {
    ii = i+1
    lowerid[[i]] = rep(teams[i],nteams-i)
    upperid[[i]] = teams[ii:nteams]
    stopifnot(length(lowerid[[i]])==length(upperid[[i]]))
  }
  data.frame(lowerid=unlist(lowerid), upperid=unlist(upperid))
})

tourney_teams = tourney_teams[order(tourney_teams$season, tourney_teams$lowerid),]



# Add in season S
# A hack since season S tournament has not occurred yet
tmp = colsplit(read.csv("../data/raw/sample_submission.csv")$id, "_", c("season","lowerid","upperid"))
seasonS = data.frame(season=tmp$season, lowerid=tmp$lowerid, upperid=tmp$upperid)
rm(tmp)

tourney_teams = rbind(tourney_teams, seasonS)

sample_submission_all_seasons = data.frame(id=paste(tourney_teams$season,
                                                    tourney_teams$lowerid,
                                                    tourney_teams$upperid,
                                                    sep="_"),
                                           pred=0.5)




write.csv(sample_submission_all_seasons,
          "../data/sample_submission_all_seasons.csv", 
          row.names=FALSE)

