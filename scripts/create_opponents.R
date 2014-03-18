# Creates a data frame containing three columns:
#   season   : season id
#   teams    : team id
#   opponent : opponent id
# The primary use is for calculating strength of schedule

regular_season = read.csv("../data/raw/regular_season_results.csv")

tmp = expand.grid(teams  = sort(unique(c(regular_season$wteam, regular_season$lteam))),
                  season = sort(unique(as.character(regular_season$season))))

games = ddply(tmp, .(season,teams), function(x) {
  this_season = regular_season[regular_season$season==x$season,]
  data.frame(opponents = c(this_season$lteam[this_season$wteam == x$teams],
                           this_season$wteam[this_season$lteam == x$teams]))
})

write.csv(games, "../data/opponents.csv", row.names=FALSE)
