load(paste(model,"_mcmc.RData",sep=""))

source("read_regular_season_results.R")

# Team summary statistics
summary = ldply(mcmc, function(x) {
  as.data.frame(summary(x)$summary[1:nlevels(regular_season$wteam),1:8])
})
summary$id = levels(regular_season$wteam)

# Calculate strength of schedule
opponents = read.csv("../../data/opponents.csv")
opponents$my_teams     = match(opponents$teams,    levels(regular_season$wteam))
opponents$my_opponents = match(opponents$opponent, levels(regular_season$wteam))

team_strength = 
ddply(opponents, .(season,teams), function(x) {
  theta = summary$mean[summary$season==unique(x$season)]
  data.frame(theta = theta[x$my_opponents])
})

tmp = ddply(team_strength, .(season,teams), summarise, strength_of_schedule=mean(theta))
names(tmp)[2] = "id"

write.csv(merge(summary,tmp, all=TRUE),
          file=paste("../../data/team_statistics/",model,".csv",sep=""), 
          row.names=F)
