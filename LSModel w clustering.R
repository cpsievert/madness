# Nick Berry, Sam Helmich
# Create LS model for NCAA tourney basketball teams for every season since 1996
# Run computed probabilities through kaggle algorithm to get 'score'

games = read.csv("data/regular_season_results.csv")
teams = read.csv("data/teams revised.csv")
clusters = read.csv("data/team clusters.csv")

games$year = as.numeric(games$season) + 1995

games$wName = character(dim(games)[1])
games$lName = character(dim(games)[1])
for(i in 1:length(teams$name))
{
  games$wName[which(games$wteam == i+500)] = as.character(teams$name[i])
  games$lName[which(games$lteam == i+500)] = as.character(teams$name[i])
}

modelSeason = function(year = -1, season = "Z")
{
  seas = season
  # Make sure year is year that tournament played not year season started.
  if(year == -1 & year == "Z")
    stop("Enter a year number or season letter from the file.")
  if(year != -1)
    seas = toupper(letters[year-1995])
  else
    seas = toupper(season)
  subDat = subset(games, games$season == seas)
  
  if(seas %in% c("P", "Q", "R")){
    subClusts <- subset(clusters, season == seas)
    subDat$spread = numeric(dim(subDat)[1])
    subDat$spread[which(subDat$wloc == 'H' | subDat$wloc == 'N')] = subDat$wscore[which(subDat$wloc == 'H' | subDat$wloc == 'N')]-subDat$lscore[which(subDat$wloc == 'H' | subDat$wloc == 'N')]
    subDat$spread[which(subDat$wloc == 'A')] = -1*(subDat$wscore[which(subDat$wloc == 'A')]-subDat$lscore[which(subDat$wloc == 'A')])
    
    yearTeams = unique(sort(c(subDat$wteam, subDat$lteam)))
    
    modelMat = matrix(0, ncol = length(yearTeams) + 1 + 6, nrow = dim(subDat)[1])
    modelMat[,1] = 1
    modelMat[which(subDat$wloc == 'N'),1] = 0
    
    clustercols <- c((length(yearTeams) + 2):(length(yearTeams)+7))
    
    for(i in 1:dim(subDat)[1])
    {
      if(subDat$wloc[i] == 'H' | subDat$wloc[i] == 'N')
      {
        modelMat[i,which(yearTeams == subDat$wteam[i]) + 1] = 1
        wt.clust <- subClusts[subClusts$id == subDat$wteam[i], "cluster"]  
        modelMat[i,clustercols[wt.clust]] = 1
        modelMat[i,which(yearTeams == subDat$lteam[i]) + 1] = -1
        lt.clust <- subClusts[subClusts$id == subDat$lteam[i], "cluster"]
        modelMat[i,clustercols[lt.clust]] = -1
        if(wt.clust == lt.clust) modelMat[i,clustercols[wt.clust]] = 0
      }
      else if(subDat$wloc[i] == 'A')
      {
        modelMat[i,which(yearTeams == subDat$wteam[i]) + 1] = -1
        wt.clust <- subClusts[subClusts$id == subDat$wteam[i], "cluster"]  
        modelMat[i,clustercols[wt.clust]] = -1
        modelMat[i,which(yearTeams == subDat$lteam[i]) + 1] = 1
        lt.clust <- subClusts[subClusts$id == subDat$lteam[i], "cluster"]
        modelMat[i,clustercols[lt.clust]] = 1
        if(wt.clust == lt.clust) modelMat[i,clustercols[wt.clust]] = 0
      }
    }
    
    lmod = lm(subDat$spread ~ modelMat + 0)
    retList = list(lmod$coef[-1], lmod$coef[1], summary(lmod)$sigma)
    retList[[1]][length(yearTeams)] = 0
    retList[[1]][is.na(retList[[1]])] = 0
    cluster.coefs <- tail(as.vector(retList[[1]]),6)
    for(i in 1:length(yearTeams)){
      retList[[1]][i] = retList[[1]][i] + cluster.coefs[subClusts[subClusts$id == yearTeams[i],"cluster"]]
    }
    retList[[1]] = head(retList[[1]], length(yearTeams))
    retList[[1]] = retList[[1]] - mean(retList[[1]], na.rm=TRUE)
    names(retList) = c("coefs", "hfAdv", "resSE")
    names(retList$coefs) = yearTeams[as.numeric(substr(names(retList$coefs), 9,11))-1]
    return(retList)
  }
  
  subDat$spread = numeric(dim(subDat)[1])
  subDat$spread[which(subDat$wloc == 'H' | subDat$wloc == 'N')] = subDat$wscore[which(subDat$wloc == 'H' | subDat$wloc == 'N')]-subDat$lscore[which(subDat$wloc == 'H' | subDat$wloc == 'N')]
  subDat$spread[which(subDat$wloc == 'A')] = -1*(subDat$wscore[which(subDat$wloc == 'A')]-subDat$lscore[which(subDat$wloc == 'A')])

  yearTeams = unique(sort(c(subDat$wteam, subDat$lteam)))
  
  modelMat = matrix(0, ncol = length(yearTeams) + 1, nrow = dim(subDat)[1])
  modelMat[,1] = 1
  modelMat[which(subDat$wloc == 'N'),1] = 0
  
  for(i in 1:dim(subDat)[1])
  {
    if(subDat$wloc[i] == 'H' | subDat$wloc[i] == 'N')
    {
      modelMat[i,which(yearTeams == subDat$wteam[i]) + 1] = 1
      modelMat[i,which(yearTeams == subDat$lteam[i]) + 1] = -1
    }
    else if(subDat$wloc[i] == 'A')
    {
      modelMat[i,which(yearTeams == subDat$wteam[i]) + 1] = -1
      modelMat[i,which(yearTeams == subDat$lteam[i]) + 1] = 1
    }
  }
  
  lmod = lm(subDat$spread ~ modelMat + 0)
  retList = list(lmod$coef[-1], lmod$coef[1], summary(lmod)$sigma)
  retList[[1]][length(yearTeams)] = 0
  retList[[1]] = retList[[1]] - mean(retList[[1]], na.rm=TRUE)
  names(retList) = c("coefs", "hfAdv", "resSE")
  names(retList$coefs) = yearTeams[as.numeric(substr(names(retList$coefs), 9,11))-1]
  return(retList)
}

model2013 = modelSeason(2013)

models = list(rep(list(NA), 18))
for(i in 1996:2013)
{
  models[[i-1995]] = modelSeason(i)
}

### Use model on past 5 years tournament games

t4 = read.csv("data/sample_submission.csv")
t5 = strsplit(as.character(t4$id), split='_')
out=t(as.data.frame(t5,dim=c(length(t5),3), row.names=NULL))
row.names(out)=NULL
out2 = matrix(NA, nrow(out), ncol(out))
out2[out[,1] == "N",1] = 14
out2[out[,1] == "O",1] = 15
out2[out[,1] == "P",1] = 16
out2[out[,1] == "Q",1] = 17
out2[out[,1] == "R",1] = 18
out2[,2] = as.integer(out[,2])-500
out2[,3] = as.integer(out[,3])-500

out2 = cbind(out2, rep(0, dim(t4)[1]))
rNames = character(dim(t4)[1])
for(i in 1:dim(t4)[1])
{
  out2[i,4] = pnorm((models[[out2[i,1]]]$coefs[which(names(models[[out2[i,1]]]$coefs) == as.character(500 + out2[i,2]))] - models[[out2[i,1]]]$coefs[which(names(models[[out2[i,1]]]$coefs) == as.character(500 + out2[i,3]))])/models[[out2[i,1]]]$resSE)
  rNames[i] = paste(names(models[[out2[i,1]]]$coefs[which(names(models[[out2[i,1]]]$coefs) == as.character(500 + out2[i,2]))]), names(models[[out2[i,1]]]$coefs[which(names(models[[out2[i,1]]]$coefs) == as.character(500 + out2[i,3]))]), sep="_") 
}
row.names(out2) = rNames

t6 = paste(toupper(letters[out2[,1]]), row.names(out2), sep = "_")
t6 = cbind(paste(toupper(letters[out2[,1]]), row.names(out2), sep = "_"), out2[,4])
colnames(t6) = c("id", "pred")

write.csv(t6, file="data/sam_submission_1.csv", row.names=F)


#Viewable matrix with team names in row names
out3 = out2
for(i in 1:dim(t4)[1])
{
  rNames[i] = paste(teams$name[which(teams$id == 500 + out3[i,2])], teams$name[which(teams$id == 500 + out3[i,3])], sep="_") 
}
row.names(out3) = rNames

out3_Duke = out3[grepl("Duke", row.names(out3)),]

#install.packages("lme4")
