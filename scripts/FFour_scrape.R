### Code to scrape season data from basketball reference website and
### attach team id number.
### Teamstats.csv is a list of team statsitics and statistics for each
### team's opponents
### Ranks.csv is a list of each team's rank and the rank of their opponents

library(XML)
library(lubridate)
library(RCurl)
library(gdata)

setwd("U:\\Final Four\\")

conv <- read.csv("conversion.csv")$id[1:356]
name_url <- "http://www.sports-reference.com/cbb/schools/"
doc<- htmlParse(name_url)
col_info <- getNodeSet(doc, "//a")

names <- read.csv("teams.csv")
teamnames=data.frame(names=names$name,con=conv)

for(i in 1:length(teamnames$con)){
id <-teamnames$con[i]
if(!is.na(id)){

#if(length(grep("gid", xmlAttrs(gameinfo[[i]])))!=0) {

col_id <- unlist(xmlAttrs(col_info[[id+34]]))[[1]]
col_url <- paste("http://www.sports-reference.com",col_id, sep="")

while(class(try(doc1 <- htmlParse(col_url), silent=TRUE))=="try-error"){
			Sys.sleep(5)
		}

yr_info <- getNodeSet(doc1, "//a")
pt=36
year<-as.numeric(strsplit(xmlValue(yr_info[[pt]]),"-")[[1]][1])
while((year>1998)&&(!is.na(year))){

yurl<- paste(col_url, as.character(year+1),".html", sep="")
while(class(try(dat <- readHTMLTable(yurl), silent=TRUE))=="try-error"){
			Sys.sleep(5)
		}

if((id==1)&&(pt==36)){
#plr <- data.frame(id=(500+i), year=(year+1),roster=dat$roster)
#tstats <- data.frame(id=(500+i), year=(year+1),team_stats=dat$team_stats )
pgstats <- data.frame(id=(500+i), year=(year+1),pgstats=dat$per_game)
advanced <- data.frame(id=(500+i), year=(year+1),advanced=dat$advanced)
}
else{
#plr <- rbind(plr,data.frame(id=(500+i), year=(year+1),roster=dat$roster))
#tstats <- rbind(tstats,data.frame(id=(500+i), year=as.character(year+1),team_stats=dat$team_stats))
pgstats <- rbind(pgstats, data.frame(id=(500+i), year=as.character(year+1),pgstats=dat$per_game))
advanced <- rbind(advanced, data.frame(id=(500+i), year=as.character(year+1),advanced=dat$advanced))
}

pt=pt+1
year<-as.numeric(strsplit(xmlValue(yr_info[[pt]]),"-")[[1]][1])
}
}
}




write.csv(plr, "U:\\Final Four\\playerf.csv")
write.csv(tstats, "U:\\Final Four\\team_statsf.csv")
write.csv(pgstats, "U:\\Final Four\\per_gamef.csv")
write.csv(advanced, "U:\\Final Four\\advancedf.csv")

ranks <- subset(tstats, team_stats. == 'Rank')

ranks <- ranks[, -c(1,4,5,6)]
names(ranks) <- sub("team_stats", "", names(ranks))
names(ranks) <- sub("\\.", "", names(ranks))
names(ranks) <- sub("PTS.G", "PTSperG", names(ranks))
names(ranks) <- sub("\\.", "_percent", names(ranks))
for(i in seq_along(ranks)){
ranks[,i] <- as.numeric(sub("[a-z]+$","",ranks[,i]))
}
ranks$teamVSopp <- rep(c("Team", "Opponent"), times=dim(ranks)[1]/2)

teamstats <- subset(tstats, team_stats. %in% c('Team','Opponent'))

teamstats <- teamstats[, -1]
names(teamstats) <- sub("team_stats", "", names(teamstats))
names(teamstats) <- sub("\\.", "", names(teamstats))
names(teamstats) <- sub("PTS.G", "PTSperG", names(teamstats))
names(teamstats) <- sub("\\.", "_percent", names(teamstats))
teamstats <- teamstats[, -3]
teamstats$teamVSopp <- rep(c("Team", "Opponent"), times=dim(teamstats)[1]/2)

write.csv(ranks, "U:\\Final Four\\ranks.csv")
write.csv(teamstats, "U:\\Final Four\\teamstats.csv")

