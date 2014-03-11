#  make_test_data is a function which creates a matrix 
#  where each row represents a game in the "sample_submission.csv"
#  file.  This matrix can be used for prediction by a machine learning method.
#  First use the  makeX.r file to make an x-matrix with which to train the 
#  learner.  Then, use make_test_data to make a matrix for predictions.  
# 
#  The first argument should be the sample_submission.csv file 
#  Additional arugments are any other data files which have a  "year" or "season" column and an "id" column.
#  Use the same data files that were used to train the learner.
# 
#  The output will be a data frame where each row represents 
#  a potential postseason game, with statistics for each team in the 
#  matchup.
#  Note:  It takes a little while for this to run.  Sorry!
require(plyr)

make_test_data <- function(games,...){
require(plyr)
input_list <- list(...)
n=length(input_list)
datnames <- as.character(substitute(c(...))[-1])


torm <- c(rep(1, n))
for(i in 1:n){
if("lowerid" %in% names(input_list[[i]])){
torm[i] <- (-1)
}
}



# a function which converts season letters to years, and drops off all 
# years prior to 2003 
seastoyear <- function(x){
a <- x
a$year <- NA
a$year[which(x$season=="H")]<- 2003
a$year[which(x$season=="I")]<- 2004
a$year[which(x$season=="J")] <- 2005
a$year[which(x$season=="K")]<- 2006
a$year[which(x$season=="L")]<- 2007
a$year[which(x$season=="M")] <- 2008
a$year[which(x$season=="N")] <- 2009
a$year[which(x$season=="O")]<- 2010
a$year[which(x$season=="P")]<- 2011
a$year[which(x$season=="Q")] <- 2012
a$year[which(x$season=="R")] <- 2013
a$year[which(x$season=="S")] <- 2014
a$season <- a$year
a <- a[,-which(names(a)=="year")]
names(a)[which(names(a)=="season")] <- "year"
return(a)
}


for(i in 1:n){
	if(!(is.null(input_list[[i]]$season))){
		input_list[[i]] <- seastoyear(input_list[[i]])
	}
	bnames <- paste(datnames[i], names(input_list[[i]]),sep=".")
	names(input_list[[i]])[which(!(names(input_list[[i]])%in%
		c("id","year", "lowerid", "upperid")))] <- bnames[which(!(names(input_list[[i]])%in%
		c("id","year", "lowerid", "upperid")))]
}


nt <- length(which(torm==1))
nm <- length(which(torm==-1))
ts<- which(torm==1)
ms<-which(torm==-1)



if(nt>0){
stats <- input_list[[ts[1]]]
}
if(nt>1){
	for(i in 2:nt){
		stats <- join(stats,input_list[[ts[i]]])
	}
}

splits <- strsplit(as.character(games$id),"_")

for(i in 1:dim(games)[1]){
st <- splits[[i]]
games$season[i] <- st[1]
games$upperid[i] <- as.numeric(st[3])
games$lowerid[i]<- as.numeric(st[2])
}

games <- seastoyear(games)


#combine team stats for loteam and hiteam into each row... get big rows!
#this is the x matrix!
combine_all <- function(res,stat){
	new <- data.frame(res, upper = stat[which(stat$id==res$upperid & stat$year==res$year),
		-which(names(stat) %in% c("year", "season", "id"))], 
	lower = stat[which(stat$id==res$lowerid & stat$year==res$year),
		-which(names(stat) %in% c("year", "season", "id"))])
	return(new)
}


if(exists("stats")){
	pred <- adply(games[,3:5], 1, combine_all,stat=stats)
	if(nm>0){
		for(i in 1:nm){
			pred<- join(pred,input_list[[ms[i]]])
		}
	}
}


else{
	if(nm>0){
		pred <- join(games[,3:5],input_list[[ms[1]]])
	}
	if(nm>1){
		for(i in 2:nm){
			pred<- join(pred,input_list[[ms[i]]])
		}
	}
}

for(i in 1:dim(pred)[2]){
	if(!(is.factor(pred[1,i]))){
		pred[,i] <- as.numeric(pred[,i])
	}
}

return(pred)
}



