#  makeX is a function which creates an x matrix.  
#  The first argument should be a data frame containing the tourney_results.csv file (for training)
#  
#  Additional arugments are any other data frames which have a  "year" or "season" column and an "id" column.
#  
#  The output will be a data frame where each row represents a postseason game.
#  The first column indicates whether the lower id team won. 
#  Other columns contain the team id's, year, and all the additional data given in the 
#  "..." data files.  Any number of data files can be included.
#  Note: only works for games from 2002-2003 onward
require(plyr)


makeX <- function(results,...){
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
a <- a[-which(is.na(a$year)),]
a <- a[,-which(names(a)=="season")]
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


results <- seastoyear(results)
results$lowwin <- 1
results[which(results$wteam>results$lteam),]$lowwin <- 0

#remove unnecessary columns
 results <- results[,-1]


#determine hi team and low team by team id
results$upperid <- NA
results$lowerid <- NA

hit <- function(x){
new <- x
new$upperid <- max(x$wteam,x$lteam)
new$lowerid <- min(x$wteam,x$lteam)
return(new)}

#new data frame with hi team and low team
nresults<- adply(results, 1, hit)

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
	x <- adply(nresults[,c(7,6,8,9)], 1, combine_all, stat=stats)
	if(nm>0){
		for(i in 1:nm){
			x<- join(x,input_list[[ms[i]]])
		}
	}
}

else{
	if(nm>0){
		x <- join(nresults[,c(7,6,8,9)],input_list[[ms[1]]])
	}
	if(nm>1){
		for(i in 2:nm){
			x<- join(x,input_list[[ms[i]]])
		}
	}
}

x[,1] <- as.factor(x[,1])
for(i in 1:dim(x)[2]){
	if(!(is.factor(x[1,i]))){
		x[,i] <- as.numeric(x[,i])
	}
}
return(x)
}



