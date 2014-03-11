#  makeX is a function which creates an x matrix.  
#  The first argument should be the tourney_results.csv file (for training)
#  
#  Additional arugments are any other data files which have a  "year" or "season" column and an "id" column.
#  
#  The output will be a data frame where each row represents a postseason game.
#  The first column indicates whether the lower id team won. 
#  Other columns contain the team id's, year, and all the additional data given in the 
#  "..." data files.  Any number of data files can be included.
#  Note: only works for games from 2002-2003 onward
require(plyr)


makeX <- function(results,...){
input_list <- list(...)
n=length(input_list)
datnames <- as.character(substitute(c(...))[-1])


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
		c("id","year")))] <- bnames[which(!(names(input_list[[i]])%in%
		c("id","year")))]
}


stats <- input_list[[1]]
if(n>1){
	stats <- join(stats,input_list[[2]])
	if(n>2){
		for(i in 3:n){
			stats <- join(stats,input_list[[i]])
		}
	}
}

results <- seastoyear(results)
results$lowwin <- 1
results[which(results$wteam>results$lteam),]$lowwin <- 0

#remove unnecessary columns
 results <- results[,-1]


#determine hi team and low team by team id
results$hiteam <- NA
results$loteam <- NA

hit <- function(x){
new <- x
new$hiteam <- max(x$wteam,x$lteam)
new$loteam <- min(x$wteam,x$lteam)
return(new)}

#new data frame with hi team and low team
nresults<- adply(results, 1, hit)

#combine team stats for loteam and hiteam into each row... get big rows!
#this is the x matrix!
combine_all <- function(res,stat){
	new <- data.frame(res, hi = stat[which(stat$id==res$hiteam & stat$year==res$year),
		-which(names(stat) %in% c("year", "season", "id"))], 
	lo = stat[which(stat$id==res$loteam & stat$year==res$year),
		-which(names(stat) %in% c("year", "season", "id"))])
	return(new)
}
x <- adply(nresults[,c(7,6,8,9)], 1, combine_all, stat=stats)
return(x)
}


