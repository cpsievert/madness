setwd("/Users/lukefostvedt/Documents/Kaggle March Madness/")
data <- read.csv("sample_submission.csv")
tourny.results <- read.csv("tourney_results.csv")
tourny.seeds <- read.csv("tourney_seeds.csv")
tourny.slots <- read.csv("tourney_slots.csv")
teams <- read.csv("teams.csv")


# simulate the tournament for a given season
# "Sea" is the Season to be simulated: "ABCDEFGHIJKLMNOPQR"
# prob.mat is a matrix with probabilities where the upper 
# triangle represents the probability of the colname team winning
# The lower triangle represents the probability of the row.name
# winning.

# This function requires the following data sets
#tourny.seeds <- read.csv("tourney_seeds.csv")
#tourny.slots <- read.csv("tourney_slots.csv")
#teams <- read.csv("teams.csv")

# Play-in games are included so some seasons will need more than 64 teams
# Seasons A-E: 64 teams
# Seasons F-O: 65 teams
# Seasons P-R: 68 teams


tourn.simulator <- function(Sea = "R", prob.mat,tourny.seeds,tourny.slots,teams){	
Seeds <- tourny.seeds[which(tourny.seeds$season==Sea),]
Slots <- tourny.slots[which(tourny.slots$season==Sea),]	
	
x <- as.numeric(prob.mat)
# checking to see if the probabilities are legitimate
if(sum(x>1 | x<0,na.rm=T) >0){
	stop("Your have undefined/impossible probabilities in your matrix")
}
# checking to see if the probabilities are legitimate
if(length(grep(Sea,"ABCDEFGHIJKLMNOPQR"))!=1){
	stop("You have not selected a valid season\nPlease select one of the following seasons: \nA B C D E F G H I J K L M N O P Q R \n")
}
Ranks <- tourny.seeds[which(tourny.seeds$season==Sea),]
if(sum(is.na(match(colnames(prob.mat),Ranks$team)))>0){
	stop("You have not included probabilities for all teams. \n Make sure you have the correct teams for tournament:",Sea," \n")
}

#Simulating who wins each of all possible matchups using rbinom()
# with the probabilities submitted in the probability matrix

a <- prob.mat
b <- as.vector(lower.tri(a))
w <- rep(1,length(b))
w[which(lower.tri(a) == TRUE)] <- rbinom(sum(lower.tri(a)),1,a[which(lower.tri(a) == TRUE)])
w[which(upper.tri(a) == TRUE)] <- 1-w[which(lower.tri(a) == TRUE)] 
win.mat <- matrix(w,ncol=dim(a)[1],nrow=dim(a)[2])
colnames(win.mat) <- colnames(prob.mat)
row.names(win.mat) <- row.names(prob.mat)
# win.mat is a matrix with 1's on the diagonal, and then a 0 or 1
# on the upper triangle and then a 1 or 0 on the lower triangle
# with the result consistent with the upper triangle result. 
# This means the lower triangle probability must 
# be the complement of the upper triangle

# Determining the tournament outcome
# play-in outcomes
Slots$strongteam <- Seeds$team[match(Slots$strongseed,Seeds$seed)]
Slots$weakteam <- Seeds$team[match(Slots$weakseed,Seeds$seed)]
Slots$win <- NA

for (i in 1:(length(Slots$slot)-1)){
	game <- Slots$slot[i]
	winner <- win.mat[which(colnames(win.mat)==Slots$weakteam[i]),which(row.names(win.mat)==Slots$strongteam[i])]
	if(winner==1) Slots$win[i] <- Slots$strongteam[i]
	if(winner==0) Slots$win[i] <- Slots$weakteam[i]
	ind1 <- which(as.character(Slots$weakseed) == as.character(Slots$slot)[i])
	ind2 <- which(as.character(Slots$strongseed) == as.character(Slots$slot)[i])
	if(length(ind2) >0)	{
		Slots$strongteam[ind2] <- Slots$win[i]
		}
	if(length(ind1)>0) {
		Slots$weakteam[ind1] <- Slots$win[i]	
	}
}
Home <- as.character(teams$name[which(teams$id ==Slots$strongteam[i+1])])
Away <- as.character(teams$name[which(teams$id ==Slots$weakteam[i+1])])
winner <- win.mat[which(colnames(win.mat)==Slots$weakteam[i+1]),which(row.names(win.mat)==Slots$strongteam[i+1])]
	if(winner==1) Slots$win[i+1] <- Slots$strongteam[i+1]
	if(winner==0) Slots$win[i+1] <- Slots$weakteam[i+1]
Champ <- as.character(teams$name[which(teams$id ==Slots$win[i+1])])
cat("In the Championship matchup\n",Home,"vs.",Away,"\n \nThe National Champion is", Champ," \n ")
return(Slots)
}



# Minimum Working Example with all probabilities at 0.5
Sea="P"
Seeds <- tourny.seeds[which(tourny.seeds$season==Sea),]
Slots <- tourny.slots[which(tourny.slots$season==Sea),]	
prob.mat <- matrix(.5,68,68)
#Probmat <- D
diag(prob.mat) <- 1
row.names(prob.mat) <- Seeds$team[which(Seeds$season==Sea)]
colnames(prob.mat) <- row.names(prob.mat)
head(prob.mat)
result <- tourn.simulator(Sea,prob.mat,tourny.seeds,tourny.slots,teams)
result
