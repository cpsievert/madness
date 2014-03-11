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

source("seastoyear.R")

make_test_data <- function(games,...){
  input_list <- list(...)
  n=length(input_list)
  datnames <- as.character(substitute(c(...))[-1])
  
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
  
  splits <- strsplit(as.character(games$id),"_")
  
  for(i in 1:dim(games)[1]){
    st <- splits[[i]]
    games$season[i] <- st[1]
    games$hiteam[i] <- as.numeric(st[3])
    games$loteam[i]<- as.numeric(st[2])
  }
  
  games <- seastoyear(games)
  
  
  #combine team stats for loteam and hiteam into each row... get big rows!
  #this is the x matrix!
  combine_all <- function(res,stat){
    new <- data.frame(res, hi = stat[which(stat$id==res$hiteam & stat$year==res$year),
                                     -which(names(stat) %in% c("year", "season", "id"))], 
                      lo = stat[which(stat$id==res$loteam & stat$year==res$year),
                                -which(names(stat) %in% c("year", "season", "id"))])
    return(new)
  }
  
  pred <- adply(games[,3:5], 1, combine_all,stat=stats)
  return(pred)
}



