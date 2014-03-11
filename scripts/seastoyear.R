
# a function which converts season letters to years, and drops off all 
# years prior to 2003 
seastoyear <- function(x){
  a <- x
  a$year <- NA
  a$year[which(x$season=="H")] <- 2003
  a$year[which(x$season=="I")] <- 2004
  a$year[which(x$season=="J")] <- 2005
  a$year[which(x$season=="K")] <- 2006
  a$year[which(x$season=="L")] <- 2007
  a$year[which(x$season=="M")] <- 2008
  a$year[which(x$season=="N")] <- 2009
  a$year[which(x$season=="O")] <- 2010
  a$year[which(x$season=="P")] <- 2011
  a$year[which(x$season=="Q")] <- 2012
  a$year[which(x$season=="R")] <- 2013
  a$year[which(x$season=="S")] <- 2014
  a$season <- a$year
  a <- a[,-which(names(a)=="year")]
  names(a)[which(names(a)=="season")] <- "year"
  return(a)
}
