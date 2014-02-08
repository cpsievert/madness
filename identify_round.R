library(plyr)
library(stringr)
seeds <- read.csv("raw/tourney_seeds.csv")
res <- read.csv("raw/tourney_results.csv")

#appends column to res to identify seed of winning team (wseed)
names(seeds) <- gsub("^team$", "wteam", names(seeds))
names(seeds) <- gsub("^seed$", "wseed", names(seeds))
res <- plyr::join(res, seeds, by=c("wteam", "season"))

#appends column to res to identify seed of losing team (lseed)
names(seeds) <- gsub("^wteam$", "lteam", names(seeds))
names(seeds) <- gsub("^wseed$", "lseed", names(seeds))
res <- plyr::join(res, seeds, by=c("lteam", "season"))

#add a region identifier
res$wregion <- substr(res$wseed, 0, 1)
res$lregion <- substr(res$lseed, 0, 1)

#start building a matchup identifier (which is used to determine round)
res$matchup <- NA
res$wrank <- as.integer(str_extract_all(res$wseed, "[0-9]+"))
res$lrank <- as.integer(str_extract_all(res$lseed, "[0-9]+"))
lowseed <- with(res, ifelse(wrank < lrank, wrank, lrank))
highseed <- with(res, ifelse(wrank > lrank, wrank, lrank))
res$matchup <- paste(lowseed, highseed, sep="-")

#final four can have any matchup (so matchup really shouldn't count)!
res$matchup[res$wregion != res$lregion] <- "final_four"

#it seems as though teams that made it through the 'preliminary' round keep an 'a' or 'b' at the end of the seed
#we can identify a 'preliminary game' if both teams have 'a' or 'b'
bothseed <- with(res, paste(wseed, lseed, sep="-"))
res[grep("[A-Z][0-9]+[a-z]-[A-Z][0-9]+[a-z]", bothseed), "matchup"] <- "preliminary"
#now we can get rid of the 'a'/'b' and treat everyone the same
res$wseed <- sub("[a-z]$", "", res$wseed)
res$lseed <- sub("[a-z]$", "", res$lseed)


#function that takes on a vector of possible seeds and produces matchups (with higher seed put first)
getMatchups <- function(seeds) {
  seeds <- sort(seeds, decreasing=TRUE)
  matchups <- NULL
  for (i in seq_along(seeds)){
    seedz <- seeds[seeds[i] < seeds]
    if (length(seedz)) matchup <- paste(seeds[i], seedz, sep="-") else matchup <- NULL
    matchups <- c(matchups, matchup)
  }
  return(matchups)
}

#matchups for first round are easy
rd1 <- paste(seq(1, 8), 17-seq(1, 8), sep="-")
#matchups for second round
a <- c(1, 16, 8, 9)
rd2a <- getMatchups(a)
rd2a <- rd2a[!rd2a %in% rd1] #throw away games that were played previously
b <- c(5, 12, 4, 13)
rd2b <- getMatchups(b)
rd2b <- rd2b[!rd2b %in% rd1] 
c <- c(6, 11, 3, 14)
rd2c <- getMatchups(c)
rd2c <- rd2c[!rd2c %in% rd1] 
d <- c(7, 10, 2, 15)
rd2d <- getMatchups(d)
rd2d <- rd2d[!rd2d %in% rd1] 
rd2 <- c(rd2a, rd2b, rd2c, rd2d)

#matchups for third round
rd3a <- getMatchups(c(a, b))
rd3a <- rd3a[!rd3a %in% c(rd1, rd2)]
rd3b <- getMatchups(c(c, d))
rd3b <- rd3b[!rd3b %in% c(rd1, rd2)]
rd3 <- c(rd3a, rd3b)

#matchups for fourth round
rd4 <- getMatchups(c(a, b, c, d))
rd4 <- rd4[!rd4 %in% c(rd1, rd2, rd3)]

#finally, create the round identifier
#have to split across seasons or weird stuff happens
res$round <- res$matchup
res$round[ddply(res, "season", summarise, rd1=matchup %in% rd1)[,2]] <- "1"
res$round[ddply(res, "season", summarise, rd2=matchup %in% rd2)[,2]] <- "2"
res$round[ddply(res, "season", summarise, rd3=matchup %in% rd3)[,2]] <- "3"
res$round[ddply(res, "season", summarise, rd4=matchup %in% rd4)[,2]] <- "4"

#sanity checks
table(res$round)/length(unique(res$season))
#um does somebody know if this makes sense?
table(subset(res, round %in% "preliminary")$season)

#don't have to save these columns
res <- res[,-grep("final_four|wrank|lrank|wregion|lregion", names(res))]

write.csv(res, file="tourney_results_with_round.csv", row.names=FALSE)

