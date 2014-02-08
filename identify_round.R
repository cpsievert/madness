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
res$final_four <- as.integer(res$wregion != res$lregion)
res[grep("[A-Z][0-9]+b", res$wseed),] #why are there a and b seeds? Hopefully that doesn't matter...

#get a matchup identifier (with higher seed put first)
res$wrank <- as.integer(str_extract_all(res$wseed, "[0-9]+"))
res$lrank <- as.integer(str_extract_all(res$lseed, "[0-9]+"))
lowseed <- with(res, ifelse(wrank < lrank, wrank, lrank))
highseed <- with(res, ifelse(wrank > lrank, wrank, lrank))
res$matchup <- paste(lowseed, highseed, sep="-")

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
res$round <- NA
#final four can have any matchup!
res$round[as.logical(res$final_four)] <- "final_four"
res$round[res$matchup %in% rd1] <- "1"
res$round[res$matchup %in% rd2] <- "2"
res$round[res$matchup %in% rd3] <- "3"
res$round[res$matchup %in% rd4] <- "4"
res$round[is.na(res$round)] <- "preliminary_round?"

#don't really need to save these columns
res <- res[,-grep("final_four|wrank|lrank|wregion|lregion", names(res))]

write.csv(res, file="tourney_results_with_round.csv", row.names=FALSE)

