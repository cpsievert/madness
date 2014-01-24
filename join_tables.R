#Before running script, make sure your working directory is 'madness'
#setwd("~/Desktop/github/local/madness")

library(dplyr)
db_file <- list.files()[grepl(".sqlite3", list.files())]
my_db <- src_sqlite(db_file)
res <- tbl(my_db, "regular_season_results")
teams <- tbl(my_db, "teams")
colnames(teams) <- c("wteam", "winning_team")
res2 <- inner_join(res, teams)


