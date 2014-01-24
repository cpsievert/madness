#Before running script, make sure your working directory is 'madness'
#setwd("~/Desktop/github/local/madness")

library(dplyr)
#does a SQLite database already exist? If not, create one.
db_file <- list.files()[grepl(".sqlite3", list.files())]
if (length(db_file) > 0){
  my_db <- src_sqlite(db_file, create = FALSE)
} else { 
  my_db <- src_sqlite("madness.sqlite3", create = TRUE)
}

# copy raw files to database
csvs <- list.files("raw")
for (i in csvs) {
  name <- sub(".csv", "", i)
  copy_to(my_db, df=read.csv(paste0("raw/", i), header=TRUE), name)
}
