library(plyr)
library(reshape)

# Read in the data
basket_season_data <- read.csv("regular_season_results.csv")
team_names <- read.csv("teams.csv")

# Create margin of victory/loss columns
basket_season_data$margin_victory <- basket_season_data$wscore - basket_season_data$lscore
basket_season_data$margin_loss <- basket_season_data$lscore - basket_season_data$wscore


# Match team names to the id numbers 
df <- data.frame(lapply(basket_season_data, function(x) team_names$name[match(x,team_names$id)]))
basket_season_data$wteam <- df$wteam
basket_season_data$lteam <- df$lteam


# Create some basic results
victory_results <- ddply(basket_season_data, .(wteam, season), summarize,
                          wscore = sum(wscore),                                         # total sum of winning scores for a given team in a given season
                          num_wins = length(wteam),                                     # total number of wins for a given team in a given season
                          avg_margin_victory = sum(margin_victory)/length(wteam)        # avg margin of victory for a given team in a given season
                          )
loss_results <- ddply(basket_season_data, .(lteam, season), summarize,
                          lscore = sum(lscore),                                         # total sum of losing scores for a given team in a given season
                          num_loss = length(lteam),                                     # total number of losses for a given team in a given season
                          avg_margin_loss = sum(margin_loss)/length(lteam)              # avg margin of defeat for a given team in a given season
                          )

# Merges both of the victory and defeat results by team and season
results <- merge(victory_results, loss_results, by.x=c("wteam", "season"), by.y=c("lteam","season"))

# The average points scored by a team in a given season
results$avg_score <- (results$wscore + results$lscore)/(results$num_wins + results$num_loss)



# Ceates a subset of games where the margin of scores was less then 6pts. We will call these games 'close'.
sub_data <- subset(basket_season_data, margin_victory <= 6)

# Counts the number of wins in the subset
victory_sub_results <- ddply(sub_data, .(wteam, season), summarize,
                         num_wins = length(wteam)
)

# Counts the number of losses in the subset 
loss_sub_results <- ddply(sub_data, .(lteam, season), summarize,
                      num_loss = length(lteam)
)

# Merges the two results into one
results_sub <- merge(victory_sub_results, loss_sub_results, by.x=c("wteam", "season"), by.y=c("lteam","season"))

# Creates the percentages of games a given team wins when the game is close( pt margin < 6)
results_sub$close_win <- results_sub$num_wins/(results_sub$num_wins + results_sub$num_loss)

# Merges all the results together!
full_results <- merge(results, results_sub[,c(1,2,5)], by.x=c("wteam", "season"), by.y=c("wteam","season"))




write.csv(file="SeasonResults.csv", x=full_results)