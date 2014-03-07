regular_season = read.csv("../../data/raw/regular_season_results.csv")
regular_season$wteam = factor(regular_season$wteam)
regular_season$lteam = factor(regular_season$lteam)
stopifnot(all.equal(levels(regular_season$wteam), 
                    levels(regular_season$lteam)))
