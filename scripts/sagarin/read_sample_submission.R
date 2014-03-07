# Submission data
submission = read.csv("../../data/raw/sample_submission.csv")
submission$season = factor(substr(submission$id,1,1))
submission$team1  = factor(substr(submission$id,3,5), levels(d$wteam))
submission$team2  = factor(substr(submission$id,7,9), levels(d$wteam))
