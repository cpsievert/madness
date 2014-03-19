d = data.frame(
  elo = read.csv("../data/submission/elo_chess2014.csv")$pred,
  pred = read.csv("../data/submission/predictor2014.csv")$pred,
  ls = read.csv("../data/submission/LSModel2014.csv")$pred,
  pom = read.csv("../data/submission/2014 Pomeroy Win Prob.csv")$pred
  )

pairs(d)