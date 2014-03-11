model = "predictor"

source("matchup_statistics.R")

probs = 
  ldply(mcmc, function(x) {
    season = attr(x,"name")
    theta  = extract(x, "theta")[[1]]
    sigma  = extract(x, "sigma")[[1]]
    
    sub = submission[submission$season==season,]
    n   = nrow(sub)
    
    prob = numeric(n)
    for (i in 1:n) {
      prob[i] = mean(pnorm(theta[,sub$my_lowerid[i]]-theta[,sub$my_upperid[i]],0,
                           sigma))
    }
    
    data.frame(lowerid = sub$my_lowerid[i],
               upperid = sub$my_upperid[i],
               pred = prob)
  })


source("matchup_statistics_write.R")
