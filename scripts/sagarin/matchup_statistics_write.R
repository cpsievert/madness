write.csv(probs, 
          paste("../../data/matchup_statistics/",model,".csv",sep=""), 
          row.names=FALSE)

# write in submission format
write.csv(data.frame(id = paste(probs$season,"_",probs$lowerid,"_",probs$upperid,sep=""),
                     pred = probs$pred),
          paste("../../data/submission/",model,".csv",sep=""),
          row.names=FALSE)
