write.csv(probs, 
          paste("../../data/matchup_statistics/",model,".csv",sep=""), 
          row.names=FALSE)

# write in submission format
write.csv(data.frame(id = paste(probs$season,"_",probs$lowerid,"_",probs$upperid,sep=""),
                     pred = probs$pred),
          paste("../../data/submission/",model,".csv",sep=""),
          row.names=FALSE)

# write competition submission file
tmp = read.csv(paste("../../data/submission/",model,".csv",sep=""))
write.csv(tmp[grep("S",tmp$id),],
          paste("../../data/submission/",model,"2014.csv",sep=""),
          row.names=FALSE)