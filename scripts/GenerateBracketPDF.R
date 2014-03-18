rm(list=ls())
submission<-read.csv("./data/submission/predictor2014.csv")
teams<-read.csv("./data/raw/teams.csv")
seeds<-read.csv("./data/raw/tourney_seeds.csv")
pdf("../PRED_Bracket.pdf",width=11,height=8.5)
tgtseason<-"S"

seeds$region<-substr(seeds$seed,1,1)
seeds$seed<-as.numeric(substr(seeds$seed,2,3))
submission$season<-substr(submission[,1],1,1)
submission$team<-as.numeric(substr(submission[,1],3,5))
submission<-merge(submission,seeds)
names(submission)<-c("season", "team1", "id", "pred1", "seed1", "region1")
names(teams)<-c("team1", "name1")
submission<-merge(submission,teams)
submission$team2<-as.numeric(substr(submission[,3],7,9))
names(seeds)<-c("season","seed2","team2","region2")
submission<-merge(submission,seeds)
names(teams)<-c("team2", "name2")
submission<-merge(submission,teams)
submission<-subset(submission, season==tgtseason)
submission<-submission[,c(3,1,8,11,6,9,7,10,5)]

##############################################################
# You don't need the next line if your predictions are made. #
##############################################################

#submission$pred1<-round(sample(seq(0,1,.0001),nrow(submission)),4)
submission$pred2<-round((1-submission$pred1),4)

subw<-subset(submission, region1=="W" & region2=="W")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subw1<-merge(seed12,subw)
remove<-subw1[which(subw1$seed1==subw1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subw1<-subset(subw1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subw1$ord<-ifelse(subw1$seed1<subw1$seed2, subw1$seed1,subw1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw1<-merge(ord,subw1)
subw1<-subw1[with(subw1, order(ord1)), ]
subw1$str1<-ifelse(subw1$seed1<subw1$seed2, 
	paste(subw1$seed1,substr(subw1$name1,1,14),substr(subw1$pred1,2,4),sep="  "),
	paste(subw1$seed2,substr(subw1$name2,1,14),substr(subw1$pred2,2,4),sep="  "))
subw1$str2<-ifelse(subw1$seed1<subw1$seed2, 
	paste(subw1$seed2,substr(subw1$name2,1,14),substr(subw1$pred2,2,4),sep="  "),
	paste(subw1$seed1,substr(subw1$name1,1,14),substr(subw1$pred1,2,4),sep="  "))

r2teams<-ifelse(subw1$pred1>subw1$pred2, paste(subw1$name1), paste(subw1$name2))
subw2<-subset(subw,(name1 %in% r2teams) & (name2 %in% r2teams))
subw2<-subset(subw2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
	(seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
	(seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
	(seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subw2$ord<-ifelse(subw2$seed1<subw2$seed2, subw2$seed1,subw2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw2<-merge(ord,subw2)
subw2<-subw2[with(subw2, order(ord1)), ]
subw2$str1<-ifelse(subw2$seed1 %in% c(1,16,5,12,6,11,7,10), 
	paste(subw2$seed1,substr(subw2$name1,1,14),substr(subw2$pred1,2,4),sep="  "),
	paste(subw2$seed2,substr(subw2$name2,1,14),substr(subw2$pred2,2,4),sep="  "))
subw2$str2<-ifelse(subw2$seed1 %in% c(8,9,4,13,3,14,2,15), 
	paste(subw2$seed1,substr(subw2$name1,1,14),substr(subw2$pred1,2,4),sep="  "),
	paste(subw2$seed2,substr(subw2$name2,1,14),substr(subw2$pred2,2,4),sep="  "))


r3teams<-ifelse(subw2$pred1>subw2$pred2, paste(subw2$name1), paste(subw2$name2))
subw3<-subset(subw,(name1 %in% r3teams) & (name2 %in% r3teams))
subw3<-subset(subw3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
	(seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subw3$ord<-ifelse(subw3$seed1<subw3$seed2, subw3$seed1,subw3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subw3<-merge(ord,subw3)
subw3<-subw3[with(subw3, order(ord1)), ]
subw3$str1<-ifelse(subw3$seed1 %in% c(1,16,8,9,6,11,3,14), 
	paste(subw3$seed1,substr(subw3$name1,1,14),substr(subw3$pred1,2,4),sep="  "),
	paste(subw3$seed2,substr(subw3$name2,1,14),substr(subw3$pred2,2,4),sep="  "))
subw3$str2<-ifelse(subw3$seed1 %in% c(5,12,4,13,7,10,2,15), 
	paste(subw3$seed1,substr(subw3$name1,1,14),substr(subw3$pred1,2,4),sep="  "),
	paste(subw3$seed2,substr(subw3$name2,1,14),substr(subw3$pred2,2,4),sep="  "))

r4teams<-ifelse(subw3$pred1>subw3$pred2, paste(subw3$name1), paste(subw3$name2))
subw4<-subset(subw,(name1 %in% r4teams) & (name2 %in% r4teams))
subw4$str1<-ifelse(subw4$seed1 %in% c(1,16,8,9,5,12,4,13), 
	paste(subw4$seed1,substr(subw4$name1,1,14),substr(subw4$pred1,2,4),sep="  "),
	paste(subw4$seed2,substr(subw4$name2,1,14),substr(subw4$pred2,2,4),sep="  "))
subw4$str2<-ifelse(subw4$seed1 %in% c(6,11,3,14,7,10,2,15), 
	paste(subw4$seed1,substr(subw4$name1,1,14),substr(subw4$pred1,2,4),sep="  "),
	paste(subw4$seed2,substr(subw4$name2,1,14),substr(subw4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

subx<-subset(submission, region1=="X" & region2=="X")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subx1<-merge(seed12,subx)
remove<-subx1[which(subx1$seed1==subx1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subx1<-subset(subx1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subx1$ord<-ifelse(subx1$seed1<subx1$seed2, subx1$seed1,subx1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx1<-merge(ord,subx1)
subx1<-subx1[with(subx1, order(ord1)), ]
subx1$str1<-ifelse(subx1$seed1<subx1$seed2, 
	paste(subx1$seed1,substr(subx1$name1,1,14),substr(subx1$pred1,2,4),sep="  "),
	paste(subx1$seed2,substr(subx1$name2,1,14),substr(subx1$pred2,2,4),sep="  "))
subx1$str2<-ifelse(subx1$seed1<subx1$seed2, 
	paste(subx1$seed2,substr(subx1$name2,1,14),substr(subx1$pred2,2,4),sep="  "),
	paste(subx1$seed1,substr(subx1$name1,1,14),substr(subx1$pred1,2,4),sep="  "))

r2teams<-ifelse(subx1$pred1>subx1$pred2, paste(subx1$name1), paste(subx1$name2))
subx2<-subset(subx,(name1 %in% r2teams) & (name2 %in% r2teams))
subx2<-subset(subx2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
	(seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
	(seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
	(seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subx2$ord<-ifelse(subx2$seed1<subx2$seed2, subx2$seed1,subx2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx2<-merge(ord,subx2)
subx2<-subx2[with(subx2, order(ord1)), ]
subx2$str1<-ifelse(subx2$seed1 %in% c(1,16,5,12,6,11,7,10), 
	paste(subx2$seed1,substr(subx2$name1,1,14),substr(subx2$pred1,2,4),sep="  "),
	paste(subx2$seed2,substr(subx2$name2,1,14),substr(subx2$pred2,2,4),sep="  "))
subx2$str2<-ifelse(subx2$seed1 %in% c(8,9,4,13,3,14,2,15), 
	paste(subx2$seed1,substr(subx2$name1,1,14),substr(subx2$pred1,2,4),sep="  "),
	paste(subx2$seed2,substr(subx2$name2,1,14),substr(subx2$pred2,2,4),sep="  "))


r3teams<-ifelse(subx2$pred1>subx2$pred2, paste(subx2$name1), paste(subx2$name2))
subx3<-subset(subx,(name1 %in% r3teams) & (name2 %in% r3teams))
subx3<-subset(subx3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
	(seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subx3$ord<-ifelse(subx3$seed1<subx3$seed2, subx3$seed1,subx3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subx3<-merge(ord,subx3)
subx3<-subx3[with(subx3, order(ord1)), ]
subx3$str1<-ifelse(subx3$seed1 %in% c(1,16,8,9,6,11,3,14), 
	paste(subx3$seed1,substr(subx3$name1,1,14),substr(subx3$pred1,2,4),sep="  "),
	paste(subx3$seed2,substr(subx3$name2,1,14),substr(subx3$pred2,2,4),sep="  "))
subx3$str2<-ifelse(subx3$seed1 %in% c(5,12,4,13,7,10,2,15), 
	paste(subx3$seed1,substr(subx3$name1,1,14),substr(subx3$pred1,2,4),sep="  "),
	paste(subx3$seed2,substr(subx3$name2,1,14),substr(subx3$pred2,2,4),sep="  "))

r4teams<-ifelse(subx3$pred1>subx3$pred2, paste(subx3$name1), paste(subx3$name2))
subx4<-subset(subx,(name1 %in% r4teams) & (name2 %in% r4teams))
subx4$str1<-ifelse(subx4$seed1 %in% c(1,16,8,9,5,12,4,13), 
	paste(subx4$seed1,substr(subx4$name1,1,14),substr(subx4$pred1,2,4),sep="  "),
	paste(subx4$seed2,substr(subx4$name2,1,14),substr(subx4$pred2,2,4),sep="  "))
subx4$str2<-ifelse(subx4$seed1 %in% c(6,11,3,14,7,10,2,15), 
	paste(subx4$seed1,substr(subx4$name1,1,14),substr(subx4$pred1,2,4),sep="  "),
	paste(subx4$seed2,substr(subx4$name2,1,14),substr(subx4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

suby<-subset(submission, region1=="Y" & region2=="Y")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
suby1<-merge(seed12,suby)
remove<-suby1[which(suby1$seed1==suby1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
suby1<-subset(suby1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
suby1$ord<-ifelse(suby1$seed1<suby1$seed2, suby1$seed1,suby1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby1<-merge(ord,suby1)
suby1<-suby1[with(suby1, order(ord1)), ]
suby1$str1<-ifelse(suby1$seed1<suby1$seed2, 
	paste(suby1$seed1,substr(suby1$name1,1,14),substr(suby1$pred1,2,4),sep="  "),
	paste(suby1$seed2,substr(suby1$name2,1,14),substr(suby1$pred2,2,4),sep="  "))
suby1$str2<-ifelse(suby1$seed1<suby1$seed2, 
	paste(suby1$seed2,substr(suby1$name2,1,14),substr(suby1$pred2,2,4),sep="  "),
	paste(suby1$seed1,substr(suby1$name1,1,14),substr(suby1$pred1,2,4),sep="  "))

r2teams<-ifelse(suby1$pred1>suby1$pred2, paste(suby1$name1), paste(suby1$name2))
suby2<-subset(suby,(name1 %in% r2teams) & (name2 %in% r2teams))
suby2<-subset(suby2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
	(seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
	(seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
	(seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
suby2$ord<-ifelse(suby2$seed1<suby2$seed2, suby2$seed1,suby2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby2<-merge(ord,suby2)
suby2<-suby2[with(suby2, order(ord1)), ]
suby2$str1<-ifelse(suby2$seed1 %in% c(1,16,5,12,6,11,7,10), 
	paste(suby2$seed1,substr(suby2$name1,1,14),substr(suby2$pred1,2,4),sep="  "),
	paste(suby2$seed2,substr(suby2$name2,1,14),substr(suby2$pred2,2,4),sep="  "))
suby2$str2<-ifelse(suby2$seed1 %in% c(8,9,4,13,3,14,2,15), 
	paste(suby2$seed1,substr(suby2$name1,1,14),substr(suby2$pred1,2,4),sep="  "),
	paste(suby2$seed2,substr(suby2$name2,1,14),substr(suby2$pred2,2,4),sep="  "))


r3teams<-ifelse(suby2$pred1>suby2$pred2, paste(suby2$name1), paste(suby2$name2))
suby3<-subset(suby,(name1 %in% r3teams) & (name2 %in% r3teams))
suby3<-subset(suby3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
	(seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
suby3$ord<-ifelse(suby3$seed1<suby3$seed2, suby3$seed1,suby3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
suby3<-merge(ord,suby3)
suby3<-suby3[with(suby3, order(ord1)), ]
suby3$str1<-ifelse(suby3$seed1 %in% c(1,16,8,9,6,11,3,14), 
	paste(suby3$seed1,substr(suby3$name1,1,14),substr(suby3$pred1,2,4),sep="  "),
	paste(suby3$seed2,substr(suby3$name2,1,14),substr(suby3$pred2,2,4),sep="  "))
suby3$str2<-ifelse(suby3$seed1 %in% c(5,12,4,13,7,10,2,15), 
	paste(suby3$seed1,substr(suby3$name1,1,14),substr(suby3$pred1,2,4),sep="  "),
	paste(suby3$seed2,substr(suby3$name2,1,14),substr(suby3$pred2,2,4),sep="  "))

r4teams<-ifelse(suby3$pred1>suby3$pred2, paste(suby3$name1), paste(suby3$name2))
suby4<-subset(suby,(name1 %in% r4teams) & (name2 %in% r4teams))
suby4$str1<-ifelse(suby4$seed1 %in% c(1,16,8,9,5,12,4,13), 
	paste(suby4$seed1,substr(suby4$name1,1,14),substr(suby4$pred1,2,4),sep="  "),
	paste(suby4$seed2,substr(suby4$name2,1,14),substr(suby4$pred2,2,4),sep="  "))
suby4$str2<-ifelse(suby4$seed1 %in% c(6,11,3,14,7,10,2,15), 
	paste(suby4$seed1,substr(suby4$name1,1,14),substr(suby4$pred1,2,4),sep="  "),
	paste(suby4$seed2,substr(suby4$name2,1,14),substr(suby4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################

subz<-subset(submission, region1=="Z" & region2=="Z")

seed1<-c(1,8,5,4,6,3,7,2,16,9,12,13,11,14,10,15,1:16)
seed2<-c(16,9,12,13,11,14,10,15,1,8,5,4,6,3,7,2,1:16)
seed12<-as.data.frame(cbind(seed1,seed2))
subz1<-merge(seed12,subz)
remove<-subz1[which(subz1$seed1==subz1$seed2),]
remove<-ifelse(remove$pred1>remove$pred2, paste(remove$name2), paste(remove$name1))
subz1<-subset(subz1, !((name1 %in% c(remove)) | (name2 %in% c(remove))))
subz1$ord<-ifelse(subz1$seed1<subz1$seed2, subz1$seed1,subz1$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz1<-merge(ord,subz1)
subz1<-subz1[with(subz1, order(ord1)), ]
subz1$str1<-ifelse(subz1$seed1<subz1$seed2, 
	paste(subz1$seed1,substr(subz1$name1,1,14),substr(subz1$pred1,2,4),sep="  "),
	paste(subz1$seed2,substr(subz1$name2,1,14),substr(subz1$pred2,2,4),sep="  "))
subz1$str2<-ifelse(subz1$seed1<subz1$seed2, 
	paste(subz1$seed2,substr(subz1$name2,1,14),substr(subz1$pred2,2,4),sep="  "),
	paste(subz1$seed1,substr(subz1$name1,1,14),substr(subz1$pred1,2,4),sep="  "))

r2teams<-ifelse(subz1$pred1>subz1$pred2, paste(subz1$name1), paste(subz1$name2))
subz2<-subset(subz,(name1 %in% r2teams) & (name2 %in% r2teams))
subz2<-subset(subz2,(seed1 %in% c(1,16,8,9) & seed2 %in% c(1,16,8,9)) | 
	(seed1 %in% c(5,12,4,13) & seed2 %in% c(5,12,4,13)) |
	(seed1 %in% c(6,11,3,14) & seed2 %in% c(6,11,3,14)) |
	(seed1 %in% c(7,10,2,15) & seed2 %in% c(7,10,2,15)))
subz2$ord<-ifelse(subz2$seed1<subz2$seed2, subz2$seed1,subz2$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz2<-merge(ord,subz2)
subz2<-subz2[with(subz2, order(ord1)), ]
subz2$str1<-ifelse(subz2$seed1 %in% c(1,16,5,12,6,11,7,10), 
	paste(subz2$seed1,substr(subz2$name1,1,14),substr(subz2$pred1,2,4),sep="  "),
	paste(subz2$seed2,substr(subz2$name2,1,14),substr(subz2$pred2,2,4),sep="  "))
subz2$str2<-ifelse(subz2$seed1 %in% c(8,9,4,13,3,14,2,15), 
	paste(subz2$seed1,substr(subz2$name1,1,14),substr(subz2$pred1,2,4),sep="  "),
	paste(subz2$seed2,substr(subz2$name2,1,14),substr(subz2$pred2,2,4),sep="  "))


r3teams<-ifelse(subz2$pred1>subz2$pred2, paste(subz2$name1), paste(subz2$name2))
subz3<-subset(subz,(name1 %in% r3teams) & (name2 %in% r3teams))
subz3<-subset(subz3,(seed1 %in% c(1,16,8,9,5,12,4,13) & seed2 %in% c(1,16,8,9,5,12,4,13)) | 
	(seed1 %in% c(7,10,2,15,6,11,3,14) & seed2 %in% c(7,10,2,15,6,11,3,14)))
subz3$ord<-ifelse(subz3$seed1<subz3$seed2, subz3$seed1,subz3$seed2)
ord<-as.data.frame(c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15))
names(ord)<-c("ord")
ord$ord1<-c(1:16)
subz3<-merge(ord,subz3)
subz3<-subz3[with(subz3, order(ord1)), ]
subz3$str1<-ifelse(subz3$seed1 %in% c(1,16,8,9,6,11,3,14), 
	paste(subz3$seed1,substr(subz3$name1,1,14),substr(subz3$pred1,2,4),sep="  "),
	paste(subz3$seed2,substr(subz3$name2,1,14),substr(subz3$pred2,2,4),sep="  "))
subz3$str2<-ifelse(subz3$seed1 %in% c(5,12,4,13,7,10,2,15), 
	paste(subz3$seed1,substr(subz3$name1,1,14),substr(subz3$pred1,2,4),sep="  "),
	paste(subz3$seed2,substr(subz3$name2,1,14),substr(subz3$pred2,2,4),sep="  "))

r4teams<-ifelse(subz3$pred1>subz3$pred2, paste(subz3$name1), paste(subz3$name2))
subz4<-subset(subz,(name1 %in% r4teams) & (name2 %in% r4teams))
subz4$str1<-ifelse(subz4$seed1 %in% c(1,16,8,9,5,12,4,13), 
	paste(subz4$seed1,substr(subz4$name1,1,14),substr(subz4$pred1,2,4),sep="  "),
	paste(subz4$seed2,substr(subz4$name2,1,14),substr(subz4$pred2,2,4),sep="  "))
subz4$str2<-ifelse(subz4$seed1 %in% c(6,11,3,14,7,10,2,15), 
	paste(subz4$seed1,substr(subz4$name1,1,14),substr(subz4$pred1,2,4),sep="  "),
	paste(subz4$seed2,substr(subz4$name2,1,14),substr(subz4$pred2,2,4),sep="  "))

######################################################################################
######################################################################################
######################################################################################
ffdf<-rbind(subw4,subx4,suby4,subz4)
ffnames<-ifelse(ffdf$pred1>ffdf$pred2,paste(ffdf$name1),paste(ffdf$name2))

subff<-subset(submission,(name1 %in% ffnames) & (name2 %in% ffnames))
subff<-subset(subff, 
	 ((region1 %in% c("X","W")) & (region2 %in% c("X","W"))) |
	 ((region1 %in% c("Y","Z")) & (region2 %in% c("Y","Z")))
		  )
subff$ord<-ifelse(subff$region1 %in% c("X","W"),1,2)
subff<-subff[with(subff, order(ord)), ]

subff$str1<-ifelse(subff$region1 %in% c("W","Y"), 
	paste(subff$seed1,substr(subff$name1,1,14),substr(subff$pred1,2,4),sep="  "),
	paste(subff$seed2,substr(subff$name2,1,14),substr(subff$pred2,2,4),sep="  "))
subff$str2<-ifelse(subff$region1 %in% c("X","Z"), 
	paste(subff$seed1,substr(subff$name1,1,14),substr(subff$pred1,2,4),sep="  "),
	paste(subff$seed2,substr(subff$name2,1,14),substr(subff$pred2,2,4),sep="  "))
######################################################################################
######################################################################################
######################################################################################
finnames<-ifelse(subff$pred1>subff$pred2,paste(subff$name1),paste(subff$name2))

subfinal<-subset(submission,(name1 %in% finnames) & (name2 %in% finnames))


subfinal$str1<-ifelse(subfinal$region1 %in% c("W","X"), 
	paste(subfinal$seed1,substr(subfinal$name1,1,14),substr(subfinal$pred1,2,4),sep="  "),
	paste(subfinal$seed2,substr(subfinal$name2,1,14),substr(subfinal$pred2,2,4),sep="  "))
subfinal$str2<-ifelse(subfinal$region1 %in% c("Y","Z"), 
	paste(subfinal$seed1,substr(subfinal$name1,1,14),substr(subfinal$pred1,2,4),sep="  "),
	paste(subfinal$seed2,substr(subfinal$name2,1,14),substr(subfinal$pred2,2,4),sep="  "))

winner<-ifelse(subfinal$pred1>subfinal$pred2,paste(subfinal$name1),paste(subfinal$name2))




x<-seq(0,220,(221/67))
y<-0:66

plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
	axes=F, col="white")
segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2))) 
segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
segments(60,c(3,19,37,53),60,c(11,27,45,61))
segments(60,c(7,23,41,57),80,c(7,23,41,57))
segments(80,c(7,41),80,c(23,57))
segments(80,c(15,49),100,c(15,49))
segments(100,c(27,37),120,c(27,37))
segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2))) 
segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
segments(160,c(3,19,37,53),160,c(11,27,45,61))
segments(140,c(7,23,41,57),160,c(7,23,41,57))
segments(140,c(7,41),140,c(23,57))
segments(120,c(15,49),140,c(15,49))

text(9.8,64.5,subw1[1,13],cex=.4)
text(9.8,62.5,subw1[1,14],cex=.4)
text(9.8,60.5,subw1[2,13],cex=.4)
text(9.8,58.5,subw1[2,14],cex=.4)
text(9.8,56.5,subw1[3,13],cex=.4)
text(9.8,54.5,subw1[3,14],cex=.4)
text(9.8,52.5,subw1[4,13],cex=.4)
text(9.8,50.5,subw1[4,14],cex=.4)
text(9.8,48.5,subw1[5,13],cex=.4)
text(9.8,46.5,subw1[5,14],cex=.4)
text(9.8,44.5,subw1[6,13],cex=.4)
text(9.8,42.5,subw1[6,14],cex=.4)
text(9.8,40.5,subw1[7,13],cex=.4)
text(9.8,38.5,subw1[7,14],cex=.4)
text(9.8,36.5,subw1[8,13],cex=.4)
text(9.8,34.5,subw1[8,14],cex=.4)

text(29.8,63.5,subw2[1,13],cex=.4)
text(29.8,59.5,subw2[1,14],cex=.4)
text(29.8,55.5,subw2[2,13],cex=.4)
text(29.8,51.5,subw2[2,14],cex=.4)
text(29.8,47.5,subw2[3,13],cex=.4)
text(29.8,43.5,subw2[3,14],cex=.4)
text(29.8,39.5,subw2[4,13],cex=.4)
text(29.8,35.5,subw2[4,14],cex=.4)

text(49.8,61.5,subw3[1,13],cex=.4)
text(49.8,53.5,subw3[1,14],cex=.4)
text(49.8,45.5,subw3[2,13],cex=.4)
text(49.8,37.5,subw3[2,14],cex=.4)

text(69.8,57.5,subw4[1,11],cex=.4)
text(69.8,41.5,subw4[1,12],cex=.4)

text(9.8,30.5,suby1[1,13],cex=.4)
text(9.8,28.5,suby1[1,14],cex=.4)
text(9.8,26.5,suby1[2,13],cex=.4)
text(9.8,24.5,suby1[2,14],cex=.4)
text(9.8,22.5,suby1[3,13],cex=.4)
text(9.8,20.5,suby1[3,14],cex=.4)
text(9.8,18.5,suby1[4,13],cex=.4)
text(9.8,16.5,suby1[4,14],cex=.4)
text(9.8,14.5,suby1[5,13],cex=.4)
text(9.8,12.5,suby1[5,14],cex=.4)
text(9.8,10.5,suby1[6,13],cex=.4)
text(9.8,8.5,suby1[6,14],cex=.4)
text(9.8,6.5,suby1[7,13],cex=.4)
text(9.8,4.5,suby1[7,14],cex=.4)
text(9.8,2.5,suby1[8,13],cex=.4)
text(9.8,0.5,suby1[8,14],cex=.4)

text(29.8,29.5,suby2[1,13],cex=.4)
text(29.8,25.5,suby2[1,14],cex=.4)
text(29.8,21.5,suby2[2,13],cex=.4)
text(29.8,17.5,suby2[2,14],cex=.4)
text(29.8,13.5,suby2[3,13],cex=.4)
text(29.8,9.5,suby2[3,14],cex=.4)
text(29.8,5.5,suby2[4,13],cex=.4)
text(29.8,1.5,suby2[4,14],cex=.4)

text(49.8,27.5,suby3[1,13],cex=.4)
text(49.8,19.5,suby3[1,14],cex=.4)
text(49.8,11.5,suby3[2,13],cex=.4)
text(49.8,3.5,suby3[2,14],cex=.4)

text(69.8,23.5,suby4[1,11],cex=.4)
text(69.8,7.5,suby4[1,12],cex=.4)


text(209.8,64.5,subx1[1,13],cex=.4)
text(209.8,62.5,subx1[1,14],cex=.4)
text(209.8,60.5,subx1[2,13],cex=.4)
text(209.8,58.5,subx1[2,14],cex=.4)
text(209.8,56.5,subx1[3,13],cex=.4)
text(209.8,54.5,subx1[3,14],cex=.4)
text(209.8,52.5,subx1[4,13],cex=.4)
text(209.8,50.5,subx1[4,14],cex=.4)
text(209.8,48.5,subx1[5,13],cex=.4)
text(209.8,46.5,subx1[5,14],cex=.4)
text(209.8,44.5,subx1[6,13],cex=.4)
text(209.8,42.5,subx1[6,14],cex=.4)
text(209.8,40.5,subx1[7,13],cex=.4)
text(209.8,38.5,subx1[7,14],cex=.4)
text(209.8,36.5,subx1[8,13],cex=.4)
text(209.8,34.5,subx1[8,14],cex=.4)

text(189.8,63.5,subx2[1,13],cex=.4)
text(189.8,59.5,subx2[1,14],cex=.4)
text(189.8,55.5,subx2[2,13],cex=.4)
text(189.8,51.5,subx2[2,14],cex=.4)
text(189.8,47.5,subx2[3,13],cex=.4)
text(189.8,43.5,subx2[3,14],cex=.4)
text(189.8,39.5,subx2[4,13],cex=.4)
text(189.8,35.5,subx2[4,14],cex=.4)

text(169.8,61.5,subx3[1,13],cex=.4)
text(169.8,53.5,subx3[1,14],cex=.4)
text(169.8,45.5,subx3[2,13],cex=.4)
text(169.8,37.5,subx3[2,14],cex=.4)

text(149.8,57.5,subx4[1,11],cex=.4)
text(149.8,41.5,subx4[1,12],cex=.4)


text(209.8,30.5,subz1[1,13],cex=.4)
text(209.8,28.5,subz1[1,14],cex=.4)
text(209.8,26.5,subz1[2,13],cex=.4)
text(209.8,24.5,subz1[2,14],cex=.4)
text(209.8,22.5,subz1[3,13],cex=.4)
text(209.8,20.5,subz1[3,14],cex=.4)
text(209.8,18.5,subz1[4,13],cex=.4)
text(209.8,16.5,subz1[4,14],cex=.4)
text(209.8,14.5,subz1[5,13],cex=.4)
text(209.8,12.5,subz1[5,14],cex=.4)
text(209.8,10.5,subz1[6,13],cex=.4)
text(209.8,8.5,subz1[6,14],cex=.4)
text(209.8,6.5,subz1[7,13],cex=.4)
text(209.8,4.5,subz1[7,14],cex=.4)
text(209.8,2.5,subz1[8,13],cex=.4)
text(209.8,0.5,subz1[8,14],cex=.4)

text(189.8,29.5,subz2[1,13],cex=.4)
text(189.8,25.5,subz2[1,14],cex=.4)
text(189.8,21.5,subz2[2,13],cex=.4)
text(189.8,17.5,subz2[2,14],cex=.4)
text(189.8,13.5,subz2[3,13],cex=.4)
text(189.8,9.5,subz2[3,14],cex=.4)
text(189.8,5.5,subz2[4,13],cex=.4)
text(189.8,1.5,subz2[4,14],cex=.4)

text(169.8,27.5,subz3[1,13],cex=.4)
text(169.8,19.5,subz3[1,14],cex=.4)
text(169.8,11.5,subz3[2,13],cex=.4)
text(169.8,3.5,subz3[2,14],cex=.4)

text(149.8,23.5,subz4[1,11],cex=.4)
text(149.8,7.5,subz4[1,12],cex=.4)

text(89.8,49.5,subff[1,12],cex=.4)
text(129.8,49.5,subff[1,13],cex=.4)
text(89.8,15.5,subff[2,12],cex=.4)
text(129.8,15.5,subff[2,13],cex=.4)


text(109.8,37.5,subfinal[1,11],cex=.4)
text(109.8,27.5,subfinal[1,12],cex=.4)

text(109.8,32.5,winner,cex=2.5)


dev.off()