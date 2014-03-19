d=read.csv("../data/ModelsAndPreviousSeason.csv")
head(d)
names(d)

dsr=(d$lower.seed-d$upper.seed)/sqrt(as.numeric(d$round))
x=log(d$pred_m.pred/(1-d$pred_m.pred))
y=d$lo_win
o=glm(y~x+dsr,family=binomial(link=logit))
summary(o)

#To generate predictions for this year, use
#b=coef(o)
#LowWinProb=1/(1+exp(-b[1]-b[2]*xtest-b[3]*dsrtest))
#where xtest is the logit of Jarad's pred for this season's
#matchups and dsrtest is
#(seed of lower id - seed of upper id)/sqrt(round).

loss=function(y,yhat){
  -mean(y*log(yhat)+(1-y)*log(1-yhat))
}


years=unique(d$year)
years
nyears=length(years)
dyear=d$year

####Leave-one-year-out CV

predlwp=list()
length(predlwp)=nyears
lrlwp=predlwp
predloss=rep(0,nyears)
lrloss=rep(0,nyears)

for(i in 1:nyears){
  xtrain=x[dyear!=years[i]]
  dsrtrain=dsr[dyear!=years[i]]
  xtest=x[dyear==years[i]]
  dsrtest=dsr[dyear==years[i]]
  ytrain=y[dyear!=years[i]]
  ytest=y[dyear==years[i]]
  o=glm(ytrain~xtrain+dsrtrain,family=binomial(link=logit))
  b=coef(o)
  lrlwp[[i]]=1/(1+exp(-b[1]-b[2]*xtest-b[3]*dsrtest))
  predlwp[[i]]=d$pred_m.pred[dyear==years[i]]
  lrloss[i]=loss(y=ytest,yhat=lrlwp[[i]])
  predloss[i]=loss(y=ytest,yhat=predlwp[[i]])
}

boxplot(predloss,lrloss,
        axes=F,ylab="Loss",xlab="Method",col=4)
axis(2)
axis(1,labels=c("PRED","LR"),at=1:2)
box()

losses=cbind(predloss,lrloss)
plot(rep(1:2,each=nyears),losses,pch=" ",
        axes=F,ylab="Loss",xlab="Method")
axis(2)
axis(1,labels=c("PRED","LR"),at=1:2)

for(i in 1:nyears){
  lines(1:2,losses[i,],col=4)
  points(1:2,losses[i,],pch=paste(i-1))
}
box()

mean(lrloss<predloss)
mean(lrloss)

plot(d$pred_m.pred,unlist(lrlwp))