dataPath <- "C:/Users/Daniel Tallarico/Desktop/"
test_dat <- read.table(paste(dataPath,'Week 9 Assignment Alvin.csv',sep = '/'), header=TRUE)

test_dat$Resp# - response values;
test_dat$Pred1# - predictor 1 values;
test_dat$Pred2# - predictor 2 values;
test_dat$Pred3 #- predictor 3 values;
test_dat$Pred4 -# predictor 4 values;
test_dat$Pred5 #predictor 5 values;
test_dat$Pred6 #redictor 6 values;
test_dat$Pred7 #- predictor 7 values;
test_dat$Pred8 #- predictor 8 values;
test_dat$Pred9 #- predictor 9 values;
test_dat$Pred10# - predictor 10 values.

cor(test_dat)

Data.Levels<-as.numeric(test_dat[1,])
Project.Data<-test_dat[-1,]
head(Project.Data)

matplot(Project.Data,type="l")

rownames()
#Fit linear model with all predictors.
linMod<-lm(Resp~.,data=test_dat)
linMod
summary(linMod)

drop1(linMod)

linMod.no10<-lm(Resp ~ Pred1 + Pred2 + Pred3 + Pred4 + Pred5 + Pred6 + Pred7 + Pred8 + Pred9,data=test_dat)
drop1(linMod.no10)
summary(linMod.no10)

summary(lm(Resp ~ Pred1 + Pred6 + Pred7,data=test_dat))

summary(lm(Resp ~ Pred7 + Pred8+ Pred9,data=test_dat))


#not 1,2,3
#not 1,2,4
#1,2,5 and 1,2,9 and 1,3,4,1,3,7,1,3,8
#1,4,5 and 1,4,6 and 1,4,7 and 1,4,8 and 1,5,8 and 1,5,9
#maybe 1,2,6
#1,2,7 and 1,2,8, 1,3,5, 1,3,6..1,3,9 and 1,5,7

#1,4,9 really close as is 1,5,6 and 1,6,9
 #92.17
#PCA
library(leaps)
subsetsSwiss<-regsubsets(x=test_dat[2:6],y=test_dat$Resp)

testPredictors<-test_dat[,-1]

testPredictors.PCA<-princomp(testPredictors)
names(testPredictors.PCA)
plot(testPredictors.PCA)

testPredictors.PCA$sdev^2

(swissPCALoadings<-testPredictors.PCA$loadings)
testPredictors.PCA$center

swissPCAFactors<-testPredictors.PCA$scores
swissPCAFactors
swissRotated<-as.data.frame(cbind(Resp=test_dat$Resp,swissPCAFactors))
swissRotated
library(relaimpo)
pairs(as.matrix(swissRotated)
linModPCA<-lm(Resp~.,data=swissRotated)
summary(linModPCA)
      
metrics.swiss.pca <- calc.relimp(linModPCA, type = c("lmg", "first", "last","betasq", "pratt"))
metrics.swiss.pca

metrics.swiss.pca@lmg.rank
sum(metrics.swiss.pca@lmg)
      
orderComponents<-c(3,1,5,2,4)
swissRotatedReordered<-swissRotated[,c(1,orderComponents+1)]
(nestedRSquared<-sapply(2:6,function(z) summary(lm(Resp~.,data=swissRotatedReordered[,1:z]))$r.squared))

matplot(cbind(Regsubsets=summary(subsetsSwiss)$rsq,PCA=nestedRSquared),type="b",xlab="Number of Variables",ylab="R.Squared",lty=1,lwd=2,pch=16)
legend("bottomright",legend=c("Regsubsets","nestedRSquared"),lty=1,lwd=2,col=c("black","red"))


subsetsSwissRotated<-regsubsets(x=testPredictors.PCA$scores,y=test_dat$Resp)

summary(subsetsSwissRotated)$rsq  #answer to part 3


cumsum((testPredictors.PCA$sdev^2/sum(testPredictors.PCA$sdev^2))) #answer 2
