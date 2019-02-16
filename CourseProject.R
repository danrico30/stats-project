# bNnASGRf
library(readr)
AssignmentData<-read.csv(file=paste("C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/RegressionAssignmentData2014.csv",sep="/"),
         row.names=1,header=TRUE,sep=",")

View(AssignmentData)
head(AssignmentData)
matplot(AssignmentData[,-c(8,9,10)],type='l',ylab="Interest Rates",
        main="History of Interest Rates",xlab="Index")
matplot(AssignmentData[,-c(9,10)],type='l',ylab="Interest Rates",
        main="History of Interest Rates and Output",xlab="Index")

#2 Step 1. Simple Regressions

# mUSGG3M is object of the model with formula USGG3M~Output1 
mUSGG3M<- lm(USGG3M~Output1,data = AssignmentData)
c(Total.Variance=var(AssignmentData[,8]),Unexplained.Variance=summary(mUSGG3M)$sigma^2)
coef(mUSGG3M)
matplot(AssignmentData[,1],type="l",col="blue",xaxt="n")
lines(mUSGG3M$fitted.values,col="orange")
newPredictor<-data.frame(Output1=AssignmentData$Output1[1])
predict(mUSGG3M,newdata=newPredictor)
newPredictor <- read.table(paste('StatisticalAnalysis_Course_Assignment_1_Data.csv',sep = '/'), header=TRUE)
newPredictor

mUSGG6M<-lm(USGG6M~Output1,data = AssignmentData)
coef(mUSGG6M)
#Part 1
USGG2YR<-lm(USGG2YR~Output1,data = AssignmentData)
USGG3YR<-lm(USGG3YR~Output1,data = AssignmentData)
USGG5YR<-lm(USGG5YR~Output1,data = AssignmentData)
USGG10YR<-lm(USGG10YR~Output1,data = AssignmentData)
USGG30YR<-lm(USGG30YR~Output1,data = AssignmentData)
colnames(AssignmentData)
?predict()
newPredictor
prUSGG3M<-predict(mUSGG3M,newdata=newPredictor)
prUSGG6M<-predict(mUSGG6M,newdata=newPredictor)
prUSGG2YR<-predict(USGG2YR,newdata=newPredictor)
prUSGG3YR<-predict(USGG3YR,newdata=newPredictor)
prUSGG5YR<-predict(USGG5YR,newdata=newPredictor)
prUSGG10YR<-predict(USGG10YR,newdata=newPredictor)
prUSGG30YR<-predict(USGG30YR,newdata=newPredictor)
predicted.values <- c(`prUSGG3M`, `prUSGG6M`, `prUSGG2YR`, `prUSGG3YR`, `prUSGG5YR`, `prUSGG10YR`, `prUSGG30YR`)


predicted.values
res <- list(predicted.values =  predicted.values)
write.table(res, file = paste('C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/result.csv',sep = '/'), row.names = F)


#Part 2
AssignmentDataLogistic<-AssignmentData[,c(1:7,10)]
All.NAs<-is.na(AssignmentData[,9])&is.na(AssignmentData[,10]) # neither tightening nor easing
noTightening<-is.na(AssignmentData[,10]) 
AssignmentDataLogistic[noTightening,'Tightening']<-0 # replace NAs with 0
cat("Before: ",dim(AssignmentDataLogistic),"\n") # before

AssignmentDataLogistic<-AssignmentDataLogistic[!All.NAs,]
cat("After: ",dim(AssignmentDataLogistic),"\n") # after removing neutral periods
AssignmentDataLogistic[c(275:284),]


data <- readRDS(paste('C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/StatisticalAnalysis_Course_Assignment_2_Data.rds',sep = '/'))
data$US_Treasury_rate
LogisticModelAll<-glm(Tightening~.,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModelAll
summary(LogisticModelAll)


LogisticModel_3M<-glm(Tightening~USGG3M,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_6M<-glm(Tightening~USGG6M,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_2YR<-glm(Tightening~USGG2YR,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_3YR<-glm(Tightening~USGG3YR,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_5YR<-glm(Tightening~USGG5YR,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_10YR<-glm(Tightening~USGG10YR,family=binomial(link=logit),AssignmentDataLogistic)
LogisticModel_30YR<-glm(Tightening~USGG30YR,family=binomial(link=logit),AssignmentDataLogistic)



newPredictor<-data$Predictor
newPredictor

(newPredictors<-setNames(newPredictor,colnames(AssignmentDataLogistic[1:7])))
(newPredictors<-as.data.frame(t(newPredictors)))

odds<-predict(LogisticModelAll,newdata=newPredictors,type = 'response')
summary(odds)
     # predict log-odds
Probabilities<--1/(exp(-3.3253517)+1)  
Probabilities
exp(3.3253517*.01)*100

unlist(LogisticModelAll$coefficients)

summary(LogisticModel_All)$aic
summary(LogisticModel_All)$coefficients[,c(1,4)]
matplot(AssignmentDataLogistic[,-8],type="l",ylab="Results of Logistic Regression",ylim=c(-5,20))
lines(AssignmentDataLogistic[,8]*20,col="blue",lwd=3)
lines(LogisticModel_All$fitted.values*20,col="orange",lwd=3)
legend("topright",legend=c("prediction","response"),lty=1,
       col=c("orange","blue"),lwd=3)
plot(Log.Odds,type="l",ylab="Response & Log-Odds",lwd=3,col="blue",
     main="Log-odds and Response")
lines(AssignmentDataLogistic[,8],col="orange",lwd=3)
legend("topright",legend=c("Log-Odds","Response"),
       lty=1,lwd=3,col=c("blue","orange"))


(newPredictors<-unname(unlist(AssignmentDataLogistic[1,1:7])))
(newPredictors<-setNames(newPredictors,colnames(AssignmentDataLogistic[1:7])))
(newPredictors<-as.data.frame(t(newPredictors)))
predict(LogisticModel_All,newdata=newPredictors,type='response')


#Part 3
AssignmentData1<-AssignmentData[,-c(9,10)]
Null<-lm(Output1~1,data = AssignmentData)
Full<-lm(Output1~.,data= AssignmentData1)


compare<-lm(Output1~(USGG3M),data=AssignmentData)

compare6M<-lm(Output1~(USGG6M+ USGG3M),data=AssignmentData)
compare2YR<-lm(Output1~(USGG2YR+ USGG3M),data=AssignmentData)
compare3YR<-lm(Output1~(USGG3YR + USGG3M),data=AssignmentData)
compare5YR<-lm(Output1~(USGG5YR + USGG3M),data=AssignmentData)
compare10YR<-lm(Output1~(USGG10YR + USGG3M),data=AssignmentData)
compare30YR<-lm(Output1~(USGG30YR + USGG3M),data=AssignmentData)


compare1ANOVA<-anova(compare,compare6M)
compare2ANOVA<-anova(compare,compare2YR)
compare3ANOVA<-anova(compare,compare3YR)
compare4ANOVA<-anova(compare,compare5YR)
compare5ANOVA<-anova(compare,compare10YR)
compare6ANOVA<-anova(compare,compare30YR)

V1<-compare1ANOVA$`Sum of Sq`
V2<-compare2ANOVA$`Sum of Sq`
V3<-compare3ANOVA$`Sum of Sq`
V4<-compare4ANOVA$`Sum of Sq`
V5<-compare5ANOVA$`Sum of Sq`
V6<-compare6ANOVA$`Sum of Sq`

V<-c(V1,V2,V3,V4,V5,V6)

Selected.US.Treasury<-'USGG6M'
AIC6M<-AIC(compare6M)
AIC2YR<-AIC(compare2YR)
AIC3YR<-AIC(compare3YR)
AIC5YR<-AIC(compare5YR)
AIC10YR<-AIC(compare10YR)
AIC30YR<-AIC(compare30YR)

AIC<-c(AIC6M,AIC2YR,AIC3YR,AIC5YR,AIC10YR,AIC30YR)
AIC
res <- list(SumSq= SumSq,AIC = AIC,Selected.US.Treasury = Selected.US.Treasury)
saveRDS(res, file = paste('C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/result.rds',sep = '/'))
res

testrate<- (AssignmentData$USGG3M) 
modeltest<-lm(Output1~testrate, data=AssignmentData)
modeladditionalrate1 <- lm(Output1~testrate+USGG6M, data = AssignmentData)
modeladditionalrate2 <- lm(Output1~testrate+USGG2YR, data = AssignmentData)
modeladditionalrate3 <- lm(Output1~testrate+USGG3YR, data = AssignmentData)
modeladditionalrate4 <- lm(Output1~testrate+USGG5YR, data = AssignmentData)
modeladditionalrate5 <- lm(Output1~testrate+USGG10YR, data = AssignmentData)
modeladditionalrate6 <- lm(Output1~testrate+USGG30YR, data = AssignmentData)
v1<-anova(modeltest, modeladditionalrate1)$"Sum of Sq"[2]
v2<-anova(modeltest, modeladditionalrate2)$"Sum of Sq"[2]
v3<-anova(modeltest, modeladditionalrate3)$"Sum of Sq"[2]
v4<-anova(modeltest, modeladditionalrate4)$"Sum of Sq"[2]
v5<-anova(modeltest, modeladditionalrate5)$"Sum of Sq"[2]
v6<-anova(modeltest, modeladditionalrate6)$"Sum of Sq"[2]
SumSq <- c(v1, v2, v3, v4, v5, v6)
SumSq
Selected.US.Treasury<-"USGG5YR"
AIC1<-AIC(modeladditionalrate1)
AIC2<-AIC(modeladditionalrate2)
AIC3<-AIC(modeladditionalrate3)
AIC4<-AIC(modeladditionalrate4)
AIC5<-AIC(modeladditionalrate5)
AIC6<-AIC(modeladditionalrate6)
AIC<-c(AIC1, AIC2, AIC3, AIC4, AIC5, AIC6)
res <- list(SumSq= SumSq,AIC = AIC,Selected.US.Treasury = Selected.US.Treasury)
saveRDS(res, file = paste('C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/result.rds',sep = '/'))
res
# Step 4
Window.width<-20; Window.shift<-5
library(zoo)




x<-rollapply(AssignmentDataRegressionComparison,Rolling.window.matrix,sd)
head(x)


Rolling.window.matrix<-rollapply(Count,width=Window.width,by=Window.shift,by.column=FALSE,FUN=function(z) z)



testDate='6/17/2004'



matplot(Coefficients[,-1],xaxt="n",type="l",col=c("black","cyan","magenta"),
        lty=1,ylab="Rolling Slopes",
        main="Slopes of Rolling Models")
axis(side=1,at=1:1657,rownames(Coefficients))
legend("bottomleft",legend=c("USGG3M","USGG5YR","USGG30YR"),
       col=c("black","cyan","magenta"),lty=1)


high.slopespread.periods<-rownames(Coefficients)[Coefficients[,3]-Coefficients[,4]>3]
high.slope5Y<-rownames(Coefficients)[Coefficients[,3]>2.5]
high.slopespread.periods


r.squared<-rollapply(AssignmentDataRegressionComparison,
                     width=Window.width,by=Window.shift,by.column=FALSE,
                     FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,
                                                data=as.data.frame(z)))$r.squared)
r.squared<-data.frame(Date=rolling.dates[,10],R2=r.squared)
head(r.squared)



Coefficients<-rollapply(AssignmentDataRegressionComparison,width=Window.width,
                          by=Window.shift,by.column=FALSE,
                          FUN=function(z) coef(lm(Output1~USGG3M+USGG5YR+USGG30YR,
                                                  data=as.data.frame(z))))
rolling.dates<-rollapply(AssignmentDataRegressionComparison[,1:8],
                         width=Window.width,by=Window.shift,by.column=FALSE,
                         FUN=function(z) rownames(z))

rsqrd<-r.squared[idxDate,2]

high.slopespread.periods<-rownames(Coefficients)[Coefficients[,3]-Coefficients[,4]>3]
high.slope5Y<-rownames(Coefficients)[Coefficients[,3]>2.5]
high.slopespread.periods

# P-values
Pvalues<-rollapply(AssignmentDataRegressionComparison,width=Window.width,
                   by=Window.shift,by.column=FALSE,
                   FUN=function(z) summary(lm(Output1~USGG3M+USGG5YR+USGG30YR,                                                                              data=as.data.frame(z)))$coefficients[,4])
rownames(Pvalues)<-rolling.dates[,10]

rownames(Coefficients)<-rolling.dates[,10]
Pvalues
cfnts
)

Pvalues[testDate,]

idxDate
idxDate<-match(testDate,rolling.dates[,10])   #what was r2 on this date

AssignmentDataRegressionComparison[testDate,]

cfnts
cfnts<-Coefficients[testDate,]
pvls<-Pvalues[testDate,]
prdctn<--6.557026
prdctn
highestSensitivity<-'USGG5YR'
rsqrd<-r.squared[idxDate,2]
cfnts
AssignmentDataRegressionComparison[testDate,]
res<-list(Date=testDate,Coefficients=cfnts,
          P_values=pvls,R_squared=rsqrd,Prediction=prdctn,
          HighSensitivity=highestSensitivity)

res
saveRDS(res, file = paste('C:/Users/Daniel Tallarico/Desktop/StatsAnalysis/result.rds',sep = '/'))

model<-lm(Output1~USGG3M+USGG5YR+USGG30YR,data=AssignmentDataRegressionComparison)
x<-predict(model, (newdata = as.data.frame(cfnts))
model
           
           
cfnts
AssignmentDataRegressionComparison[testDate,] 
x=(-13.1532439 +  1.2293553*1.2509 +  1.4654232*3.9096 +   -0.125381*5.3501 )
x


#Step 5

AssignmentDataPCA<-AssignmentData[,1:7]
dim(AssignmentDataPCA)
library(rgl)
