#回归法选股
#SVR和LM

#Get the list of components of HS300
# library(WindR)
# w.start()
# w_wset_data <- w.wset('SectorConstituent','date=20151108;windcode=000300.SH')$Data
# write.csv(w_wset_data,"HS300成份股.csv",row.names=F)

#Get the relative log return to HS300
MonthLogReturn <- read.csv("hs300月对数收益率.csv",head=T)
MonthLogReturn[,-1] <- apply(MonthLogReturn[,-1],c(1,2),function(x){ifelse(x==0,x<-NA,x)})
StockName<-substr(colnames(MonthLogReturn)[-1],2,7)
# HS300Return <- w.wsd("000300.SH","pct_chg","2010-05-31","2015-06-30","Period=M")$Data
# HS300LogReturn <- HS300Return
# HS300LogReturn[,-1] <- log(1+HS300Return[,-1]/100)
# write.csv(HS300LogReturn,"HS300指数对数收益率.csv",row.names=F)
HS300LogReturn <- read.csv("HS300指数对数收益率.csv",head=T,stringsAsFactors=F)
ELReturn <- MonthLogReturn
ELReturn[,-1] <- apply(MonthLogReturn[,-1],2,function(x){x-HS300LogReturn[,-1]})
ELReturn <- ELReturn[as.numeric(format(as.Date(ELReturn$DATETIME),"%Y%m%d"))>=20100930,]
#Get the factor list
stockdate<-c("20100331","20100430",format(as.Date(HS300LogReturn$DATETIME),"%Y%m%d"))
stockdate1<-stockdate[as.numeric(stockdate)<=20141231]
stockdate2<-stockdate[as.numeric(stockdate)>=20100831&as.numeric(stockdate)<=20150531]
FactorDir1 <- dir("试题/附录2：300支股票对应的财务指标/",full.names=T,pattern=".csv")
FactorName1 <- dir("试题/附录2：300支股票对应的财务指标/",full.names=F,pattern=".csv")
FactorList1 <- lapply(FactorDir1,function(x)
  {
    tplist <- read.csv(x,head=T)
    tplist$date<-format(as.Date(tplist$date),"%Y%m%d")
    tplist <- tplist[match(stockdate1,tplist$date),]
    return(tplist)
  })
FactorDir2 <- dir("试题/自找数据/factor/",full.names=T)
FactorName2 <- dir("试题/自找数据/factor/",full.names=F)
FactorList2 <- lapply(FactorDir2,function(x)
  {
    tplist <- read.csv(x, header = F, stringsAsFactors = F,skip = 3)
    names <- read.csv(x, header = F, stringsAsFactors = F, nrows = 1)
    names[1]<-"date"
    colnames(tplist)<-names
    tplist$date<-format(as.Date(tplist$date),"%Y%m%d")
    tplist <- tplist[match(stockdate2,tplist$date),]
    return(tplist)
  })

FinalTable <- NULL
for(i in 2:ncol(ELReturn))
{
  index <- as.numeric(format(as.Date(ELReturn$DATETIME),"%Y%m%d"))<=20141231
  TmpTable <- matrix(ELReturn[index,i],ncol=1)
  for(tp in FactorList1)  {tp<-tp[index,c(1,match(colnames(ELReturn[,-1]),colnames(tp[,-1]))+1)];TmpTable <- cbind(TmpTable,tp[,i])}
  for(tp in FactorList2)  {tp<-tp[index,c(1,match(colnames(ELReturn[,-1]),paste("X",colnames(tp[,-1]),sep=""))+1)];TmpTable <- cbind(TmpTable,tp[,i])}
  FinalTable <- rbind(FinalTable,TmpTable)
}

EffectiveIndex <- c(1,22,23,25,27,7,10,13,31,17,18,35) #这个是对着那个excel表选出来

PredictDate <- stockdate[stockdate>20141231]
for(i in PredictDate)
{
  index <- as.numeric(format(as.Date(ELReturn$DATETIME),"%Y%m%d"))==i
  TmpTable <- ELReturn[index,]
  names <- colnames(TmpTable)
  for(tp in FactorList1) 
  {
    tp <- tp[index,c(1,match(colnames(ELReturn[,-1]),colnames(tp[,-1]))+1)];
    colnames(tp) <- names;
    TmpTable <- rbind(TmpTable,tp,deparse.level = 0);
  }
  for(tp in FactorList2) 
  {
    tp <- tp[index,c(1,match(colnames(ELReturn[,-1]),paste("X",colnames(tp[,-1]),sep=""))+1)];
    colnames(tp) <- names;
    TmpTable <- rbind(TmpTable,tp,deparse.level = 0);
  }
  names <- c("超额对数收益率",substr(FactorName1,1,nchar(FactorName1)-4),substr(FactorName2,1,nchar(FactorName2)-4))
  rownames(TmpTable) <- names
  TmpTable <- TmpTable[EffectiveIndex,]
  write.csv(t(TmpTable),paste("DataForRegression_Predict/",i,".csv",sep=""))
}

names <- c("超额对数收益率",substr(FactorName1,1,nchar(FactorName1)-4),substr(FactorName2,1,nchar(FactorName2)-4))
colnames(FinalTable) <- names

#Choose the effective factors

EffectiveTable <- FinalTable[,c(1,EffectiveIndex)]
# EffectiveTable <- FinalTable
EffectiveTable <- EffectiveTable[complete.cases(EffectiveTable),]
write.csv(EffectiveTable,"DataForRegression_Train.csv",row.names=F)


EffectiveTable <- read.csv("DataForRegression_Train.csv",stringsAsFactor=F,head=T)
colmeans <- colMeans(EffectiveTable)
colsd <- apply(EffectiveTable,2,sd)

EffectiveTable <- scale(EffectiveTable,center=T,scale=T)
EffectiveTable <- data.frame(EffectiveTable)

PredictDir <- dir("DataForRegression_Predict/",full.names = T)
PredictTable <- lapply(PredictDir,function(x)
  {
    tp <- read.csv(x,head=F,skip=2,stringsAsFactors=F)
    names <- read.csv(x,head=F,nrows=1,stringsAsFactors=F)
    names[1,1]<-"code"
    colnames(tp)<-names[1,]
    tp
  })




#Linear Regression
lm.sol <- lm(EffectiveTable$超额对数收益率 ~ ., data=EffectiveTable)
RegressionResult.lm <- predict(lm.sol,newdata=EffectiveTable[,-1]) 
plot(RegressionResult.lm,EffectiveTable$超额对数收益率)
names<-colnames(EffectiveTable)
lmStock <<- data.frame(matrix(0,ncol=6,nrow=50))
count <<- 1
PReturn<-sapply(PredictTable,function(x)
  {
    names<-colnames(EffectiveTable)
    x<-x[complete.cases(x),]
    y<-(x[,-(1:2)]-colmeans[-1])/colsd[-1]
    y<-x[,-(1:2)]
    colnames(y)<-names[-1]
    pre<-predict(lm.sol,newdata=y)
    index<-order(pre,decreasing=T)[1:50]
    lmStock[,count]<<-substr(x[index,1],2,10)
    count <<- count+1
    mean(x[index,2],na.rm=T)
  })
write.csv(lmStock,"回归结果/lm_stock.csv",row.names=F)
result.lm <- cbind(PReturn,cumsum(PReturn))
colnames(result.lm)<-c("每月超额收益率","累积超额收益率")
write.csv(result.lm,"回归结果/lm_return.csv")
plot(cumsum(PReturn),type="l",col="red",xlab="2015年前六月",ylab="累计收益率",main="线性回归")
#SVM
if(!require(e1071)) {install.packages("e1071");library(e1071)}
svm.sol<-svm(EffectiveTable$超额对数收益率~.,data=EffectiveTable,cost=1.4)
RegressionResult.svm <- predict(svm.sol,EffectiveTable[,2:ncol(EffectiveTable)])
plot(RegressionResult.svm,EffectiveTable$超额对数收益率)
svmStock <<- data.frame(matrix(0,ncol=6,nrow=50))
count <<- 1
PReturn<-sapply(PredictTable,function(x)
{
  names<-colnames(EffectiveTable)
  x<-x[complete.cases(x),]
  y<-x[,-(1:2)]
  y<-(x[,-(1:2)]-colmeans[-1])/colsd[-1]
  colnames(y)<-names[-1]
  pre<-predict(svm.sol,newdata=y)
  index<-order(pre,decreasing=T)[1:50]
  svmStock[,count]<<-substr(x[index,1],2,10)
  count <<- count+1
  mean(x[index,2],na.rm=T)
})
write.csv(svmStock,"回归结果/svm_stock.csv",row.names=F)
result.svm <- cbind(PReturn,cumsum(PReturn))
colnames(result.svm)<-c("每月超额收益率","累积超额收益率")
write.csv(result.svm,"回归结果/svm_return.csv")
plot(cumsum(PReturn),type="l",col="red",xlab="2015年前六月",ylab="累计收益率",main="SVR回归")
#randomForest
if(!(require(randomForest))) {install.packages("randomForest");library(randomForest)}
rdForest.sol <- randomForest(EffectiveTable$超额对数收益率~.,data=EffectiveTable)
RegressionResult.rdForest <- predict(rdForest.sol,newdata=EffectiveTable[,2:ncol(EffectiveTable)])
plot(EffectiveTable$超额对数收益率,RegressionResult.rdForest)
rdFStock <<- data.frame(matrix(0,ncol=6,nrow=50))
count <<- 1
PReturn<-sapply(PredictTable,function(x)
{
  names<-colnames(EffectiveTable)
  x<-x[complete.cases(x),]
  y<-x[,-(1:2)]
  y<-(x[,-(1:2)]-colmeans[-1])/colsd[-1]
  colnames(y)<-names[-1]
  pre<-predict(rdForest.sol,newdata=y)
  index<-order(pre,decreasing=T)[1:50]
  rdFStock[,count]<<-substr(x[index,1],2,10)
  count <<- count+1
  mean(x[index,2],na.rm=T)
})
write.csv(rdFStock,"回归结果/rdF_stock.csv",row.names=F)
result.rdF <- cbind(PReturn,cumsum(PReturn))
colnames(result.rdF)<-c("每月超额收益率","累积超额收益率")
write.csv(result.rdF,"回归结果/rdF_return.csv")
plot(cumsum(PReturn),type="l",col="red",xlab="2015年前六月",ylab="累计收益率",main="RandomForest回归")
