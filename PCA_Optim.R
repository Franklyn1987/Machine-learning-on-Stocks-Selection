Return<-read.csv(file.choose("G:/建模大赛/成分股对数收益率/ReturnWeekly.csv"),
                 header=TRUE,stringsAsFactors = F)
Return_Month<-read.csv(file.choose("G:/建模大赛/成分股对数收益率/ReturnWeekly.csv"),
                       header=TRUE,stringsAsFactors = F)
PCA_stock<-read.csv("G:/建模大赛/选股结果/PCA_stock.csv",header=T,stringsAsFactors = F)
colnames(Return)<-gsub("X","",colnames(Return))
colnames(Return_Month)<-gsub("X","",colnames(Return_Month))
for (i in 1:6){
  PCA_stock[,i]<-gsub("X","",PCA_stock[,i])
}
Stock_List<-list()
Stock_List1<-list()
library(timeDate)
library(timeSeries)
library(fBasics)
library(fAssets)
library(fPortfolio)
RPortfolio_EWeight<-c()
RPortfolio_UWeight<-c()
for (i in 1:6){
  Stock_List[[i]]<-na.omit(Return[,c(1,match(PCA_stock[,i],colnames(Return)))])
  Stock_List1[[i]]<-Return_Month[,match(PCA_stock[,i],colnames(Return_Month))]
  Code<-colnames(Stock_List1[[i]]) 
  Yeild<-Stock_List1[[i]][56+i,]
  Yeild<-t(Yeild)
  Stock_List[[i]][,1]<-as.Date(Stock_List[[i]][,1])
  Stock_List[[i]][,1]<-as.character(Stock_List[[i]][,1])
  x<-as.timeSeries(Stock_List[[i]])
  y<-100*x
  qiexian<-portfolioSpec()
  qiexianzuhe<-tangencyPortfolio(data=y,spec=qiexian,constraints = "LongOnly")
  z<-print(qiexianzuhe)
  Weights<-z@portfolio@portfolio$weights
  Wreturn<-Yeild*Weights
  OPtim_Stock<-data.frame(Code,Yeild,Weights,Wreturn)
  OPtim_Stock
  RPortfolio_EWeight[i]<-mean(OPtim_Stock[,2])
  RPortfolio_UWeight[i]<-sum(OPtim_Stock[,4])
}
RPortfolio_EWeight
RPortfolio_UWeight
Cum_EWeight<-cumsum(RPortfolio_EWeight)
Cum_UWeight<-cumsum(RPortfolio_UWeight)
HS300<-c(-0.028507569,0.039522311,0.125652483,0.159106497,0.018965369,-0.079027199)
PCA_optim<-data.frame(RPortfolio_EWeight,Cum_EWeight,RPortfolio_UWeight,
                          Cum_UWeight,HS300)
write.csv(PCA_optim,"G:/建模大赛/选股结果/PCA_Optim.csv",row.names = F)
