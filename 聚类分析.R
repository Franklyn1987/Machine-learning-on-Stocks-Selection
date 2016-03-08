rm(list=ls())
#读入训练集数据，并去掉数据的第一列超额对数收益率
TrainData<-read.csv("G:/建模大赛/选股数据集/DataForRegression_Train.csv",header=T)
TrainData<-TrainData[,-1]
#去掉数据中存在因子值为0的行，并将数据保存在Train_Data1数据框中
Factors<-c()
for(i in 1:nrow(TrainData)){
  Factors[i]<-all(TrainData[i,]!=0)
}
Train_Data1<-TrainData[Factors,]
#统一因子对收益率的影响方向为正向，将反向关系的因子值取倒数，并将新数据保存在Train_Data矩阵中
Train_Data<-matrix(nrow=nrow(Train_Data1),ncol=11)
Vectors<-c(rep(0,4),rep(1,2),rep(0,5))#方向向量，值为0表示对应列因子为反向影响
for (i in 1:length(Vectors)){
  if(Vectors[i]==0){
    Train_Data[,i]<-1/Train_Data1[,i]
  } else Train_Data[,i]<-Train_Data1[,i]
}
Train_Data<-as.data.frame(Train_Data)
Mean<-apply(Train_Data,2,mean)
Sd<-apply(Train_Data,2,sd)
XSC<-t(scale(Train_Data))
HC_Complete<-hclust(dist(XSC),method="complete")
Weights<-c(0.589286,0.714286,0.660714)
setwd("G:/建模大赛/选股数据集/Predict")
name<-dir()
filename<-as.character(1:length(name))
ExReturn<-c()
#创建矩阵来保存每月选出的50只股票代码
Stock_List<-matrix(nrow=50,ncol=6,byrow=F)
#循环读入6个月测试集数据
for (i in 1:length(name)){
  assign(filename[i],read.csv(name[i],header=TRUE,stringsAsFactors = FALSE))
#去掉原始数据集中的第一行，并保存在数据框file_name1中
  file_name1<-get(filename[i])[-1,]
#去掉缺省行和第1、2两列，并保存在数据框file_name2中
  file_name2<-na.omit(file_name1)[,c(-1,-2)]
#去掉数据框file_name2中取值为0的行
  factors<-c()
  for(i in 1:nrow(file_name2)){
    factors[i]<-all(file_name2[i,]!=0)
  }
  file_name2<-file_name2[factors,]
#保存处理后相应的股票代码和超额收益率数据到数据框file_name3中
  file_name3<-na.omit(file_name1)[factors,c(1,2)]
#统一因子影响方向，方向为0的因子值取倒数
  for (i in 1:length(Vectors)){
    if(Vectors[i]==0){
      file_name2[,i]<-1/file_name2[,i]
    } else file_name2[,i]<-file_name2[,i]
  }
#对因子值进行标准化处理
  scaled_data<-t(apply(file_name2,1,function(x){(x-Mean)/Sd}))
  Prin_Score<-scaled_data[,c(2,3,11)]%*%Weights
  file_name3$Score<-Prin_Score
  Sort_Stock<-file_name3[order(file_name3[,3],decreasing=TRUE),]
  Select_Stock<-head(Sort_Stock,50L)
  Stock_List[,i]<-Select_Stock[,1,drop=TRUE]
  Stock_Selected<-as.numeric(Select_Stock[,2])
  ExReturn[i]<-mean(Stock_Selected)
}
Stock_List<-as.data.frame(Stock_List)

colnames(Stock_List)<-c("1月","2月","3月","4月","5月","6月")
write.csv(Stock_List,"G:/建模大赛/Cluster_stock.csv", row.names=F)
CumReturn<-cumprod(ExReturn+1)
Date<-as.Date(c("20150130","20150227","20150331","20150430","20150529",
                "20150630"),"%Y%m%d")
Portflio<-data.frame(Date,ExReturn,CumReturn)
pdf("G:/建模大赛/Cluster_return.pdf")
plot(Portflio$Date,Portflio$CumReturn,type="l",lwd=2,ylim=c(0.95,1),xlab="Date",
     ylab="Cumulative Return",pin=c(5,1.5),col="red")
dev.off()

