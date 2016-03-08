rm(list=ls())
HS300<-read.csv("G:/建模大赛/000300月收盘价.csv",stringsAsFactors=F)
LogReturn<-diff(log(HS300$收盘价))[1:56]
LogReturn<-data.frame(LogReturn)
setwd("G:/建模大赛/排序未分组因子")
name<-dir()
filename<-as.character(1:length(name))
Factor_Performance<-matrix(nrow=30,ncol=4)
Vectors<-c(rep(0,5),1,0,rep(1,11),rep(0,2),rep(1,3),rep(0,4),rep(1,3))
FacReturn<-matrix(nrow=56,ncol=30)
FacList<-list()
Group_Return<-list()
for (i in 1:length(name)){
  options(digits=4)
  assign(filename[i],read.csv(name[i],header=TRUE,stringsAsFactors = F))
  file_name1<-get(filename[i])[1:56,2:ncol(get(filename[i]))]
  file_name2<-list()
  group_mean<-matrix(nrow=nrow(file_name1),ncol=5)
  for(n in 1:nrow(file_name1)){
    file_name2[[n]]<-na.omit(unlist(file_name1[n,,drop=TRUE]))
    group<-floor(quantile(1:length(file_name2[[n]]),c(0.2,0.4,0.6,0.8,1.0)))
    groupmean1<-mean(file_name2[[n]][1:group[1]])
    groupmean2<-mean(file_name2[[n]][(group[1]+1):group[2]])
    groupmean3<-mean(file_name2[[n]][(group[2]+1):group[3]])
    groupmean4<-mean(file_name2[[n]][(group[3]+1):group[4]])
    groupmean5<-mean(file_name2[[n]][(group[4]+1):group[5]])
    groupmean<-c(groupmean1,groupmean2,groupmean3,groupmean4,groupmean5)
    group_mean[n,]<-groupmean
  }
  group_mean<-as.data.frame(group_mean)
  group_mean<-cbind(group_mean,LogReturn)
  Group_Return[[i]]<-group_mean
  group_mean<-apply(group_mean,2,function(x){x+1})
  group_mean<-apply(group_mean,2,cumprod)
  group_mean<-cbind(unlist(get(filename[i])[1:56,1,drop=TRUE]),group_mean)
  group_mean<-as.data.frame(group_mean)  #把对象变回数据框
  if (Vectors[i]==1) {
    colnames(group_mean)<-c("Date","Top","Class2","Class3","Class4","Bottom","BenchMark")
  } else colnames(group_mean)<-c("Date","Bottom","Class4","Class3","Class2","Top","BenchMark")
  group_mean[,1]<-as.character(group_mean[,1])
  group_mean[,1]<-as.Date(group_mean[,1],"%Y%m%d")
  FacList[[i]]<-group_mean
}
FacName<-gsub(".csv","",name)
pdf("G:/建模大赛/30个因子五分位组合累计收益.pdf",family="GB1")
opar<-par(no.readonly=TRUE)
par(lwd=2,lty=1,pin=c(6,5))
for(i in 1:30){
  FactorName<-FacName[i]
  plot(FacList[[i]]$Date,FacList[[i]]$Top,main=FactorName,type="l",xlab="日期",ylab="因子累计收益",col="red")
  lines(FacList[[i]]$Date,FacList[[i]]$Class2,type="l",col="purple")
  lines(FacList[[i]]$Date,FacList[[i]]$Class3,type="l",col="blue")
  lines(FacList[[i]]$Date,FacList[[i]]$Class4,type="l",col="green")
  lines(FacList[[i]]$Date,FacList[[i]]$Bottom,type="l",col="black")
  lines(FacList[[i]]$Date,FacList[[i]]$BenchMark,type="l",col="gray")
  legend("top",title=("Class"),c("Top","Class2","Class3","Class4","Bottom","BenchMark"),
         lty=c(rep(1,6)),col=c("red","purple","blue","green","black","gray"))
}
par(opar)
dev.off()