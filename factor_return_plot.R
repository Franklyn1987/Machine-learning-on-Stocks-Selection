rm(list())
HS300<-read.csv("G:/建模大赛/000300月收盘价.csv",stringsAsFactors=F)
LogReturn<-diff(log(HS300$收盘价))[1:56]
LogReturn<-LogReturn
LogReturn<-data.frame(LogReturn)
setwd("G:/建模大赛/排序未分组因子")
name<-dir()
filename<-as.character(1:length(name))
Factor_Performance<-matrix(nrow=30,ncol=4)
Vectors<-c(rep(0,5),1,0,rep(1,11),rep(0,2),rep(1,3),rep(0,4),rep(1,3))
FacReturn<-matrix(nrow=56,ncol=30)
for (i in 1:length(name)){
  options(digits=4)
  assign(filename[i],read.csv(name[i],header=TRUE))
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
  group_mean<-cbind(unlist(get(filename[i])[1:56,1,drop=TRUE]),group_mean)
  group_mean<-cbind(group_mean,LogReturn)
  if (Vectors[i]==1) {
    colnames(group_mean)<-c("Date","Top","Class2","Class3","Class4","Bottom","BenchMark")
  } else colnames(group_mean)<-c("Date","Bottom","Class4","Class3","Class2","Top","BenchMark")
  group_mean$Date<-as.character(group_mean$Date)
  group_mean$Date<-as.Date(group_mean$Date,"%Y%m%d")
  FacReturn[,i]<-group_mean$Top-group_mean$Bottom
}
FacName<-gsub(".csv","",name)
pdf("G:/建模大赛/30个因子的逐月收益图.pdf",family="GB1")
for(i in 1:30){
FactorName<-FacName[i]
barplot(names.arg = group_mean$Date,FacReturn[,i],main=FactorName,
        beside = TRUE,xlab="Date",ylab="Factor Return",
        col="blue",pin=c(5,2.5))
}
dev.off()



