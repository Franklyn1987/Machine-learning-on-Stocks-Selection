factor_dir <- dir("试题/附录2：300支股票对应的财务指标",full.names=T,pattern="*.csv")
factor_dir_name <- dir("试题/附录2：300支股票对应的财务指标",full.names=F,pattern="*.csv")
factor_list <- lapply(factor_dir,function(fl){read.csv(fl, header = T, stringsAsFactors = F)})
stock_dir <- dir("试题/附录3：沪深300成分股日线数据",full.names=T,pattern="*.txt")
stock_date <<- NULL
day.begin.factor <- 20100331
day.end.factor <- 20141230
day.begin <- 20100630
day.end <- 20141231
stock_list <- lapply(stock_dir,function(fl)
  {
    tt <- read.table(fl,skip=2,header=F,sep=",",fill=T,stringsAsFactors=F)[,c(1,5)]
    tt <- tt[-nrow(tt),]
    colnames(tt) <- c("date","close")
    tt[,1 ]<- as.numeric(tt[,1])
    tt <- tt[(tt$date>=day.begin&tt$date<=day.end),]
    stock_date <<- union(stock_date,tt[,1])
    return(tt)
  })
stock_date <- sort(stock_date)
stock_name <- substr(stock_dir,nchar(stock_dir)-9,nchar(stock_dir)-4)
stock_return <- data.frame(matrix(NA,nrow=length(stock_date),ncol=length(stock_name)+1))
colnames(stock_return) <- c("date",stock_name)
stock_return[,1] <- stock_date
stock_date_index <- rep(1,length(stock_name))
list_index <<-2
lapply(stock_list,function(x)
  {
    index <- match(x$date,stock_date)
    stock_return[index,list_index] <<- x[,2]
    list_index <<- list_index + 1
  })
stock_log <- stock_return[-1,]
stock_log[,-1] <-log(stock_return[-1,-1]/stock_return[-nrow(stock_return),-1])
date_1 <- stock_date[-length(stock_date)]
date_2 <- stock_date[-1]
popup_date <- c(stock_date[1],date_1[which(substr(date_1,5,6)!=substr(date_2,5,6))-1])
popup_date <- c(day.begin.factor,popup_date[c(1:length(popup_date)-1)%%3==0],day.end.factor)
popup_date <- data.frame(d1=popup_date[-length(popup_date)],d2=popup_date[-1],d3=c(popup_date[-(1:2)],Inf))
#计算因子收益的函数
factor_return <<- data.frame(matrix(NA,nrow=length(stock_date),ncol=length(stock_name)+1))
factor_stockrank <<- data.frame(matrix(NA,nrow=length(stock_date),ncol=length(stock_name)+1))
rank.of.stock <- match(stock_name,substr(colnames(factor_list[[1]]),2,7)[-1])
factor_list<-lapply(factor_list,function(x)
{
  x<-x[,c(1,rank.of.stock+1)]
  x[,1]<-as.numeric(format(as.Date(x$date),"%Y%m%d"))
  return(x)
})
name_i <<- 1
lapply(factor_list,function(x)
{
  stock_log_tmp <<- stock_log
  apply(popup_date,1,function(y)
  {
    x <- x[x$date==y[1],]
    x <- x[-1]
    x_rank <- rank(x)
    index_tmp <- (stock_log_tmp$date>=y[2])&(stock_log_tmp$date<=y[3])
    if(length(index_tmp)>0)
    {
      tmp_log <- stock_log_tmp[index_tmp,]
      tmp_log <- tmp_log[,c(1,x_rank+1)]
      stock_log_tmp[index_tmp,] <<- tmp_log
    }
  })
  write.csv(stock_log_tmp,paste("output/1/","未分五档",factor_dir_name[name_i],sep=""),row.names=F)
  name_i <<- name_i+1
})

