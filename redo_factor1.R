dtime<-c("20100331", "20100630", "20100930", "20101231", "20110331", "20110630", "20110930", "20111230", "20120330", "20120629", "20120928", "20121231", "20130329", "20130628", "20130930", "20131231", "20140331", "20140630", "20140930", "20141231", "20150331", "20150630")
factor_dir <- dir("试题/附录2：300支股票对应的财务指标",full.names=T,pattern="*.csv")
factor_dir_name <- dir("试题/附录2：300支股票对应的财务指标",full.names=F,pattern="*.csv")
factor_list <- lapply(factor_dir,function(fl){read.csv(fl, header = T, stringsAsFactors = F)})
tp <- stock_log
stock_log<-stock_log[as.numeric(stock_log$date)>20100930)&as.numeric(stock_log$date)<=20150630),]
what<<-1
lapply(factor_list,function(x)
  {
    tmp_LReturn <- stock_log
    x$date <- format(as.Date(x$date),"%Y%m%d")
    index <- match(dtime,x$date)
    x <- x[index,]
    x <- x[as.numeric(x$date)<=20140930]
    x[is.na(x)]<--Inf
    for(i in 2:ncol(x))
      {
        j <- 1
        while(j<=(nrow(x)-1))
        {
         count <- 1
         while(x[j,i]==x[j+count,i]&x[j,i]!=-Inf&(j+count<=nrow(x)))
         {
           x[j+count,i] <- -Inf
            count <- count+1
         }
         j <- count+j
        }
      }
    for(i in 1:(nrow(x)-1))
      {
        index<-stock_log$date>=x$date[i]&stock_log$date<x$date[i+1]
        tmp_LReturn[index,-1] <- tmp_LReturn[index,order(x[i,-1],decreasing=T)+1]
      }
    write.csv(tmp_LReturn,paste("好困/",factor_dir_name[what],sep=""),row.names=F)
    what <<- what+1
    }
  )