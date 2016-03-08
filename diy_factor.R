library(WindR)
w.start()
code300<-w.wset('IndexConstituent','date=20151108;windcode=000300.SH')$Data$wind_code
w_wsd_data<-w.wsd(code300,"close","2010-04-01","2015-06-30","Period=W")$Data
View(w_wsd_data)
write.csv(w_wsd_data,"hs300周收盘价.csv",row.names=F)
w_wsd_data_after <- w_wsd_data[-1,-1]
w_wsd_data_forward <- w_wsd_data[-nrow(w_wsd_data),-1]
LReturnWeek <- w_wsd_data[-1,]
LReturnWeek[,-1]<- log(w_wsd_data_after/w_wsd_data_forward)
write.csv(LReturnWeek,"hs300周对数收益率.csv",row.names=F)
w_wsd_data<-w.wsd(code300,"close","2010-04-01","2015-06-30","Period=M")$Data
write.csv(w_wsd_data,"hs300月收盘价.csv",row.names=F)
w_wsd_data_after <- w_wsd_data[-1,-1]
w_wsd_data_forward <- w_wsd_data[-nrow(w_wsd_data),-1]
LReturnMonth <- w_wsd_data[-1,]
LReturnMonth[,-1]<- log(w_wsd_data_after/w_wsd_data_forward)
write.csv(LReturnMonth,"hs300月对数收益率.csv",row.names=F)

#Begin to sort monthly log return by factors
factor_dir <- dir("试题/自找数据/factor",full.names=T,pattern="*.csv")
factor_dir_name <- dir("试题/自找数据/factor",full.names=F,pattern="*.csv")
factor_list <- lapply(factor_dir,function(fl)
  {
    fltmp <- read.csv(fl, header = F, stringsAsFactors = F,skip = 3)
    names <- read.csv(fl, header = F, stringsAsFactors = F, nrows = 1)
    names[,1] <- "date"
    names[,-1] <- substr(names[1,-1],1,6)
    colnames(fltmp) <- names 
    fltmp$date <- format(as.Date(fltmp$date),"%Y%m%d")
    fltmp
  })
stock_name <- substr(code300,1,6)
stock_log <- LReturnMonth
colnames(stock_log) <- c("date",stock_name)
stock_log$date <- format(as.Date(stock_log$date),"%Y%m%d")

for(j in 1:length(factor_list))
  {
      x <- factor_list[[j]]
      x <- x[as.numeric(x$date)<=20150531,]
      rank_x <- apply(x[,-1],1,function(y){order(y,decreasing=T)})
      tmp_LReturn <- stock_log
      tmp_LReturn <- tmp_LReturn[as.numeric(tmp_LReturn$date)<=20150631,]
      for(i in 1:ncol(rank_x))
      {
        tmp_LReturn[i,-1]<-tmp_LReturn[i,rank_x[,i]+1]
      }
      write.csv(tmp_LReturn,paste(factor_dir_name[j],sep=""),row.names=F)
      print(j)
  }
