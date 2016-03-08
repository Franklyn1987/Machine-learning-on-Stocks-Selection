library(WindR)
w.start()
code<-w.wset('IndexConstituent','date=20151105;windcode=000300.SH')$Data$wind_code

#估计冲击成本函数
lm.sol<-lapply(code,function(x)
{
  tp<-w_wsd_data<-w.wsd(x,"pct_chg,volume","2014-01-01","2014-12-31")$Data
  chg<-w_wsd_data$PCT_CHG[-1]
  vch<-w_wsd_data$VOLUME[-1]-w_wsd_data$VOLUME[-nrow(w_wsd_data)]
  r2<-0
  i<-0
  fit<-NULL
  try({for(k in seq(0.2,2,by=0.2))
  {
    r2.old<-r2
    i.old<-i
    i<-k
    lmsol<-lm(chg~I(vch^k)+0)
    r2<-summary(lmsol)$adj.r.squared
    if(r2<r2.old) {r2<-r2.old;i<-i.old}
  }
  fit<-lm(chg~I(vch^i)+0)
  })
  return(fit)
})
