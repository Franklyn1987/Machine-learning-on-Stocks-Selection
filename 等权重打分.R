load("E:/金融建模/data/data_for_trainging_and_prediction.RData")

# 提取数据
S1 <- FactorList2[[1]] # PB市净率
S2 <- FactorList2[[2]] # PCF市现率
S3 <- FactorList2[[4]] # PS市销率
S4 <- FactorList2[[6]] # 换手率
S5 <- FactorList2[[10]] # 市盈率TTM
S6 <- FactorList2[[14]] #总市值

S6list <- list() #将S6的数据修改成数值
for(i in 1:301){
    S6list[[i]] <- do.call(rbind,lapply(strsplit(S6[,i],','), paste, collapse = ''))
}
temp <- do.call(cbind, S6list)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(S6)
S6 <- temp 

F1 <- FactorList1[[6]] # 每股经营活动产生的现金流量净额
F2 <- FactorList1[[9]] # 每股收益EPS
F3 <- FactorList1[[12]] # 权益乘数
F4 <- FactorList1[[16]] # 资产负债率
F5 <- FactorList1[[17]] # 总负债（同比增长率）

colnames(S1) <- paste("X", colnames(S1), sep = '')
colnames(S2) <- paste("X", colnames(S2), sep = '')
colnames(S3) <- paste("X", colnames(S3), sep = '')
colnames(S4) <- paste("X", colnames(S4), sep = '')
colnames(S5) <- paste("X", colnames(S5), sep = '')
colnames(S6) <- paste("X", colnames(S6), sep = '')

# 获取2015年上半年所需因子数据
FacName <- c(paste('S', c(1:6), sep = ''), paste('F', c(1:5), sep = ''))
for(i in 1:11){
    assign(FacName[i], get(FacName[i])[53:58,]) 
}

# 标准化数据
DataScale <- function(dt){
    dtt <- t(dt[, -1])
    time <- t(as.numeric(dt[, 1]))
    temp <- apply(dtt, 2, function(x){scale(as.numeric(x))}) 
    
    temp <- cbind(row.names(dtt), as.data.frame(temp))
}

for(i in 1:11){
    assign(FacName[i], DataScale(get(FacName[i])))
} # 进行标准化

# 合并数据
MaIndex <- match(S1[,1], F1[,1])
temp <- cbind(S1, S2[,-1], S3[,-1], S4[,-1], S5[,-1], S6[,-1],
              F1[MaIndex,-1], F2[MaIndex,-1], F3[MaIndex,-1],
              F4[MaIndex,-1], F5[MaIndex,-1])

Index <- seq(from = 2, to = 67, 6)
fac1 <- temp[, c(1, Index)] # 选取2015年1月股票组合所需因子暴露
fac2 <- temp[, c(1, Index+1)] # 选取2015年2月股票组合所需因子暴露
fac3 <- temp[, c(1, Index+2)]
fac4 <- temp[, c(1, Index+3)]
fac5 <- temp[, c(1, Index+4)]
fac6 <- temp[, c(1, Index+5)]

# 等权重求得分排序
Rank <- function(Fdata){
    Score <- apply(Fdata[, c(8, 9)], 1, sum) - apply(Fdata[, c(-1, -8, -9)], 1, sum)
    Stock <- cbind(as.data.frame(Fdata[,1]), Score)
    as.character(Stock[order(Stock[,2], decreasing = T)[1:50], 1])
}

TELReturn <- t(ELReturn)
ELReturn1 <- TELReturn[match(Rank(fac1), row.names(TELReturn)), 53]
ELReturn2 <- TELReturn[match(Rank(fac2), row.names(TELReturn)), 54]
ELReturn3 <- TELReturn[match(Rank(fac3), row.names(TELReturn)), 55]
ELReturn4 <- TELReturn[match(Rank(fac4), row.names(TELReturn)), 56]
ELReturn5 <- TELReturn[match(Rank(fac5), row.names(TELReturn)), 57]
ELReturn6 <- TELReturn[match(Rank(fac6), row.names(TELReturn)), 58]

ELR <- c(mean(as.numeric(ELReturn1)),
         mean(as.numeric(ELReturn2)),
         mean(as.numeric(ELReturn3)),
         mean(as.numeric(ELReturn4), na.rm = T),
         mean(as.numeric(ELReturn5), na.rm = T),
         mean(as.numeric(ELReturn6)))
plot(cumsum(ELR)*100, type = 'l')

set <- cbind(Rank(fac1),Rank(fac2),Rank(fac3),Rank(fac4),Rank(fac5),Rank(fac6))
set <- as.data.frame(set)
write.csv(set,"等权重方法选股结果.csv")
