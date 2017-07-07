library(readr)
retail <- read.csv("C:/Users/zmy/Desktop/Online Retail.csv")

# 删除缺失值
retail <- na.omit(retail)

# 删除单价小于等于零的值
retail <- retail[retail$UnitPrice >0,]

# 找有取消订单的用户的所有订单
retail$flag <- ifelse(substr(retail$InvoiceNo,1,1)=="C",1,0)

# 原始数据设置为时间
retail$InvoiceDate <- as.POSIXlt(retail$InvoiceDate)

# 按时间排序
retail <- retail[order(retail$InvoiceDate,decreasing = T),]

# 生成key
retail$Id <- 1:nrow(retail)

# 建立取消订单数据集
# flag标记为1的，为以C开头的取消订单，肯定要删
CancelRe <- subset(retail,flag==1)

#flag 标记为2的，CustID中有过取消订单，为嫌疑犯，可能要删
retail[(which((retail$flag ==0) &(retail$CustomerID  %in% CancelRe$CustomerID))),"flag"] <- 2
suspect <- subset(retail,flag==2)


# 准备写循环
suspect$union <- paste(suspect$CustomerID,suspect$StockCode,sep = "-")
CancelRe$union <- paste(CancelRe$CustomerID,CancelRe$StockCode,sep = "-")

retail$num <- NA

# 跑循环前，建立备份数据
Cancel <- CancelRe
retai <- retail
suspec <- suspect

for(i in 1:nrow(Cancel)){
    s1 <- suspec[which(suspec$union == Cancel[i,"union"],suspec$flag ==2),c("Id","union","InvoiceDate","InvoiceNo","Quantity","flag")]
    s1 <- subset(s1,s1$InvoiceDate <Cancel[i,"InvoiceDate"])
    if(nrow(s1)==0){
        Cancel$flag[i]="NotFind"
    }
    else{
        retai$num[which(retai$Id == s1[1,1])] <- Cancel[i,"Quantity"]
        Cancel[i,"flag"] <- retai[which(retai$Id == s1[1,1]),"Quantity"]
        print(i)
    }
}

# 如果把数据直接赋值回数据，应该可以手动循环了，重新构造循环函数

for(i in 1:nrow(Cancel)){
    s1 <- suspec[which(suspec$union == Cancel[i,"union"],suspec$flag ==2),c("Id","union","InvoiceDate","InvoiceNo","Quantity","flag")]
    s1 <- subset(s1,s1$InvoiceDate <Cancel[i,"InvoiceDate"])
    if(nrow(s1)==0){
        Cancel$flag[i]="NotFind"
    }
    else{
        rowNum <- which(retai$Id == s1[1,1])
        sumNum <- Cancel[i,"Quantity"]+retai$Quantity[rowNum]
        if(sumNum >=0){
            retai$Quantity[rowNum] <- sumNum
            retai$flag[rowNum] <- ifelse(sumNum==0,0,2)
        
            Cancel[i,"flag"] <- retai[which(retai$Id == s1[1,1]),"Quantity"]
        }
        else{
            for(j in 1:nrow(s1)){
                
            }
        }
        
        
        print(i)
    }
}



write.csv(reta,"C:/Users/zmy/Desktop/reta.csv")
write.csv(canc,"C:/Users/zmy/Desktop/cancel.csv")