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

# 用备份数据跑循环
Cancel <- CancelRe
retai <- retail
suspec <- suspect

# 构造循环函数

for(i in 1:nrow(Cancel)){
    # 从总体中取顾客id、商品id、标记为2 相等，购买时间小于退货时间的子集
    s1 <- suspec[which(suspec$union == Cancel[i,"union"],suspec$flag ==2),c("Id","union","InvoiceDate","InvoiceNo","Quantity","flag")]
    s1 <- subset(s1,s1$InvoiceDate <Cancel[i,"InvoiceDate"])
    if(nrow(s1)==0){
        Cancel$flag[i]="NotFind"  #没有子集，cancel里标记为“notfind”
    }
    else{
        rowNum <- which(retai$Id == s1[1,1])    # 匹配行号
        sumNum <- Cancel[i,"Quantity"]+retai$Quantity[rowNum]   # 购买 和 退货 相减
        if(sumNum >=0){
            # 相减结果大于等于0
            retai$Quantity[rowNum] <- sumNum    # 原数据赋结果值
            retai$flag[rowNum] <- ifelse(sumNum==0,0,2)     # 若相减为0，标记为0，以后不检索，否则还为2
            Cancel[i,"flag"] <- "over"    # cancel标记“over” 代表结束
        }
        else{
            # 相减结果小于0，需再从中选取子集，循环运算
            rowsN <- s1$Id    # 选取子集行号
            j <- 1
            repeat{
                Cancel[i,"Quantity"] <- Cancel[i,"Quantity"] + retai$Quantity[rowsN[j]]    #相加结果赋值
                retai[rowsN[j],"Quantity"] <- ifelse(Cancel[i,"Quantity"] <=0,0,Cancel[i,"Quantity"])    # 
                retai[rowsN[j],"flag"] <- ifelse(Cancel[i,"Quantity"] <=0,0,1)
                Cancel[i,"flag"] <- ifelse(Cancel[i,"Quantity"] >=0,0,1)  #相加结果大于0，cancel标记为0
                j = j+1
                if(Cancel[i,"flag"]==0|j>length(rowsN)){break}   # 标记为0或循环次数过大，跳出
            }
        }
    }
    print(i)
}

# write.csv(retai,"C:/Users/zmy/Desktop/retai.csv")
# write.csv(Cancel,"C:/Users/zmy/Desktop/cancel.csv")

# 最后选出的结果，全集中数量大于0，去掉flag，id列
reta <- subset(retai,Quantity >0,select = 1:8)
write.csv(reta,"C:/Users/zmy/Desktop/reta.csv")
