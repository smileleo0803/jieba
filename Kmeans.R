kmeans5 <- U.CCAT_5[,c("Gender","Age","CUS071","CUS081","SumAMT","TranCount",
                      "SumAUM","SumNET","DiscountRate")]
kmeans5$Gender <- as.numeric(kmeans5$Gender)
#去除非數值欄位
str(kmeans5)
kmeans5 <- kmeans5[complete.cases(kmeans5), ]
# s_kmeans5 <- scale(kmeans5)
# head(s_kmeans5,5)

cor(kmeans5)
cov(kmeans5)


################################################################################
#決定k個數
library(factoextra)
#Elbow Method應用在kmeans
fviz_nbclust(kmeans5,
             FUNcluster = kmeans ,
             method ="wss",
             k.max = 12)+
  labs(title="Elbow")+
  geom_vline(xintercept = 3,
             linetype =2)

###################################################################################


set.seed(123)
kmeans5.result <- kmeans(kmeans5,10) 
kmeans5.result

kmeans5_8.result <- kmeans(kmeans5,8)
kmeans5_8.result


str(kmeans5.result)

library(factoextra)
fviz_cluster( kmeans5.4.result, data = kmeans5)


##############################################################################################

kmeans5 <- U.CCAT_5[,c(4:6,10:13)]
kmeans5$Gender <- as.numeric(kmeans5$Gender)
#去除非數值欄位
str(kmeans5)
kmeans5 <- kmeans5[complete.cases(kmeans5), ]

cor(kmeans5)
cov(kmeans5)

set.seed(1234)
kmeans5.result <- kmeans(kmeans5,6) #分6群
kmeans5.8.result <- kmeans(kmeans5,8) #分4群
kmeans5.result
kmeans5.8.result


kmeans5.result$cluster <- as.factor(kmeans5.result$cluster)
kk <- subset(kmeans5.result,kmeans5.result[[1]])

kk <- subset(kmeans5.result , kmeans5.result[[1]]==3)
str(kmeans5.result)

library(factoextra)
fviz_cluster( kmeans5.4.result, data = kmeans5)




########################################################################
plot(kmeans5[,c("SumNET","Age")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "Age",
     xlim=c(0,3000000),ylim=c(0,110),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","Age")],col=1:6,pch=8,cex=3)


plot(kmeans5[,c("SumNET","SumAMT")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "近一年總交易量",
     xlim=c(0,600000),ylim=c(0,4000000000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","SumAMT")],col=1:6,pch=8,cex=3)


plot(kmeans5[,c("SumNET","SumAUM")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "2017/09/30 AUM",
     xlim=c(0,3000000),ylim=c(0,800000000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","SumAUM")],col=1:6,pch=8,cex=3)


plot(kmeans5[,c("SumNET","TranCount")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "近一年總交易次數",
     xlim=c(0,4000000),ylim=c(0,80000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","TranCount")],col=1:6,pch=8,cex=3)


plot(kmeans5[,c("SumNET","DiscountRate")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "近一年平均折讓率",
     xlim=c(0,6000000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","DiscountRate")],col=1:6,pch=8,cex=3)


###############################################################################
plot(kmeans5[,c("SumAUM","Age")],
     main="近一年實動戶客戶分群",
     xlab= "2017/09/30 AUM" , ylab= "Age",
     xlim=c(0,500000000),ylim=c(0,110),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumAUM","Age")],col=1:6,pch=8,cex=3)


options(scipen=999)
plot(kmeans5[,c("SumAUM","SumNET")],
     main="近一年實動戶客戶分群",
     xlab= "2017/09/30 AUM" , ylab= "一年總計淨收入",
     xlim=c(0,500000000),ylim=c(0,3000000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumAUM","SumNET")],col=1:6,pch=8,cex=3)


options(scipen=999)
plot(kmeans5[,c("SumAUM","SumAMT")],
     main="近一年實動戶客戶分群",
     xlab= "2017/09/30 AUM" , ylab= "一年總計交易量",
     xlim=c(0,500000000),ylim=c(0,30000000000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumAUM","SumAMT")],col=1:6,pch=8,cex=3)


options(scipen=999)
plot(kmeans5[,c("SumAUM","TranCount")],
     main="近一年實動戶客戶分群",
     xlab= "2017/09/30 AUM" , ylab= "一年總計交易次數",
     xlim=c(0,500000000),ylim=c(0,50000),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumAUM","TranCount")],col=1:6,pch=8,cex=3)


plot(kmeans5[,c("SumNET","DiscountRate")],
     main="近一年實動戶客戶分群",
     xlab= "近一年總淨收入" , ylab= "年平均折讓率",
     #xlim=c(0,500000000),ylim=c(0,1),
     col=kmeans5.result$cluster)
points(kmeans5.result$centers[,c("SumNET","DiscountRate")],col=1:5,pch=8,cex=3)


kmeans5
table(U.CCAT_5$Gender,kmeans5.result$cluster)
plot(kmeans5,col=kmeans5.result$cluster)













