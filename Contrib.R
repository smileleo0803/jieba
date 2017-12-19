library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")
#2016/10/01-2017/09/30的客戶貢獻度
Contrib <- sqlQuery(myconn, "select TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , 
                    AMT_ , FEE_ , DISCOUNT_ , EXPENSE_ , NET_ , IDNO_
                    from KGICONTRIB where TDATE_ > 201609 and TDATE_ < 201710  ")

close(myconn) 

class(Contrib)
head(Contrib,10)
nrow(Contrib)
str(Contrib)
summary(Contrib)
#台股客戶2016/10/01-2017/09/30的客戶貢獻度
Contrib <- filter(Contrib,MARKET_ =='1' | MARKET_ == 'R')
str(Contrib)
library("data.table")
Contrib <- as.data.table(Contrib)

#############################################################################################

#BHNO_分支代碼轉字串
Contrib$BHNO_ <- as.character(Contrib$BHNO_)
Contrib$BHNO_  <- str_trim(Contrib$BHNO_,side='both')
#CUSTNO_帳號應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
Contrib$CUSTNO_  <- str_trim(Contrib$CUSTNO_,side='both')
Contrib$CUSTNO_ <- str_pad(Contrib$CUSTNO_,6,"left",'0')
#CUSTCKNO_檢查碼轉字串
Contrib$CUSTCKNO_ <- as.character(Contrib$CUSTCKNO_)
Contrib$CUSTCKNO_  <- str_trim(Contrib$CUSTCKNO_,side='both')
#將BHNO_、CUSTNO_、CUSTCKNO_轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
Contrib$CUSTNO <- paste0(Contrib$BHNO_,Contrib$CUSTNO_,Contrib$CUSTCKNO_)
#ID由Factor轉字串
Contrib$IDNO_ <- as.character(Contrib$IDNO_)


#計算SUM(交易量).SUM(手續費).SUM(手續費折讓).SUM(應扣費用).SUM(淨收入)
sumcount <- sqldf("select CUSTNO ,sum(AMT_) as SumAMT, sum(FEE_) as SumFEE,
                   sum(DISCOUNT_) as SumDISCOUNT,sum(EXPENSE_) as SumEXPENSE,sum(NET_) as SumNET
                  from Contrib
                  group by CUSTNO
                  order by SumNET desc ") 

nrow(sumcount)#330145  
sumcount <- as.data.table(sumcount)

#CusContrib_5 → 貢獻度近一年總量與CUS_5結合
CusContrib_5 <- sqldf("select * from CUS_5 c left join sumcount s on c.CUSTNO = s.CUSTNO")
CusContrib_5 <- CusContrib_5[,c(-1)]
CusContrib_5 <- as.data.table(CusContrib_5)
nrow(CusContrib_5)
str(CusContrib_5)
summary(CusContrib_5)

################################################################################################
#手續折讓比
U.CCAT_5 <- mutate(U.CCAT_5 , DiscountRate =  1-round(SumDISCOUNT / SumFEE ,2))
summary(U.CCAT_5$DiscountRate)
quantile(U.CCAT_5$DiscountRate,na.rm = T)
U.CCAT_5[which(U.CCAT_5$DiscountRate <= 0.1)]

#2016/10-2017/09平均折讓率(0.8以下)
library(ggplot2)
ggplot(U.CCAT_5[which(U.CCAT_5$DiscountRate < 0.8)], aes(DiscountRate))+
  geom_histogram(aes(y = ..count..), binwidth = 0.1)+
  labs(title = "2016/10-2017/09平均折讓率(0.8以下)分配直方圖" , x = "2016/10-2017/09平均折讓率", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$DiscountRate < 0.8)], aes(x = DiscountRate, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 0.1,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09平均折讓率(0.8以下)分配直方圖" , x = "2016/10-2017/09平均折讓率", y="人數")

#2016/10-2017/09平均折讓率
library(ggplot2)
ggplot(U.CCAT_5[which(U.CCAT_5$DiscountRate < 1)], aes(DiscountRate))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 0.1) + 
  scale_x_continuous(breaks=seq(0,1,0.1))+
  labs(title = "2016/10-2017/09平均折讓率(< 1)分配直方圖" , x = "2016/10-2017/09平均折讓率", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$DiscountRate < 1)], aes(x = DiscountRate, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 0.1,position = "identity",alpha = 0.3,na.rm =TRUE) + 
  scale_x_continuous(breaks=seq(0,1,0.1))+
  labs(title = "2016/10-2017/09平均折讓率(< 1)分配直方圖" , x = "2016/10-2017/09平均折讓率", y="人數")




summary(U.CCAT_7$DiscountRate)
quantile(U.CCAT_7$DiscountRate,na.rm = T)
U.CCAT_7[which(U.CCAT_7$DiscountRate <= 0.1)]
U.CCAT_7 <- mutate(U.CCAT_7 , DiscountRate =  1-round(SumDISCOUNT / SumFEE ,2))

#2015/10-2016/09平均折讓率
library(ggplot2)
ggplot(U.CCAT_7[which(U.CCAT_7$DiscountRate < 1)], aes(DiscountRate))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 0.1) + 
  scale_x_continuous(breaks=seq(0,1,0.1))+
  labs(title = "2015/10-2016/09平均折讓率(< 1)分配直方圖" , x = "2015/10-2016/09平均折讓率", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$DiscountRate < 1)], aes(x = DiscountRate, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 0.1,position = "identity",alpha = 0.3,na.rm =TRUE) + 
  scale_x_continuous(breaks=seq(0,1,0.1))+
  labs(title = "2015/10-2016/09平均折讓率(< 1)分配直方圖" , x = "2015/10-2016/09平均折讓率", y="人數")

#########################################################################################################
#淨收入
summary(U.CCAT_5$SumNET)
quantile(U.CCAT_5$SumNET,na.rm = T)

#2016/10-2017/09累計淨收入(1000以下)
library(ggplot2)
ggplot(U.CCAT_5[which(U.CCAT_5$SumNET < 1000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 100)+
  labs(title = "2016/10-2017/09累計淨收入(1000以下)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumNET < 1000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09累計淨收入(1000以下)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

#2016/10-2017/09累計淨收入(1000~10000)
ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 1000 & U.CCAT_5$SumNET < 10000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 1000)+
  labs(title = "2016/10-2017/09累計淨收入(1000~10000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 1000 & U.CCAT_5$SumNET < 10000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09累計淨收入(1000~10000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

#2016/10-2017/09累計淨收入(10000~100000)
ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 10000 & U.CCAT_5$SumNET < 100000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 10000)+
  labs(title = "2016/10-2017/09累計淨收入(10000~100000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 10000 & U.CCAT_5$SumNET < 100000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 10000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09累計淨收入(10000~100000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

#2016/10-2017/09累計淨收入(100000~1000000)
ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 100000 & U.CCAT_5$SumNET < 1000000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 100000)+
  labs(title = "2016/10-2017/09累計淨收入(100000~1000000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 100000 & U.CCAT_5$SumNET < 1000000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09累計淨收入(100000~1000000)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

#2016/10-2017/09累計淨收入(1000000以上)
ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 1000000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000)+
  labs(title = "2016/10-2017/09累計淨收入(1000000以上)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumNET >= 1000000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09累計淨收入(1000000以上)分配直方圖" , x = "2016/10-2017/09累計淨收入", y="人數")


#####################################################################################

summary(U.CCAT_7$SumNET)
quantile(U.CCAT_7$SumNET,na.rm = T)
#2015/10-2016/09累計淨收入(1000以下)
library(ggplot2)
ggplot(U.CCAT_7[which(U.CCAT_7$SumNET < 1000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 100)+
  labs(title = "2015/10-2016/09累計淨收入(1000以下)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumNET < 1000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09累計淨收入(1000以下)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

#2015/10-2016/09累計淨收入(1000~10000)
ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 1000 & U.CCAT_7$SumNET < 10000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 1000)+
  labs(title = "2015/10-2016/09累計淨收入(1000~10000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 1000 & U.CCAT_7$SumNET < 10000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09累計淨收入(1000~10000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

#2015/10-2016/09累計淨收入(10000~100000)
ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 10000 & U.CCAT_7$SumNET < 100000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 10000)+
  labs(title = "2015/10-2016/09累計淨收入(10000~100000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 10000 & U.CCAT_7$SumNET < 100000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 10000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09累計淨收入(10000~100000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

#2015/10-2016/09累計淨收入(100000~1000000)
ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 100000 & U.CCAT_7$SumNET < 1000000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 100000)+
  labs(title = "2015/10-2016/09累計淨收入(100000~1000000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 100000 & U.CCAT_7$SumNET < 1000000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09累計淨收入(100000~1000000)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

#2015/10-2016/09累計淨收入(1000000以上)
ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 1000000)], aes(SumNET))+
  geom_histogram(aes(y = ..count..), binwidth = 100000)+
  labs(title = "2015/10-2016/09累計淨收入(1000000以上)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumNET >= 1000000)], aes(x = SumNET, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09累計淨收入(1000000以上)分配直方圖" , x = "2015/10-2016/09累計淨收入", y="人數")


#########################################################################################################
#AUM
summary(U.CCAT_5$SumAUM)
quantile(U.CCAT_5$SumAUM,na.rm = T)
#2016/10-2017/09動戶在2017/09/30之台股AUM(100W以下)
library(ggplot2)
ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM < 1000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 100000)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(100W以下)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM < 1000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(100W以下)分配直方圖" , x = "台股AUM", y="人數")
#2016/10-2017/09動戶在2017/09/30之台股AUM(100W~1000W)
ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 1000000 & U.CCAT_5$SumAUM < 10000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(100~1000W)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 1000000 & U.CCAT_5$SumAUM < 10000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(100~1000W)分配直方圖" , x = "台股AUM", y="人數")
#2016/10-2017/09動戶在2017/09/30之台股AUM(1000W~1E)
ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 10000000 & U.CCAT_5$SumAUM < 100000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 10000000)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(1000W~1E)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 10000000 & U.CCAT_5$SumAUM < 100000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 10000000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(1000W~1E)分配直方圖" , x = "台股AUM", y="人數")
#2016/10-2017/09動戶在2017/09/30之台股AUM(1~10E)
ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 100000000 & U.CCAT_5$SumAUM < 1000000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(1~10E)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 100000000 & U.CCAT_5$SumAUM < 1000000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(1~10E)分配直方圖" , x = "台股AUM", y="人數")

#2016/10-2017/09動戶在2017/09/30之台股AUM(10E以上)
ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 1000000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000000)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(10E以上)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_5[which(U.CCAT_5$SumAUM >= 1000000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000000,position = "identity",alpha = 0.3)+
  labs(title = "2016/10-2017/09實動客戶台股AUM(10E以上)分配直方圖" , x = "台股AUM", y="人數")
#####################################################################################################
summary(U.CCAT_7$SumAUM)
quantile(U.CCAT_7$SumAUM,na.rm = T)


#2015/10-2016/09動戶在2017/09/30之台股AUM(100W以下)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM < 1000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 100000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(100W以下)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM < 1000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(100W以下)分配直方圖" , x = "台股AUM", y="人數")
#2015/10-2016/09動戶在2017/09/30之台股AUM(100W~1000W)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 1000000 & U.CCAT_7$SumAUM < 10000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(100~1000W)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 1000000 & U.CCAT_7$SumAUM < 10000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(100~1000W)分配直方圖" , x = "台股AUM", y="人數")
#2015/10-2016/09動戶在2017/09/30之台股AUM(1000W~1E)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 10000000 & U.CCAT_7$SumAUM < 100000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 10000000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1000W~1E)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 10000000 & U.CCAT_7$SumAUM < 100000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 10000000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1000W~1E)分配直方圖" , x = "台股AUM", y="人數")
#2015/10-2016/09動戶在2017/09/30之台股AUM(1E以上)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 100000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1E以上)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 100000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1E以上)分配直方圖" , x = "台股AUM", y="人數")


#2015/10-2016/09動戶在2017/09/30之台股AUM(1~10E)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 100000000 & U.CCAT_7$SumAUM < 1000000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1~10E)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 100000000 & U.CCAT_7$SumAUM < 1000000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 100000000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(1~10E)分配直方圖" , x = "台股AUM", y="人數")

#2015/10-2016/09動戶在2017/09/30之台股AUM(10E以上)
ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 1000000000)], aes(SumAUM))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000000)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(10E以上)分配直方圖" , x = "台股AUM", y="人數")

ggplot(U.CCAT_7[which(U.CCAT_7$SumAUM >= 1000000000)], aes(x = SumAUM, fill = Gender))+
  geom_histogram(aes(y = ..count..), binwidth = 1000000000,position = "identity",alpha = 0.3)+
  labs(title = "2015/10-2016/09實動客戶台股AUM(10E以上)分配直方圖" , x = "台股AUM", y="人數")


#####################################################################################################

#交易量
options(scipen=999)
summary(CusContrib_5$SumAMT)
range(CusContrib_5$SumAMT,na.rm = T)
quantile(CusContrib_5$SumAMT,na.rm = T)
quantile(CusContrib_5$SumAMT,0.6,na.rm = T)
IQR(CusContrib_5$SumAMT,na.rm = T)
sd(CusContrib_5$SumAMT,na.rm = T)
var(CusContrib_5$SumAMT,na.rm = T)
install.packages("moments")
library(moments)
skewness(CusContrib_5$SumAMT,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(CusContrib_5$SumAMT,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)

ggplot(CusContrib_5, aes(SumAMT))+
  geom_histogram(aes(y = ..count..))+
  xlim(10000000000,130200000000)+
  ylim(0,5)+
  labs(title = "客戶近一年總交易量分配直方圖" , x = "近一年總交易量")
CusContrib_5[which(CusContrib_5$SumAMT >= 100000000000)]

#手續費
options(scipen=999)
summary(CusContrib_5$SumFEE)
quantile(CusContrib_5$SumFEE,na.rm = T)

ggplot(CusContrib_5, aes(SumFEE))+
  geom_histogram(aes(y = ..count..))+
  xlim(5000,1000000)+
  # ylim(0,20000)+
  labs(title = "客戶手續費分配直方圖" , x = "手續費")
CusContrib_5[which(CusContrib_5$SumFEE >= 100000000)]

#手續費折讓
options(scipen=999)
summary(CusContrib_5$SumDISCOUNT)
quantile(CusContrib_5$SumDISCOUNT,0.99,na.rm = T)

ggplot(CusContrib_5, aes(SumDISCOUNT))+
  geom_histogram(aes(y = ..count..))+
  xlim(0,1000000)+
  # ylim(0,20000)+
  labs(title = "客戶手續費折讓分配直方圖" , x = "手續費折讓")
CusContrib_5[which(CusContrib_5$SumFEE >= 10000000)]


# a <- sqldf("select CUSTNO , round(SumDISCOUNT / SumFEE *100 ,2) as DiscountRate 
#             from CusContrib_5 ", method = "name__class")
# a <- mutate(CusContrib_5 , DiscountRate =  round(SumDISCOUNT / SumFEE *100 ,2))
#折讓比
#應扣費用

#淨收入(REV-折讓-上手)
options(scipen=999)
summary(CusContrib_5$SumNET)
quantile(CusContrib_5$SumNET,0.99,na.rm = T)

ggplot(CusContrib_5, aes(SumNET))+
  geom_histogram(aes(y = ..count..))+
  xlim(100000,10000000)+
  # ylim(0,5)+
  labs(title = "客戶淨收入分配直方圖" , x = "淨收入")
nrow(CusContrib_5[which(CusContrib_5$SumFEE >= 3293)])

CusContrib_5[which(CusContrib_5$CUS050 == "H123800006 ")]


##########################################################################
# #交叉比較
# ggplot(CusContrib_5, aes(x = SumFEE, y = SumNET )) + 
#  geom_point(aes(color = Gender))+
#   xlim(0,5000)+
#   ylim(0,5000)+
#   geom_abline()
# 
# ggplot(CusContrib_5, aes(x=SumFEE, y=SumNET )) + 
#   geom_point(aes(color = Gender))+
#   geom_line(aes(x=SumFEE,
#                 y=SumNET,
#                 color=Gender)
#   ) +
#   xlim(0,5000)+
#   ylim(0,5000)+
#   geom_abline()
# 
# #交叉比較
# ggplot(CusContrib_5, aes(x = SumFEE, y = SumDISCOUNT )) + 
#   geom_point(aes(color = Gender))+
#   xlim(0,5000)+
#   ylim(0,5000)+
# geom_abline()
# 
# #箱型圖
# ggplot(CusContrib_5, aes(x = Gender, y = AMT_)) + 
#   geom_boxplot() +
#   
#     labs(title = "客戶性別x交易量箱型圖" )
#             
# #客戶交易量的直方圖，依性別疊合檢視
# qplot(Age, data = U.CUS_4, geom = "histogram", binwidth = 10,
#   fill = Gender,main ="客戶年齡直方圖")
#             
# #客戶年齡的直方圖，依性別分開檢視
# install.packages("lattice")
# require(lattice)
# histogram(x= ~ Age | Gender,  # 根據性別的條件，繪製客戶年齡的直方圖
#          data= U.CUS_4,     
#          xlab="U.CUS_4客戶年齡分配",  
#          layout=c(2,1))       # 以2x1的方式呈現圖表
            
        