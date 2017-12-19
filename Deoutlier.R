#總交易金額
quantile(U.CCAT_5$SumAMT,na.rm = T)
IQR(U.CCAT_5$SumAMT,na.rm = T)#3316955
#IQR * 1.5 = 4975433
156545- 4975433
3473500+ 4975433
#(-4818888,8448933)
DU.CCAT_5 <- subset(U.CCAT_5, SumAMT >= -4818888 & SumAMT < 8448933)
ggplot(DU.CCAT_5, aes(SumAMT))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 100000) + 
  scale_x_continuous(breaks=seq(0,8449000,1000000))+
  labs(title = "客戶2016/10-2017/09總交易量(0~8,448,933)分配直方圖" , x = "2016/10-2017/09總交易量",y="人數")




quantile(U.CCAT_7$SumAMT,na.rm = T)
IQR(U.CCAT_7$SumAMT,na.rm = T)#573997.2
#IQR * 1.5 = 860995.8
58800- 860995.8 =-802195.8
632797.2 + 860995.8 = 1493793
#(0,1493793)
DU.CCAT_7 <- subset(U.CCAT_7, SumAMT >= -802195.8 & SumAMT < 1493793)
ggplot(DU.CCAT_7, aes(SumAMT))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 100000) + 
  scale_x_continuous(breaks=seq(0,1500000,200000))+
  labs(title = "客戶2015/10-2016/09總交易量(0~1,493,793)分配直方圖" , x = "2015/10-2016/09總交易量",y="人數")


#####################################################################################################
#總交易次數
quantile(U.CCAT_5$TranCount,na.rm = T)
IQR(U.CCAT_5$TranCount,na.rm = T)#31
#IQR * 1.5 = 46.5
3- 46.5 =-43.5
34+ 46.5 =80.5
#(0,80.5)
DU.CCAT_5 <- subset(U.CCAT_5,TranCount < 81)
ggplot(DU.CCAT_5, aes(TranCount))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 5) + 
  scale_x_continuous(breaks=seq(0,85,5))+
  labs(title = "客戶2016/10-2017/09總交易次數(0~81)分配直方圖" , x = "2016/10-2017/09總交易次數",y="人數")



quantile(U.CCAT_7$TranCount,na.rm = T)
IQR(U.CCAT_7$TranCount,na.rm = T)#5
#IQR * 1.5 = 7.5
1- 7.5 =-6.5
6+ 7.5 =13.5
#(0,14)
DU.CCAT_7 <- subset(U.CCAT_7,TranCount < 14)
ggplot(DU.CCAT_7, aes(TranCount))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 1) + 
  scale_x_continuous(breaks=seq(0,14,1))+
  labs(title = "客戶2015/10-2016/09總交易次數(0~14)分配直方圖" , x = "2015/10-2016/09總交易次數",y="人數")
#####################################################################################################
#手續費
quantile(U.CCAT_5$SumFEE,na.rm = T)
IQR(U.CCAT_5$SumFEE,na.rm = T)#4115
#IQR * 1.5 = 6172.5
210- 6172.5 
4325+ 6172.5 #10497.5
#(0,10498)
DU.CCAT_5 <- subset(U.CCAT_5,SumFEE < 10498)
ggplot(DU.CCAT_5, aes(SumFEE))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 1000) + 
  scale_x_continuous(breaks=seq(0,10500,1000))+
  labs(title = "客戶2016/10-2017/09總手續費(0~10,498)分配直方圖" , x = "2016/10-2017/09總手續費",y="人數")


#####################################################################################################
#淨收入
quantile(U.CCAT_5$SumNET,na.rm = T)
IQR(U.CCAT_5$SumNET,na.rm = T)#3138
#IQR * 1.5 = 4707
170- 4707
3308+ 4707 #8015
#(0,8015)
DU.CCAT_5 <- subset(U.CCAT_5,SumNET <= 8015)
ggplot(DU.CCAT_5, aes(SumNET))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 500) + 
  scale_x_continuous(breaks=seq(0,8050,500))+
  labs(title = "客戶2016/10-2017/09總計淨收入(0~8,015)分配直方圖" , x = "2016/10-2017/09總計淨收入",y="人數")


quantile(U.CCAT_7$SumNET,na.rm = T)
IQR(U.CCAT_7$SumNET,na.rm = T)#599
#IQR * 1.5 = 898.5
69- 898.5
668+ 898.5 #1566.5
#(0,1567)
DU.CCAT_7 <- subset(U.CCAT_7,SumNET < 1567)
ggplot(DU.CCAT_7, aes(SumNET))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 500) + 
  scale_x_continuous(breaks=seq(0,1600,500))+
  labs(title = "客戶2015/10-2016/09總計淨收入(0~1,567)分配直方圖" , x = "2015/10-2016/09總計淨收入",y="人數")


#####################################################################################################
#AUM
quantile(U.CCAT_5$SumAUM,na.rm = T)
IQR(U.CCAT_5$SumAUM,na.rm = T)#1248623
#IQR * 1.5 = 1872935
135877- 1872935
1384500+ 1872935 #3257435
#(0,3257435)
DU.CCAT_5 <- subset(U.CCAT_5,SumAUM <= 3257435)
ggplot(DU.CCAT_5, aes(SumAUM))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 250000) + 
  scale_x_continuous(breaks=seq(0,3257500,500000))+
  labs(title = "2016/10-2017/09實動客戶AUM(0~3,257,435)分配直方圖" , x = "AUM",y="人數")



quantile(U.CCAT_7$SumAUM,na.rm = T)
IQR(U.CCAT_7$SumAUM,na.rm = T)#599488
#IQR * 1.5 = 899232
25965.5 - 899232
625453.5+ 899232 #1524686
#(0,1524686)
DU.CCAT_7 <- subset(U.CCAT_7,SumAUM <= 1524686)
ggplot(DU.CCAT_7, aes(SumAUM))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 250000) + 
  scale_x_continuous(breaks=seq(0,1524686,500000))+
  labs(title = "2015/10-2016/09實動客戶AUM(0~1,524,686)分配直方圖" , x = "AUM",y="人數")



#####################################################################################################
#開戶至今天數
quantile(U.CCAT_5$CUS071,na.rm = T)
IQR(U.CCAT_5$CUS071,na.rm = T)#4283
#IQR * 1.5 = 6424.5
2405 - 6424.5
6688 + 6424.5
#(-4019.5,13112.5)
DU.CCAT_5 <- subset(U.CCAT_5, CUS071 < 13112.5)
ggplot(DU.CCAT_5, aes(CUS071))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 365) + 
  scale_x_continuous(breaks=seq(0,13505,365))+
  labs(title = "客戶2016/10-2017/09開戶至今天數(0~13,113)分配直方圖" , x = "2016/10-2017/09開戶至今天數",y="人數")




quantile(U.CCAT_7$CUS071,na.rm = T)
IQR(U.CCAT_7$CUS071,na.rm = T)#4193
#IQR * 1.5 = 6289.5
2410 - 6289.5
6603 + 6289.5
#(-3879.5,12892.5)
DU.CCAT_7 <- subset(U.CCAT_7, CUS071 < 12892.5)
ggplot(DU.CCAT_7, aes(CUS071))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 365) + 
  scale_x_continuous(breaks=seq(0,13000,365))+
  labs(title = "客戶2015/10-2016/09開戶至今天數(0~12,893)分配直方圖" , x = "2015/10-2016/09開戶至今天數",y="人數")
