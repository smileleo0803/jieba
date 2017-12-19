library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  


###歷史客戶主檔 inner join 歷史分公司 → 所有帳號   #2672208
CUS <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,
                CUS120,CUS150,CUS250,CUS320,CUS350,CUS370,CUS380,CUS390,CUS570,CUS440,NBHNO_,NDEPT_
                from KGICS0MH_20171001 as k inner join BRHDTAH as b ON k.CUS010 = b.BHNO_ and k.CUS150 = b.DEPT_ ")
library("data.table")
CUS <- as.data.table(CUS)

close(myconn) 

class(CUS)
head(CUS,5)
nrow(CUS)
str(CUS)
summary(CUS)


######################################################################################################################
library(stringr)
#CUS370轉字串
CUS$CUS370 <- as.character(CUS$CUS370)
#CUS380應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
CUS$CUS380 <- str_pad(CUS$CUS380,6,"left",'0')
#CUS390轉字串
CUS$CUS390 <- as.character(CUS$CUS390)
#將CUS370、CUS380、CUS390轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
CUS$CUSTNO <- paste0(CUS$CUS370,CUS$CUS380,CUS$CUS390)
str(CUS)
#姓名轉字串
CUS$CUS040 <- as.character(CUS$CUS040)
#ID由Factor轉字串
CUS$CUS050 <- as.character(CUS$CUS050) 
#CUS570應為3碼字串，但轉入為int型態，補0為3碼並轉成字串
CUS$CUS570 <- str_pad(CUS$CUS570,3,"left",'0')
#NBHNO_轉字串
CUS$NBHNO_ <- as.character(CUS$NBHNO_)
CUS$NBHNO_ <- str_trim(CUS$NBHNO_,side='both')
# #NDEPT_轉字串
# CUS$NDEPT_ <- as.character(CUS$NDEPT_)
# CUS$NDEPT_ <- str_trim(CUS$NDEPT_,side='both')
str(CUS)

#########################################################################################################
###歷史客戶、分公司主檔再串現有客戶主檔
#串現有客戶主檔
#NEWCUS 共2652264
NEWCUS <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,
                  CUS120,CUS150,CUS250,CUS320,CUS350,CUS370,CUS380,CUS390,CUS570,CUS440
                   from KGICS0M_20171001 ") 
NEWCUS <- as.data.table(NEWCUS)
#CUS010轉字串
NEWCUS$CUS010 <- as.character(NEWCUS$CUS010)
#CUS020應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
NEWCUS$CUS020 <- str_pad(NEWCUS$CUS020,6,"left",'0')
#CUS030轉字串
NEWCUS$CUS030 <- as.character(NEWCUS$CUS030)
#將CUS010、CUS020、CUS030轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
NEWCUS$CUSTNO <- paste0(NEWCUS$CUS010,NEWCUS$CUS020,NEWCUS$CUS030)
# #CUS150轉字串
# NEWCUS$CUS150 <- as.character(NEWCUS$CUS150)
# NEWCUS$CUS150 <- str_trim(NEWCUS$CUS150,side='both')
str(NEWCUS)

#串資料要用完整帳號(CUSTNO)及GROUP BY完整帳號
library(sqldf)
CUS <- sqldf("select n.CUSTNO,n.CUS040,n.CUS050,n.CUS070,n.CUS080,n.CUS090,n.CUS120,
              n.CUS150,n.CUS250,n.CUS320,n.CUS350,n.CUS370,n.CUS380,n.CUS390,n.CUS570,n.CUS440,NBHNO_,
              NDEPT_
              from CUS c inner join NEWCUS n on c.CUSTNO = n.CUSTNO
              group by n.CUSTNO" , method = "name__class")
CUS <- as.data.table(CUS)
nrow(CUS)#2087885  
nrow(CUS[which(CUS$CUS090 == 0 & CUS570=='000' & CUS080 > 20160930)])


#CUS370轉字串
CUS$CUS370 <- as.character(CUS$CUS370)
#CUS380應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
CUS$CUS380 <- str_pad(CUS$CUS380,6,"left",'0')
#CUS390轉字串
CUS$CUS390 <- as.character(CUS$CUS390)


#############################################################################################################

#新增性別變數(擷取ID第2碼)
#注意：資料包含法人、外國人，無法判斷性別，造成第二碼有非0或1的值
CUS$Gender <- substring(CUS$CUS050,2,2) 
CUS$Gender <- as.factor(CUS$Gender)
# levels(CUS$Gender)
# levels(CUS$Gender) = 

#新增年齡變數
#出生日期由int轉字串
CUS$CUS350 <- as.character(CUS$CUS350)
#把出生時間為0的改為NA
CUS$CUS350[CUS$CUS350 =="0" ] <- "NA"


#新增年齡變數(現在日期-出生日期)
#CUS350為年月日格式，轉成Date(無時分秒問題)
CUS$Age <- as.Date(CUS$CUS350, format = "%Y%m%d")
#當前日期與生日相減→天數，除365轉年→floor為無條件捨去
CUS$Age <- floor(as.Date("20170930", "%Y%m%d") -CUS$Age)
CUS$Age <- as.numeric(CUS$Age)
CUS$Age <- floor(CUS$Age/365)

#型態由difftime改為numeric
CUS$CUS350 <- as.numeric(CUS$CUS350)

#CUS071開戶至今天數
CUS$CUS070 <- as.character(CUS$CUS070)
CUS$CUS071 <- as.Date(CUS$CUS070, format = "%Y%m%d")
CUS$CUS071 <- floor(as.Date("20170930", "%Y%m%d") -CUS$CUS071)
CUS$CUS071 <- as.numeric(CUS$CUS071)
CUS$CUS070 <- as.numeric(CUS$CUS070)

#CUS081最後往來日至今天數
CUS$CUS080 <- as.character(CUS$CUS080)
CUS$CUS081 <- as.Date(CUS$CUS080, format = "%Y%m%d")
CUS$CUS081 <- floor(as.Date("20170930", "%Y%m%d") -CUS$CUS081)
CUS$CUS081 <- as.numeric(CUS$CUS081)
CUS$CUS080 <- as.numeric(CUS$CUS080)


#有無email(Y/N)
CUS441 <- ifelse(CUS$CUS440 == '', 'N' , 'Y')
CUS441 <- as.factor(CUS441)
CUS <- cbind(CUS,CUS441)
rm(CUS441)

#電子管道(Y/N)
CUS251 <- ifelse(CUS$CUS250 == 0 , 'N' , 'Y')
CUS251 <- as.factor(CUS251)
CUS <- cbind(CUS,CUS251)
rm(CUS251)


#增加一欄rep(1,nrow(CUS))讓每行都計數1
#增加一欄N_account→計算每個ID持有幾個帳號
library(dplyr)
CUS <- cbind(CUS,rep(1,nrow(CUS)))
#把rep(1,nrow(CUS))欄位名改為1
colnames(CUS)[ncol(CUS)]='1'

str(CUS)

#############################################################################################################

#觀察cus的na數
sum(is.na(CUS$CUS040))
sum(is.na(CUS$CUS050))
sum(is.na(CUS$CUS080))
sum(is.na(CUS$CUS090))
sum(is.na(CUS$CUS120))
sum(is.na(CUS$CUS150))#1020361
sum(is.na(CUS$CUS250))
sum(is.na(CUS$CUS320))#309
sum(is.na(CUS$CUS350))#3950
sum(is.na(CUS$CUS370))
sum(is.na(CUS$CUS380))
sum(is.na(CUS$CUS390))#29
sum(is.na(CUS$CUS570))
sum(is.na(CUS$CUS440))
sum(is.na(CUS$NBHNO_))
sum(is.na(CUS$NDEPT_))#994895
sum(is.na(CUS$CUSTNO))
sum(is.na(CUS$Gender))
sum(is.na(CUS$Age))#4097
sum(is.na(CUS$CUS081))#406022

# CUS.shape[0] - CUS.count()
#########################################################################################################

#CUS_1扣除已關戶 #1603379
library(sqldf)
CUS_1 <- sqldf("select * from CUS where CUS090 == 0 ")
CUS_1 <- as.data.table(CUS_1)
nrow(CUS_1)
nrow(CUS[which(CUS$CUS090 == 0)])

#CUS_2扣除已關戶且為自然人 #1579911
CUS_2 <- sqldf("select * from CUS_1 where CUS320 =='1' ")
CUS_2 <- as.data.table(CUS_2)
nrow(CUS_2)
nrow(CUS[which(CUS$CUS090 == 0 & CUS$CUS320 =='1')])

#CUS_3 →CUS扣除已關戶且為本國自然人 #1576749
CUS_3 <- sqldf("select * from CUS where CUS090 = 0 and CUS570=='000'" , method = "name__class")
CUS_3 <- as.data.table(CUS_3)
nrow(CUS_3)
nrow(CUS[which(CUS$CUS090 == 0 & CUS$CUS570 =='000')])

#CUS_4 →CUS_3且只取Gender為1或2 #1576748
CUS_4 <- sqldf("select * from CUS_3 where Gender=='1' or Gender =='2'" , method = "name__class")
CUS_4 <- as.data.table(CUS_4)
nrow(CUS_4)
CUS_3[which(CUS_3$Gender != '1' & CUS_3$Gender != '2')]

#CUS_5(實動戶) →CUS_4且CUS080 > 20160930(扣除已關戶且近一年實動之本國自然人) #322932
CUS_5 <- sqldf("select * from CUS_4 where CUS080 > 20160930 " )
CUS_5 <- as.data.table(CUS_5)
nrow(CUS_5)
nrow(CUS_4[which(CUS_4$CUS080 > 20140930)])

#CUS_6(實動戶) →CUS_4且CUS080 > 20150930(扣除已關戶且近兩年實動之本國自然人) #387439
CUS_6 <- sqldf("select * from CUS_4 where CUS080 > 20150930 " )
CUS_6 <- as.data.table(CUS_6)
nrow(CUS_6)

#CUS_7 →  CUS_5與CUS_6的GAP #64507
CUS_7 <- sqldf("select * from CUS_4 where CUS080 > 20150930 and CUS080 < 20161001" )
CUS_7 <- as.data.table(CUS_7)
nrow(CUS_7)

##########################################################################################################

#看distinct 客戶ID
library(sqldf)
U.CUS <- sqldf("select * from CUS group by CUS050", method = "name__class")
U.CUS <- as.data.table(U.CUS)
nrow(U.CUS)#不重覆客戶共1761623

U.CUS_1 <- sqldf("select * from CUS_1 group by CUS050 ", method = "name__class")
U.CUS_1 <- as.data.table(U.CUS_1)
nrow(U.CUS_1)#不重覆客戶共1467374

U.CUS_2 <- sqldf("select * from CUS_2 group by CUS050 ", method = "name__class")
U.CUS_2 <- as.data.table(U.CUS_2)
nrow(U.CUS_2)#不重覆客戶共1454089

U.CUS_3 <- sqldf("select * from CUS_3 group by CUS050 ", method = "name__class")
U.CUS_3 <- as.data.table(U.CUS_3)
nrow(U.CUS_3)#不重覆客戶共1451126

U.CUS_4 <- sqldf("select * from CUS_4 group by CUS050 ", method = "name__class")
U.CUS_4 <- as.data.table(U.CUS_4)
nrow(U.CUS_4)#不重覆客戶共1451125

U.CUS_5 <- sqldf("select * from CUS_5 group by CUS050 ", method = "name__class")
U.CUS_5 <- as.data.table(U.CUS_5)
nrow(U.CUS_5)#不重覆客戶共318527

U.CUS_6 <- sqldf("select * from CUS_6 group by CUS050 ", method = "name__class")
U.CUS_6 <- as.data.table(U.CUS_6)
nrow(U.CUS_6)#不重覆客戶共381135

U.CUS_7 <- sqldf("select * from CUS_7 group by CUS050 ", method = "name__class")
U.CUS_7 <- as.data.table(U.CUS_7)
nrow(U.CUS_7)#不重覆客戶共63729


#################################################################################################

library(ggplot2)
#客戶類別比重
qplot(CUS320, data = U.CUS_1, geom = "histogram")
table(U.CUS_1$CUS320)
round(table(U.CUS_1$CUS320) / length(U.CUS_1$CUS320) *100,2)

#電子戶比重
table(CUS_4$CUS251)
round(table(CUS_4$CUS251) / length(CUS_4$CUS251) *100,2)
round(table(CUS_5$CUS251) / length(CUS_5$CUS251) *100,2)

#自來戶比重
table(CUS_4$CUS120)
round(table(CUS_4$CUS120) / length(CUS_4$CUS120) *100,2)
round(table(CUS_5$CUS120) / length(CUS_5$CUS120) *100,2)

#Email比重
table(CUS_4$CUS441)
round(table(CUS_4$CUS441) / length(CUS_4$CUS441) *100,2)
round(table(CUS_5$CUS441) / length(CUS_5$CUS441) *100,2)

#分公司比重(地區)
sort(table(CUS_4$CUS370))
sort(table(CUS_5$CUS370))


#最後交易日至今天數(近一年實動)
options(scipen=999)
qplot(CUS081, data = CUS_5, geom = "histogram" ,binwidth = 20,
      xlab = "近一年實動客戶最後交易日至今天數" )

#性別比重
pct = round(table(U.CUS_4$Gender) / length(U.CUS_4$Gender) *100,1)
labels = paste(names(pct),pct,"%")
pie(table(U.CUS_4$Gender), labels = labels)

table(U.CUS_4$Gender)
round(table(U.CUS_4$Gender) / length(U.CUS_4$Gender) *100,2)
table(U.CUS_5$Gender)
round(table(U.CUS_5$Gender) / length(U.CUS_5$Gender) *100,2)

#男女平均年齡
m <- subset(U.CUS_4, Gender == '1')
mean(m$Age ,na.rm = T)

f <- subset(U.CUS_4, Gender == '2')
mean(f$Age ,na.rm = T)

m <- subset(U.CUS_5, Gender == '1')
mean(m$Age ,na.rm = T)

f <- subset(U.CUS_5, Gender == '2')
mean(f$Age ,na.rm = T)


#檢視年齡分配(使用U.CUS_4，因U.CUS_3性別有問題的確認有誤)
options(scipen=999)
summary(U.CUS_4$Age)
range(U.CUS_4$Age,na.rm = T)
quantile(U.CUS_4$Age,na.rm = T)
IQR(U.CUS_4$Age,na.rm = T)
sd(U.CUS_4$Age,na.rm = T)
var(U.CUS_4$Age,na.rm = T)
install.packages("moments")
library(moments)
skewness(U.CUS_4$Age,na.rm = T) #偏度(偏態值 > 0，為正偏態，分配集中在平均數以下，低分群的個體較多)
kurtosis(U.CUS_4$Age,na.rm = T) #峰度(峰態係數K>0稱為高峻峰，峰態係數K=0稱為常態峰，峰態係數K<0稱為低闊峰)


ggplot(U.CUS_4, aes(Age))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 10) + 
  scale_x_continuous(breaks=seq(0,120,10))+
  labs(title = "客戶年齡分配直方圖" , x = "年齡" , y = "人數") 


#性別X年齡分配
qplot(x=Gender,                               
      y=Age,
      data=U.CUS_4,                     
      geom="boxplot",       
      xlab="性別",                          
      color= Gender,
      main= "客戶性別x年齡盒鬚圖")+
  scale_y_continuous(breaks=seq(0,120,10))

boxplot(formula = U.CUS_4$Age~ U.CUS_4$Gender,data=U.CUS_4,xlab="性別",ylab="年齡",col="#f8f3c4")
title("客戶年齡與性別箱型圖")

ggplot(U.CUS_4, aes(x = Gender, y = Age)) + 
  geom_boxplot() +
  labs(title = "客戶性別x年齡箱型圖" )

#客戶年齡的直方圖，依性別疊合檢視
ggplot(U.CUS_4, aes(x = Age, fill = Gender)) +
  # 直方圖函數：position設置堆積模式為重疊
  geom_histogram(position = "identity", alpha = 0.3,binwidth = 10)+scale_x_continuous(breaks=seq(0,120,10))+
    labs(title = "客戶性別年齡分配圖" )


#客戶年齡的直方圖，依性別分開檢視
install.packages("lattice")
require(lattice)
histogram(x= ~ Age | Gender,  # 根據性別的條件，繪製客戶年齡的直方圖
          data= U.CUS_4,     
          xlab="客戶年齡分配",  
          layout=c(1,2))       # 以2x1的方式呈現圖表


##近一年實動
#近一年實動年齡分配
summary(U.CUS_5$Age)
quantile(U.CUS_5$Age,na.rm = T)

ggplot(U.CUS_5, aes(Age))+
  geom_histogram(aes(y = ..count..),na.rm =TRUE,binwidth = 10) + 
  scale_x_continuous(breaks=seq(0,110,10))+
  labs(title = "近一年實動客戶年齡分配直方圖" , x = "年齡" , y = "人數") 

#近一年實動年齡盒鬚圖
qplot(x=Gender,                               
      y=Age,
      data=U.CUS_5,                     
      geom="boxplot",       
      xlab="性別",                          
      color= Gender,
      main= "近一年實動客戶性別x年齡盒鬚圖")+
  scale_y_continuous(breaks=seq(0,110,10))

boxplot(formula = U.CUS_5$Age~ U.CUS_5$Gender,data=U.CUS_5,xlab="性別",ylab="年齡",col="#f8f3c4")
title("近一年實動客戶年齡與性別箱型圖")

ggplot(U.CUS_5, aes(x = Gender, y = Age)) +
  geom_boxplot(aes(xlab="性別",ylab="年齡",fill= Gender)) +
  scale_y_continuous(breaks=seq(0,110,10))+
  labs(title = "近一年實動客戶性別x年齡箱型圖" )

#客戶年齡的直方圖，依性別疊合檢視
ggplot(U.CUS_5, aes(x = Age, fill = Gender)) +
  # 直方圖函數：position設置堆積模式為重疊
  geom_histogram(position = "identity", alpha = 0.3,binwidth = 10)+scale_x_continuous(breaks=seq(0,110,10))+
  labs(title = "近一年實動客戶性別年齡分配圖" )

#客戶年齡的直方圖，依性別分開檢視
install.packages("lattice")
require(lattice)
histogram(x= ~ Age | Gender,  # 根據性別的條件，繪製客戶年齡的直方圖
          data= U.CUS_5,     
          xlab="近一年實動客戶年齡分配",  
          layout=c(1,2))       # 以2x1的方式呈現圖表
#################################################################################


write.table(CUS_1, file="CUS_1.csv",sep=",",row.names=F)
write.table(CUS_3, file="CUS_3.csv",sep=",",row.names=F)
write.table(CUS_4, file="CUS_4.csv",sep=",",row.names=F)
write.table(CUS_5, file="CUS_5.csv",sep=",",row.names=F)

write.table(U.CUS_4, file="U.CUS_4.csv",sep=",",row.names=F)
write.table(U.CUS_5, file="U.CUS_5.csv",sep=",",row.names=F)


write.table(U.CCAT_5, file="U.CCAT_5.csv",sep=",",row.names=F)
write.table(U.CCAT_7, file="U.CCAT_7.csv",sep=",",row.names=F)