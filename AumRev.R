library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")
#台股客戶20170930的AUM
aum <- sqlQuery(myconn, "select IDNO_ , CUSTNO_ ,BTYPE_ ,TYPE1_ , TYPE2_ , VALUE_ 
                from KGIAUM0M 
                where TDATE_ = 20170930 ")
close(myconn)

head(aum)
summary(aum$VALUE_)
str(aum)
nrow(aum)#854897
aum <- as.data.table(aum)
class(aum)


#aum資料型態轉換
aum$IDNO_  <- as.character(aum$IDNO_)
aum$CUSTNO_  <- as.character(aum$CUSTNO_)
#觀察aum的na數
sum(is.na(aum$IDNO_))
sum(is.na(aum$CUSTNO_))
sum(is.na(aum$BTYPE_))
sum(is.na(aum$TYPE1_))
sum(is.na(aum$TYPE2_))
sum(is.na(aum$VALUE_))

##################################################################################################
#台股客戶20170930的AUM(扣除期貨庫存市值)
library(sqldf)
aum1 <- sqldf("select IDNO_ , CUSTNO_ , sum(VALUE_) as SumAUM from aum
              where TYPE2_ == '全商品' and BTYPE_ <> '期貨'
              group by CUSTNO_
              order by SumAUM desc")
class(aum1)
nrow(aum1) #707128
aum1 <- as.data.table(aum1)

library(dplyr)
filter1 <-filter(aum1,SumAUM > 0)
nrow(filter1)
#nrow(aum1)=707128
#>0 共706931
# =0 共197
# <0 共0

#aum1(挑選出自然人)
q <- select(aum1,IDNO_) %>%
  filter(nchar(IDNO_) == 8 )
head(q)
nrow(q)

###################################################################################################
#近一年實動自然人(CUS_5)+近一年總貢獻度+20170930台股AUM By帳號
CCA_5 <- sqldf("select * from CusContrib_5 c left join aum1 a on c.CUSTNO = a.CUSTNO_ ", method = "name__class")
nrow(CCA_5)
CCA_5 <- as.data.table(CCA_5)
head(CCA_5)


