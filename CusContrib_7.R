##CusContrib_7 → 貢獻度近一年總量與CUS_7結合
library("RODBC")
Contrib7 <- sqlQuery(myconn, "select TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , 
                    AMT_ , FEE_ , DISCOUNT_ , EXPENSE_ , NET_ , IDNO_
                    from KGICONTRIB where TDATE_ > 201509 and TDATE_ < 201610  ")

class(Contrib7)
head(Contrib7,10)
nrow(Contrib7)
str(Contrib7)
summary(Contrib7)
#台股客戶2015/10/01-2016/09/30的客戶貢獻度
Contrib7 <- filter(Contrib7,MARKET_ =='1' | MARKET_ == 'R')
str(Contrib7)
library("data.table")
Contrib7 <- as.data.table(Contrib7)

#############################################################################################

#BHNO_分支代碼轉字串
Contrib7$BHNO_ <- as.character(Contrib7$BHNO_)
Contrib7$BHNO_  <- str_trim(Contrib7$BHNO_,side='both')
#CUSTNO_帳號應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
Contrib7$CUSTNO_  <- str_trim(Contrib7$CUSTNO_,side='both')
Contrib7$CUSTNO_ <- str_pad(Contrib7$CUSTNO_,6,"left",'0')
#CUSTCKNO_檢查碼轉字串
Contrib7$CUSTCKNO_ <- as.character(Contrib7$CUSTCKNO_)
Contrib7$CUSTCKNO_  <- str_trim(Contrib7$CUSTCKNO_,side='both')
#將BHNO_、CUSTNO_、CUSTCKNO_轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
Contrib7$CUSTNO <- paste0(Contrib7$BHNO_,Contrib7$CUSTNO_,Contrib7$CUSTCKNO_)
#ID由Factor轉字串
Contrib7$IDNO_ <- as.character(Contrib7$IDNO_)


#計算SUM(交易量).SUM(手續費).SUM(手續費折讓).SUM(應扣費用).SUM(淨收入)
sumcount7 <- sqldf("select CUSTNO ,sum(AMT_) as SumAMT, sum(FEE_) as SumFEE,
                   sum(DISCOUNT_) as SumDISCOUNT,sum(EXPENSE_) as SumEXPENSE,sum(NET_) as SumNET
                  from Contrib7
                  group by CUSTNO
                  order by SumNET desc ") 

# sumcount <- sqldf("select TDATE_ , MARKET_ , BHNO_ , CUSTNO_ , CUSTCKNO_ , AMT_ , FEE_ , DISCOUNT_ ,
#                    EXPENSE_ , NET_ , IDNO_ , CUSTNO ,sum(AMT_) as SumAMT, sum(FEE_) as SumFEE,
#                    sum(DISCOUNT_) as SumDISCOUNT,sum(EXPENSE_) as SumEXPENSE,sum(NET_) as SumNET
#                   from Contrib
#                   group by CUSTNO
#                   order by SumNET desc ") 
nrow(sumcount7)#302253
sumcount7 <- as.data.table(sumcount7)

############################################################################################################

##CusContrib_7 → 貢獻度近一年總量與CUS_7結合
CusContrib_7 <- sqldf("select * from CUS_7 c left join sumcount7 s on c.CUSTNO = s.CUSTNO")
CusContrib_7 <- CusContrib_7[,-c(1)]
CusContrib_7 <- as.data.table(CusContrib_7)
nrow(CusContrib_7) #64507
str(CusContrib_7)
summary(CusContrib_7)

###################################################################################################
#近一年實動自然人(CUS_7)+近一年總貢獻度+20170930台股AUM By帳號
CCA_7 <- sqldf("select * from CusContrib_7 c left join aum1 a on c.CUSTNO = a.CUSTNO_ ", method = "name__class")
nrow(CCA_7)
CCA_7 <- as.data.table(CCA_7)
head(CCA_7)

###############################################################################################################
#近一年客戶交易次數資料
KGIMXMM7 <- sqlQuery(myconn, "select TDATE_ ,TERMSEQ_ ,TTYPE_ ,BHNO_ , CUSTNO_ , CUSTCKNO_
                    from KGIMXMM
                    where TDATE_ > 20150930 and TDATE_ <= 20160930")

class(KGIMXMM7)
str(KGIMXMM7)
nrow(KGIMXMM7)#29216566
library("data.table")
KGIMXMM7 <- as.data.table(KGIMXMM7)

KGIMXMM7$TERMSEQ_ <- as.character(KGIMXMM7$TERMSEQ_)
KGIMXMM7$TTYPE_ <- as.factor(KGIMXMM7$TTYPE_)
#BHNO_應為4碼字串，但轉入為factor型態，補0為4碼並轉成字串
library(stringr)
KGIMXMM7$BHNO_ <- str_pad(KGIMXMM7$BHNO_,4,"left",'0')
#CUSTNO_應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
KGIMXMM7$CUSTNO_ <- str_pad(KGIMXMM7$CUSTNO_,6,"left",'0')
#將CUSTCKNO_轉為字串
KGIMXMM7$CUSTCKNO_ <- as.character(KGIMXMM7$CUSTCKNO_)


########################################################################################
#tcount→近一年客戶交易次數
tcount7 <- select(KGIMXMM7 ,TDATE_ ,TERMSEQ_ ,TTYPE_ ,BHNO_ , CUSTNO_ , CUSTCKNO_ ) %>%   
  mutate( CUSTNO = paste0(BHNO_ , CUSTNO_ , CUSTCKNO_ )) %>%
  group_by( CUSTNO ) %>%
  summarise(TranCount = n()) %>%
  arrange(desc(TranCount))

head(tcount7,5)
tcount7 <- as.data.table(tcount7)

#############################################################################################

#近一年實動自然人(CUS_7)+近一年總貢獻度+20170930台股AUM +近一年總交易次數   By帳號
CCAT_7 <- sqldf("select * from CCA_7 c left join tcount7 t on c.CUSTNO = t.CUSTNO ", method = "name__class")
nrow(CCAT_7)
CCAT_7 <- CCAT_7[,-c(34)]
CCAT_7 <- as.data.table(CCAT_7)
str(CCAT_7)


#近一年實動自然人(CUS_7)+近一年總貢獻度+20170930台股AUM +近一年總交易次數   ByID
U.CCAT_7 <- sqldf("select * from CCAT_7 group by CUS050 ", method = "name__class")
U.CCAT_7 <- mutate(U.CCAT_7 , DiscountRate =  1-round(SumDISCOUNT / SumFEE ,2))
nrow(U.CCAT_7) #63729
U.CCAT_7 <- as.data.table(U.CCAT_7)
str(U.CCAT_7)


write.table(U.CCAT_5, file="U.CCAT_5.csv",sep=",",row.names=F)
write.table(U.CCAT_7, file="U.CCAT_7.csv",sep=",",row.names=F)

# library(openxlsx)
# library(rio)
# export(U.CCAT_5, "U.CCAT_5.xlsx")
# export(U.CCAT_7, "U.CCAT_7.xlsx")
# write.table(U.CCAT_5, file = "U.CCAT_5.xlsx", sep = " ", quote = FALSE, append = FALSE, na = "NA")
