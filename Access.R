library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  


###客戶交易管道檔(交易歷史檔Join交易管道)
CHNL5 <- sqlQuery(myconn, "select BHNO_ ,CUSTNO_,CUSTCKNO_,TDATE_, k.ORTYPE_ ,ORTPDSC_ ,ORTYPE1_ ,ORTPDSC1_ ,ORTYPE2_ ,ORTPDSC2_ ,VTYPE_ 
                from KGIMXMM k inner join KGICHNLTYPE t on k.ORTYPE_ = t.ORTYPE_
                where TDATE_ >20160930 and TDATE_ < 20171001")

CHNL7 <- sqlQuery(myconn, "select BHNO_ ,CUSTNO_,CUSTCKNO_,TDATE_, k.ORTYPE_ ,ORTPDSC_ ,ORTYPE1_ ,ORTPDSC1_ ,ORTYPE2_ ,ORTPDSC2_ ,VTYPE_ 
                from KGIMXMM k inner join KGICHNLTYPE t on k.ORTYPE_ = t.ORTYPE_
                where TDATE_ >20150930 and TDATE_ < 20161001")
library("data.table")
CHNL5 <- as.data.table(CHNL5)
CHNL7 <- as.data.table(CHNL7)

close(myconn) 

str(CHNL5)
#BHNO_分支代碼轉字串
CHNL5$BHNO_ <- as.character(CHNL5$BHNO_)
CHNL5$BHNO_  <- str_trim(CHNL5$BHNO_,side='both')
#CUSTNO_帳號應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
CHNL5$CUSTNO_  <- str_trim(CHNL5$CUSTNO_,side='both')
CHNL5$CUSTNO_ <- str_pad(CHNL5$CUSTNO_,6,"left",'0')
#CUSTCKNO_檢查碼轉字串
CHNL5$CUSTCKNO_ <- as.character(CHNL5$CUSTCKNO_)
CHNL5$CUSTCKNO_  <- str_trim(CHNL5$CUSTCKNO_,side='both')
#將BHNO_、CUSTNO_、CUSTCKNO_轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
CHNL5$CUSTNO <- paste0(CHNL5$BHNO_,CHNL5$CUSTNO_,CHNL5$CUSTCKNO_)

CHNL5 <- CHNL5[,c(5:10,12)]

CCHNL5 <- sqldf("select c.CUSTNO, ORTYPE_ ,ORTPDSC_ ,ORTYPE1_ ,ORTPDSC1_ ,ORTYPE2_ ,ORTPDSC2_ 
                from CUS_5 c inner join CHNL5 h on c.CUSTNO = h.CUSTNO") 


str(CCHNL5)
sort(round(table(CCHNL5$ORTYPE1_) / length(CCHNL5$ORTYPE1_) *100,2))
sort(table(CCHNL5$ORTYPE1_))

sort(round(table(CCHNL5$ORTPDSC_) / length(CCHNL5$ORTPDSC_) *100,2))
sort(table(CCHNL5$ORTPDSC_))





str(CHNL7)
#BHNO_分支代碼轉字串
CHNL7$BHNO_ <- as.character(CHNL7$BHNO_)
CHNL7$BHNO_  <- str_trim(CHNL7$BHNO_,side='both')
#CUSTNO_帳號應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
CHNL7$CUSTNO_  <- str_trim(CHNL7$CUSTNO_,side='both')
CHNL7$CUSTNO_ <- str_pad(CHNL7$CUSTNO_,6,"left",'0')
#CUSTCKNO_檢查碼轉字串
CHNL7$CUSTCKNO_ <- as.character(CHNL7$CUSTCKNO_)
CHNL7$CUSTCKNO_  <- str_trim(CHNL7$CUSTCKNO_,side='both')
#將BHNO_、CUSTNO_、CUSTCKNO_轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
CHNL7$CUSTNO <- paste0(CHNL7$BHNO_,CHNL7$CUSTNO_,CHNL7$CUSTCKNO_)

CHNL7 <- CHNL7[,c(5:10,12)]

CCHNL7 <- sqldf("select c.CUSTNO, ORTYPE_ ,ORTPDSC_ ,ORTYPE1_ ,ORTPDSC1_ ,ORTYPE2_ ,ORTPDSC2_ 
                from CUS_7 c inner join CHNL7 h on c.CUSTNO = h.CUSTNO") 


str(CCHNL7)
sort(round(table(CCHNL7$ORTYPE1_) / length(CCHNL7$ORTYPE1_) *100,2))
sort(table(CCHNL7$ORTYPE1_))

sort(round(table(CCHNL7$ORTPDSC_) / length(CCHNL7$ORTPDSC_) *100,2))
sort(table(CCHNL7$ORTPDSC_))



#####################################################################################################


library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  


###客戶月成交檔
Path <- sqlQuery(myconn, "select TMONTH_ , BHNO_,CUSTNO_ ,CUSTCKNO_, MKTYPE_ , ORTYPE_
                from MTSLCSDEAL
                where TMONTH_ >201509 and TMONTH_ < 201710")
library("data.table")
Path <- as.data.table(Path)

close(myconn) 

str(Path)
sort(round(table(Path$ORTYPE_) / length(Path$ORTYPE_) *100,2))
sort(table(Path$ORTYPE_))


Path5 <- Path[which(Path$TMONTH_ >201609 & Path$TMONTH_ < 201710)]
nrow(Path5)
sort(round(table(Path5$ORTYPE_) / length(Path5$ORTYPE_) *100,2))
sort(table(Path5$ORTYPE_))

Path7 <- Path[which(Path$TMONTH_ >201509 & Path$TMONTH_ < 201610)]
nrow(Path7)
sort(round(table(Path7$ORTYPE_) / length(Path7$ORTYPE_) *100,2))
sort(table(Path7$ORTYPE_))


