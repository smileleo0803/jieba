library("RODBC")
myconn <-odbcConnect("101", uid="TW\0010173", pwd="love7283")  
NEWCUS <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,
                CUS120,CUS150,CUS250,CUS320,CUS350,CUS370,CUS380,CUS390,CUS570,CUS440 
                  from KGICS0M_20171001 ")

NEWCUS <- sqlQuery(myconn, "select CUS010,CUS020,CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,
                CUS370,CUS380,CUS390,CUS570
                from KGICS0M_20171001 ")
class(NEWCUS)

#CUS010轉字串
NEWCUS$CUS010 <- as.character(NEWCUS$CUS010)
#CUS020應為6碼字串，但轉入為int型態，補0為6碼並轉成字串
library(stringr)
NEWCUS$CUS020 <- str_pad(NEWCUS$CUS020,6,"left",'0')
#CUS030轉字串
NEWCUS$CUS030 <- as.character(NEWCUS$CUS030)
#將CUS010、CUS020、CUS030轉為字串，用paste成新變數"新客戶帳號"，供和AUM、REV、交易、貢獻比對
NEWCUS$CUSTNO <- paste0(NEWCUS$CUS010,NEWCUS$CUS020,NEWCUS$CUS030)
str(NEWCUS)

NEWCUS1 <- sqldf("select * from NEWCUS where CUS090 = 0 ")

test <- sqldf("select n.CUS010,n.CUS020,n.CUS030,CUS040,CUS050,CUS070,CUS080,CUS090,CUS120,
              n.CUS150,CUS250,CUS320,CUS350,CUS370,CUS380,CUS390,CUS570,CUS440,NBHNO_,
              NDEPT_,n.CUSTNO
              from CUS c inner join NEWCUS n on c.NBHNO_ = n.CUS010 and c.NDEPT_ = n.CUS150 ")


test <- sqldf("select c.CUS010,c.CUS020,c.CUS030,n.CUS040,n.CUS050,n.CUS070,n.CUS080,n.CUS090,n.CUS120,
              n.CUS150,n.CUS250,n.CUS320,n.CUS350,n.CUS370,n.CUS380,n.CUS390,n.CUS570,n.CUS440,c.NBHNO_,c.NDEPT_,n.CUSTNO
              from CUS c inner join NEWCUS n on c.CUSTNO = n.CUSTNO")


test <- sqldf("select n.CUS040,n.CUS050,n.CUS070,n.CUS080,n.CUS090,
                n.CUS370,n.CUS380,n.CUS390,n.CUS570, c.CUSTNO, n.CUSTNO
                from CUS c inner join NEWCUS1 n on c.CUSTNO = n.CUSTNO
                where n.CUS570 == '000' and n.CUS080 > 20160930")
test1 <- sqldf("select * from test where CUS090 = 0 ")
