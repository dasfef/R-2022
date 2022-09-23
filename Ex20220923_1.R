# R Java 연동

install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

Sys.setenv(JAVA_HOME = "C:/bigdataR/jdk-11")

library(rJava)
library(DBI)
library(RJDBC)

drv <- JDBC(driverClass = "com.mysql.jdbc.Driver", "C:/bigdataR/mysql-connector-java-5.1.49.jar")   # 드라이브 경로 설정

conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/work", "scott", "tiger")   # mysql 접속 (드라이브설정 변수, mysql 경로, 계정, 비밀번호)


query <- "select * from goods" 
goodsAll <- dbGetQuery(conn, query)   # 지정된 DB에 접속하여 Query를 실행 후 결과 반환
goodsAll

query <- "select * from goods order by dan desc"   # 단가(dan) 기준 desc(내림차순) 정렬
goodsAll <- dbGetQuery(conn, query)   # 단가로 내림차순 정렬된 goodsAll 변수가 저장됨
goodsAll

insert.df <- data.frame(code = 5, name = '식기세척기', su = 1, dan = 250000)
insert.df

dbWriteTable(conn, "goods", insert.df)   # 데이터프레임을 이용한 일괄입력
dbWriteTable(conn, "goods1", insert.df)

query <- "select * from goods1"
goodsAll <- dbGetQuery(conn, query)
goodsAll


# csv 파일 읽어서 mariaDB 에서 활용하기
getwd()
setwd("C:/bigdataR")
recode <- read.csv("recode.csv", fileEncoding = "euc-kr", header = T)
recode

dbWriteTable(conn, "goods2", recode)

# goods2 Table 조회 및 추가, 삭제 수정
query <- "select * from goods2"
goodsAll <- dbGetQuery(conn, query)   # query 실행 후 반환
goodsAll

# goods2 Table에 행 추가
query <- "insert into goods2 values(6, 'test', 1, 1000)"
dbSendUpdate(conn, query)

# goods2 Tabld의 데이터를 수정
query <- "update goods2 set name = '테스트' where code = 6"
dbSendUpdate(conn, query)

# goods2 Table의 데이터를 삭제
query <- "delete from goods2 where code = 6"
dbSendUpdate(conn, query)




query <- "select * from goods"
goodsAll <- dbGetQuery(conn, query)
goodsAll



# ▣ code가 6, 단가가 200000인 '청소기' 2개  추가

query <- "insert into goods2 value(6, '청소기', 2, 200000)"
dbSendUpdate(conn, query)

query <- "select * from goods2"
goodsAll <- dbGetQuery(conn, query)
goodsAll

query <- "delete from goods2 where code = 6"
dbSendUpdate(conn, query)



# ▣ 단가 600000 보다 큰 상품에 대해 수량을 5로 수정

query <- "update goods2 set su = 5 where dan >= 600000"
dbSendUpdate(conn, query)

query <- "select * from goods2"
goodsAll <- dbGetQuery(conn, query)
goodsAll



# ▣ 수량이 1인 상품을 삭제 

query <- "delete from goods2 where su = 1"
dbSendUpdate(conn, query)
query <- "select * from goods2"

goodsAll <- dbGetQuery(conn, query)
goodsAll


# ▣ 각 문법이 실행될 때마다 전체 테이블 조회



# ★ insert into, update "table", delete from 이외의 조회 문법들

# 1) 특정 문자로 시작하거나 끝나는 문자열 조회(like, %)
query <- "select * from goods2 where name like '%기'"
goodsAll <- dbGetQuery(conn, query)
goodsAll


# 2) 일부 열의 데이터만 추가하는 문법
query <- "insert into goods2 (code, name) values(7, '안마의자')"   # 넣고자 하는 열의 열 이름과 값을 수에 맞추어 넣어준다다
dbSendUpdate(conn, query)

query <- "select * from goods2"
goodsAll <- dbGetQuery(conn, query)
goodsAll


# 3) update의 경우 복수의 열에 대해 수정이 가능하다
query <- "update goods2 set su = 1, dan = 560000 where code = 2"
dbSendUpdate(conn, query)

query <- "select * from goods2"
goodsAll <- dbGetQuery(conn, query)
goodsAll


# ★ DML(Data Manipulation Language) : 데이터 조작어

# select, insert, update, delete

# select 필드명리스트(*) from 테이블명 [where 조건절 order by 정렬키(열 이름 등) 순서(오름, 내림차순)]

# insert into 테이블명 (필드리스트) values (값리스트)

# update 테이블명 set 필드명1 = 값1, 필드명2 = 값2, ... where 조건절

# delete from 테이블명 where 조건절



# ▣ shopUser, shopProduct, shopSale 3개의 테이블 생성 

# shopUser : uid(문자열10/키), uname(문자열20), uage(정수)

# shopProduct : pCode(int/키), pName(문자열20), pPrice(int)

# shopSale : uid, pCode, sCount(int)

# 테이블 삭제시 drop table 명령어 사용



# ▣ 김삿갓이 구매한 내역에 대해 성명, 상품명, 수량 조회 

query <- "select * from shopuser"
goodsAll <- dbGetQuery(conn, query)
goodsAll

query <- "SELECT su.uname, sp.pname, ss.scount, sp.pprice * ss.scount subtot
	FROM shopuser AS su, shopproduct AS sp, shopsale AS ss
	WHERE su.uid = ss.uid AND sp.pcode = ss.pcode AND su.uname = '김삿갓'"
goodsAll <- dbGetQuery(conn, query)
goodsAll
