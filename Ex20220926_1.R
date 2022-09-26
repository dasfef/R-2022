# ★ 반정형데이터 접근 및 처리 (html)

install.packages("httr")
install.packages("XML")

library(httr)
library(XML)

url <- "https://media.daum.net"
web <- GET(url)   # 지정된 url에 접속해서 html 형식의 소스코드를 반환(단순 텍스트)
web

# htmlTreeParse() : url 소스 -> html 태그 파싱
help(htmlTreeParse)
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
html
rootNode <- xmlRoot(html)   # 최상위 노드 찾기
rootNode

news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue)   # 해당되는 모든 노드를 탐색하여 벡터로 저장
news

news_pre <- gsub("[\r\n\t]", '', news)   # gsub("패턴", "교체문자", 자료)  : 특수문자를 찾아서 제거 후 저장
news_pre <- gsub("[[:punct:]]", '', news_pre)   # 문장부호 제거
news_pre <- gsub("[[:cntrl:]]", '', news_pre)   # 특수문자 제거
news_pre

library(stringr)
news_pre <- str_trim(news_pre)   # 문자열의 좌우측 공백 제거

setwd("C:/bigdataR")
write.csv(news_pre, "news_data.csv", quote = F, fileEncoding = "euc-kr")   # 큰 따옴표 저장하지 않음 : quote = F
news_data <- read.csv("news_data.csv", header = T, fileEncoding = "euc-kr")
str(news_data)
names(news_data) <- c("No", "News_txt")   # 열이름 변경
news_data
write.csv("news_data.csv", fileEncoding = "euc-kr", row.names = F)


# ▣ 코로나19 사이트의 뉴스&이슈 번호, 제목, 담당 스크래핑
# https://ncov.kdca.go.kr
# covid.csv로 저장
# 3개의 열을 각각 스크래핑하여 저장

library(httr)
library(stringr)
library(XML)

url2 <- "http://ncov.kdca.go.kr/tcmBoardList.do?brdId=&brdGubun=&dataGubun=&ncvContSeq=&contSeq=&board_id=140&gubun="
web2 <- GET(url2)
web2

html2 <- htmlTreeParse(web2, useInternalNodes = T, trim = T, encoding = "utf-8")
html2
rootNode <- xmlRoot(html2)
rootNode

covid <- xpathSApply(rootNode, "//a[@class = 'bl_link']", xmlValue)
covid_test <- xpathSApply(rootNode, "//td[@class = 'm_dp_n']", xmlValue)

covid_pre <- gsub('[[:punct:]]', '', covid)
covid_data <- covid_pre

names(covid_data) <- c("No", "Covid_txt")
write.csv(covid_data, "covid.csv", fileEncoding = "euc-kr", row.names = T)
covid_read <- read.csv("covid.csv", header = T, fileEncoding = "euc-kr")
covid_read

#---------------------------------풀이----------------------------------#

url2 <- "http://ncov.kdca.go.kr/tcmBoardList.do?brdId=&brdGubun=&dataGubun=&ncvContSeq=&contSeq=&board_id=140&gubun="
web2 <- GET(url2)
html2 <- htmlTreeParse(web2, useInternalNodes = T, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html2)

covidNum <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[1]", xmlValue)
covidSubject <- xpathSApply(rootNode, "//a[@class = 'bl_link']", xmlValue)
covidPart <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[3]", xmlValue)
covidDate <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[4]", xmlValue)
covidNum
covidSubject
covidPart

covidNum_pre <- gsub('[[:punct:]]', '', covidNum)
covidNum_pre <- gsub('[[:cntrl:]]', '', covidNum)
covidSubject_pre <- gsub('[[:cntrl:]]', '', covidSubject)
covidSubject_pre <- gsub('[[:punct:]]', '', covidSubject)
covidPart_pre <- gsub('[[:cntrl:]]', '', covidPart)
covidPart_pre <- gsub('[[:punct:]]', '', covidPart)

covid <- data.frame(번호 = covidNum, 제목 = covidSubject, 담당 = covidPart, 날짜 = covidDate)
covid

write.csv(covid, "covid.csv", fileEncoding = "euc-kr", row.names = F)



# 반복문을 통한 여러 페이지 웹 크롤링


df <- data.frame(번호 = NULL, 제목 = NULL, 담당 = NULL, 날짜 = NULL)   # 공백 데이터프레임

for (i in c(1:10)) {
  url <- paste("http://ncov.kdca.go.kr/tcmBoardList.do?pageIndex=",  i,  "&brdId=&brdGubun=&board_id=140&search_item=1&search_content=", sep = "")
  web <- GET(url)
  html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
  rootNode <- xmlRoot(html)
  
  covidNum <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[1]", xmlValue)
  covidSubject <- xpathSApply(rootNode, "//a[@class = 'bl_link']", xmlValue)
  covidPart <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[3]", xmlValue)
  covidDate <- xpathSApply(rootNode, "//div[@class = 'board_list']//tbody/tr/td[4]", xmlValue)
  
  subdf <- data.frame(번호 = covidNum, 제목 = covidSubject, 담당 = covidPart, 날짜 = covidDate)   # 10개짜리 분량의 데이터 프레임 생성
  df <- rbind(df, subdf)   # df가 계속 누적됨
}

df

write.csv(df, "covidall.csv", fileEncoding = "euc-kr", row.names = F)


# ★ 옥션 웹크롤링(한글로 된 웹사이트 변수명)
searchName <- URLencode("노트북")
searchName

url <- paste("http://browse.auction.co.kr/search?keyword=", searchName,   "&itemno=&nickname=&frm=hometab&dom=auction&isSuggestion=No&retry=&Fwk=",     searchName,   "&acode=SRP_SU_0100&arraycategory=&encKeyword=", searchName, "&k=32&p=1", sep = "")
url
web <- GET(url)
web

html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = "utf-8")
html
rootNode <- xmlRoot(html)
rootNode

productName <- xpathSApply(rootNode, "//span[@class = 'text--title']", xmlValue)
productName
productPrice <- xpathSApply(rootNode, "//strong[@class = 'text--price_seller']", xmlValue)
productPrice

productPrice <- gsub(",", '', productPrice)   # 숫자 사이의 쉼표(,) 제거

df <- data.frame(품명 = productName, 단가 = productPrice)
df

write.csv(df, "auction.csv", row.names = F, fileEncoding = "euc-kr")



# ★ XML 파일 크롤링
url <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getCtprvnRltmMesureDnsty?serviceKey=74c5ZdkDfPARwepbwip9XOy3B2OYJkUBxj12RhzlgPLU34nfU9FiPmprQPybkilNGZS10zlDq1jRt9PG6HH0uQ%3D%3D&returnType=xml&numOfRows=100&pageNo=1&sidoName=%EC%B6%A9%EB%B6%81&ver=1.0"

xmlFile <- xmlParse(url)   # 파서를 이용하여 저장
df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))   # 반복탐색 하면서 item의 내용을 저장
df
stationName <- df$stationName
pm10Value <- as.numeric(df$pm10Value)   # 숫자형으로 변환해주는 함수 : as.numeric()
pm10Value

barplot(pm10Value, names.arg = stationName, col = rainbow(7))


# ★ 측정소별 10시간 측정정보 조회
url <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getMsrstnAcctoRltmMesureDnsty?serviceKey=74c5ZdkDfPARwepbwip9XOy3B2OYJkUBxj12RhzlgPLU34nfU9FiPmprQPybkilNGZS10zlDq1jRt9PG6HH0uQ%3D%3D&returnType=xml&numOfRows=10&pageNo=1&stationName=%EB%B3%B5%EB%8C%80%EB%8F%99&dataTerm=DAILY&ver=1.0"

xmlFile <- xmlParse(url)
df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))
df

pm10Value <- as.numeric(df$pm10Value)
pm10Value <- ifelse(!is.na(pm10Value), pm10Value, round(mean(pm10Value, na.rm = TRUE), 0))   # ! na 라면 그대로 두고, na라면 평균을 구하고 na.rm 실행

dataTime <- df$dataTime
str_sub(dataTime, 12)

barplot(pm10Value, names.arg = str_sub(dataTime, 12), col = rainbow(7))

pm10Value



# ★ wordcloud

install.packages("wordcloud")
library(wordcloud)

word <- c("서울", "부산", "대구")   # 항목 = 문자열
freq <- c(300, 230, 150)   # 빈도수 = 정수
wordcloud(word, freq, random.order = F, random.color = F, col = rainbow(3))

data <- read.csv("C:/bigdataR/101_DT_1B26001_A01_M_20220926164759.csv", header = T, fileEncoding = "euc-kr")
head(data)

x <- grep("시$", data$행정구역.시군구.별)
x
data1 <- data[x, ]
x <- grep("군$", data$행정구역.시군구.별)
data2 <- data[x, ]

data3 <- rbind(data1, data2)
data4 <- data3[data3$순이동.명. > 0, ]
data4

word <- data4$행정구역.시군구.별
freq <- data4$순이동.명.
wordcloud(data4$행정구역.시군구.별, data4$순이동.명., random.order = F, random.color = F, col = rainbow(7))

df <- data.frame(지역 = word, 빈도수 = freq)
df

# ★ wordcloud2

install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(df)
