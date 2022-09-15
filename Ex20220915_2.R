# ★ 변수 간의 관계 분석
getwd()
new_data <- read.csv("new_data.csv", header = T, fileEncoding = "euc-kr")   # 파일에 한글이 섞여 있을때 fileEncoding을 활용하여 "euc-kr" 을 추가해준다.
head(new_data)
str(new_data)

resident_gender <- table(new_data$resident2, new_data$gender2)   # table(행, 열)
gender_resident <- table(new_data$gender2, new_data$resident2)

barplot(resident_gender, beside = T, col = rainbow(5), legend = row.names(resident_gender), horiz = T, main = "성별에 따른 거주지역 분포")
barplot(gender_resident, beside = T, col = rainbow(2), legend = row.names(gender_resident), horiz = F, main = "거주지역에 따른 성별 분포")


# 연속형 vs 범주형
install.packages("lattice")   # 고급 시각화 lattice 패키지
library(lattice)

densityplot(~age, data = new_data, groups = job2, auto.key = T)   # auto.key = T : 범례

# 연속형 vs 범주형 vs 범주형
densityplot(~price | factor(gender2), data = new_data, groups = position2, auto.key = T)

densityplot(~price | factor(position2), data = new_data, groups = gender2, auto.key = T, plot.points = F)   # plot.points = T : default 값


# 연속형 2개 vs 범주형
xyplot(price ~ age | factor(gender2), data = new_data)


# ★ 더미 형식으로 파생변수 생성하기
user_data <- read.csv("user_data.csv", header = T, fileEncoding = "euc-kr")   # user_data.csv 파일 불러오기기
head(user_data)
table(user_data$house_type)   # house_type 의 빈도수 확인
length(user_data$house_type)

house_type2 <- ifelse(user_data$house_type == 1 | user_data$house_type == 2, 0, 1)   # 단독주택, 다가구 : 0 / 아파트, 오피스텔 : 1
                                                                                     # ifelse를 통해 더미 변수를 생성, 범주는 위와 같음
house_type2
user_data$house_type2 <- house_type2
head(user_data)

pay_data <- read.csv("pay_data.csv", header = T, fileEncoding = "euc-kr")   # pay_data.csv 파일 읽어오기
head(pay_data)
table(pay_data$product_type)   # pay_data$product_type 의 빈도수 확인

library(reshape2)   # 구조 변경을 위한 패키지 로딩
product_price <- dcast(pay_data, user_id~product_type, sum, na.rm = T)   # wide 포맷으로 dcast를 활용하여 행, 열로 넓게 구조 변경 진행
head(product_price, 10)
