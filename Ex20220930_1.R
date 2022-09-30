# ★ 기술통계분석 #


setwd("C:/bigdataR/Part-III")
getwd()

data <- read.csv("descriptive.csv")
head(data)
dim(data)   # 자료의 행과 열 개수를 알 수 있다

length(data)   # 데이터 열의 개수
length(data$resident)   # 특정한 열을 선택하여 길이를 알아보면 데이터 행의 개수를 알 수 있다.
str(data)
summary(data)
summary(data$gender)
x <- table(data$gender)   # 해당 열의 빈도수 측정
data <- subset(data, gender == 1 | gender == 2)   # subset : gender가 1 혹은 2 인것만 추출
x

barplot(x)   # 도수분포표 막대그래프
y <- prop.table(x)   # 비율

round(y * 100, 2)   # y : 0.5824916 등의 소수점 자리에 100을 곱하면 58.24916 같은 소수로 나타내고, 2자리수에서 반올림을 하면? => 백분율이 된다
data$level
summary(data$level)
table(data$level)   # 고졸, 대졸, 대학원졸
x1 <- table(data$level)   # 빈도수 저장
x1
barplot(x1)

survey <- data$survey
length(survey)
summary(survey)   # 최소, 최대, 중앙값, 평균(의미 없음)
table(survey)   # 가장 의미 있는 데이터, 항목별 빈도수
x1 <- table(survey)
x1
pie(x1)
hist(survey)
barplot(x1)

# ★ 기술통계 리포트 작성 #

summary(data$cost)
table(data$cost)
plot(data$cost)
data <- subset(data, data$cost >= 2 & data$cost <= 10)   # 원본데이터 뒤의 열에는 원본데이터$열 데이터 가 아닌 '열 데이터'만 작성해도 무관
length(data$cost)
x <- data$cost
mean(x)   # data 원본 파일의 정제된 cost 열의 값 중 평균
median(x)   # 열의 중앙 값(더 의미있는 데이터)
quantile(x, 1/4)   # x 데이터의 1/4 위치에 있는 데이터 값 출력
quantile(x, 3/4)

# ★ data$cost 데이터 중 가장 큰 값 위치 찾기

table(x)
x.t <- table(x)
class(x.t)
max(x.t)   # table 구조의 x.t 데이터에서 가장 큰 값을 출력

# 1) table 구조를 rbind 활용하여 값 찾기
x.m <- rbind(x.t)   # table 구조였던 2개의 행을 병합하면서 위에는 열, 아래는 행으로 matrix 구조로 변환시킴
x.m
class(x.m)   # matrix 구조
which(x.m[1, ] == 18)   # 검색조건에 부합되는 인덱스 반환

# 2) matrix를 dataframe으로 변환
x.df <- as.data.frame(x.m)
class(x.df)
x.df
which(x.df[1, ] == 18)   # 검색조건에 부합되는 인덱스 반환

x.df[1, 19]
attributes(x.df)   # 열 이름, 행 이름, 타입


# ★ 산포도 구하기
x
var(x)   # x 데이터의 분산
sd(x)   # x 데이터의 표준편차
sqrt(var(data$cost, na.rm = T))   # sd함수와 동일한 값 도출


data$cost
length(data$cost)
hist(data$cost)
plot(data$cost)

head(data)
data$cost2[data$cost >= 1 & data$cost <= 3] <- 1
data$cost2[data$cost >= 4 & data$cost <= 6] <- 2
data$cost2[data$cost >= 7] <- 3
str(data)
class(data)
attributes(data)
par(mfrow = c(1,2))
barplot(table(data$cost2))
pie(data$cost2)
par(mfrow = c(1,1))


# ★ 패키지를 활용한 비대칭도 구하기
install.packages("moments")
library(moments)
cost <- data$cost
skewness(cost)   # 왜도 구하기
kurtosis(cost)   # 첨도 구하기
hist(cost)
hist(cost, freq = F)   # 확률밀도 히스토그램
lines(density(cost), col = "blue")   # 확률밀도 추세선
x <- seq(0, 8, 0.1)
curve(dnorm(x, mean(cost), sd(cost)), col = "red", add = T)   # 정규분포를 만들어내는 함수 (dnorm : density norm)


attach(data)   # attach(데이터) : 데이터 자리에 있는 데이터의 열을 $ 기호를 쓰지 않고 불러낼 수 있다
length(cost)
summary(cost)
detach(data)   # attach 해제
age


# ★ 변수 리코딩(recoding)
table(data$resident)
summary(data$resident)

data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >= 2 & data$resident <= 4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"
data$resident2
x <- table(data$resident2)
x

prop.table(x)   # 비율계산 : 0 < x < 1 사이의 값값
y <- prop.table(x)
y
round(y * 100, 2)

# 1) 성별 리코딩
data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"

x <- table(data$gender2)
y <- prop.table(x)
y
round(y * 100, 2)

# 2) 나이 리코딩
data$age
table(data$age)
data$age2[data$age <= 45] <- "중년층"
data$age2[data$age >= 46 & data$age <= 59] <- "장년층"
data$age2[data$age >= 60] <- "노년층"

data$age2
x <- table(data$age2)
x
y <- prop.table(x)
round(y * 100, 2)

# 3) 학력 리코딩
data$level
data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"

x <- table(data$level2)
x
y <- prop.table(x)
y
round(y * 100, 2)



# ▣ type, pass 변수에 대해 빈도분석 수행, 결과를 막대그래프와 파이 차트로 시각화
head(data)
x <- table(data$type)
y <- table(data$pass)

par(mfrow = (c(1, 2)))
barplot(x)
pie(y)

# ▣ 나이 변수에 대해 요약치(평균, 표준편차)와 비대칭도(왜도와 첨도) 통계량을 구하고, 히스토그램 작성하여 비대칭도 통계량 설명
ageTable <- table(data$age)
ageTable

mean(ageTable)   # 평균
sd(ageTable)   # 표준편차

skewness(data$age)
kurtosis(data$age)

par(mfrow = c(1, 1))
hist(data$age, freq = F)
lines(density(data$age), col = "blue")

# ▣ 나이 변수에 대한 밀도분포 곡선과 정규분포 곡선으로 정규분포 검정
x <- seq(40, 69, 1)
curve(dnorm(x, mean(data$age), sd(data$age)), col = "red", add = T)


