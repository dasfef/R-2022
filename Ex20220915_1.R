# ★reshape 패키지 활용

install.packages("reshape2")
library(reshape2)

data <- read.csv("C:/bigdataR/PartII/data.csv", header = T)   # 정확한 경로 지정 대신 쓰이는 함수 : file.choose()
data
wide <- dcast(data, Customer_ID~Date, sum)
wide

setwd("C:/bigdataR/PartII")
write.csv(wide, "wide_.csv")   # 행이름 저장됨
write.csv(wide, "wide__.csv", row.names = F)   # 행이름 제거되어 저장
wide <- read.csv("wide__.csv")
wide

colnames(wide)   # 열이름 출력
colnames(wide) <- c("Customer_ID", "day1", "day2", "day3", "day4", "day5", "day6", "day7")   # colnames() 함수 활용, 열이름 변경
wide

long <- melt(wide, id = "Customer_ID")   # long format 으로 변경
long
colnames(long)
colnames(long) <- c("Customer_ID", "Date", "Buy")
long

smiths
long <- melt(id = 1:2, smiths)
long
wide <- dcast(long, subject + time ~...)
wide


airquality
str(airquality)
help("airquality")

names(airquality) <- toupper(names(airquality))
names(airquality)

air_melt <- melt(airquality, id = c("MONTH", "DAY"), na.rm = T)
air_melt
head(air_melt)
names(air_melt)
names(air_melt) <- tolower(names(air_melt))
acast <- acast(air_melt, day ~ month ~ variable, na.rm = T)
acast
names(acast)
acast2 <- acast(air_melt, month ~ variable, mean, margins = F)
acast2



# ★EDA 와 전처리

getwd()   # 현재 작업 디렉토리
dataset <- read.csv("dataset.csv", header = T)
dataset
View(dataset)
attributes(dataset)
str(dataset)
summary(dataset)   # 기술통계(최대, 최소, 개수, 평균, 중앙값, 합계, 분산, 표준편차 등)

dataset$age
dataset$resident
length(dataset$age)

x <- dataset$gender
x
y <- dataset$price
y

plot(y)   # y축, x축은 인덱스로 설정됨
dataset["gender"]
head(dataset["gender"])
dataset$gender
dataset[,2]
dataset[c(2,4:6,3,1)]   # == dataset[ , c(2, 4:6, 3, 1)]

summary(dataset$price)   # 결측치 확인
sum(dataset$price, na.rm = T)   # NA 가 섞여있기 때문에 합계를 구할 수 없음

price2 <- na.omit(dataset$price)   # na.omit을 통한 NA 값 제거
sum(price2)

x <- dataset$price
x
ifelse(!is, na(x), 0, x)   # NA 가 아닐 경우 x, NA 일 경우 0


round(mean(x, na.rm = T), 2))    # 소수 2번째자리 안올림함 소수셎
ifelse(!is.na(x), x, round(mean(x, na.rm = T), 2))


table(dataset$gender)   # 빈도수 정렬
pie(table(dataset$gender))


# gender 값 극단치 정제
dataset <- subset(dataset, gender == 1 | gender == 2)
dataset
length(dataset$gender)



dataset <- read.csv("dataset.csv", header = T)
dataset$price
summary(dataset$price)

# price 값 극단치 정제
dataset2 <- subset(dataset, price >= 2 & price <= 8)
dataset2
summary(dataset2$price)
length(dataset2$price)
stem(dataset2$price)

# age 값 극단치 정제
summary(dataset2$age)
length(dataset2$age)
dataset2 <- subset(dataset2, age >= 20 & age <= 69)
summary(dataset2$age)

boxplot(dataset2$age, col = "red")   # 극단값 제거 후의 age 값 boxplot 으로 표현

boxplot(dataset$price)   # 원본데이터(극단값 보유)의 boxplot 으로 확인

boxplot(dataset$price)$stats   # $stats 를 이용하여 dataset$price 의 통계치 확인 : 해당 값을 통해 오류로 지정할 수 있는 극단값 확인 가능
dataset_sub <- subset(dataset, price >= 2.1 & price <= 7.9)
dataset_sub
summary(dataset_sub$price)


# ★코딩 변경 : 1,2 와 같이 숫자로 표기되던 것(gender)을 실제 문자(남,여)로 표기해야 할 때
# resident 거주지 코딩 변경
dataset2$resident2[dataset2$resident == 1] <- "1.서울특별시"
dataset2$resident2[dataset2$resident == 2] <- "2.인천광역시"
dataset2$resident2[dataset2$resident == 3] <- "3.대전광역시"
dataset2$resident2[dataset2$resident == 4] <- "4.대구광역시"
dataset2$resident2[dataset2$resident == 5] <- "5.시구군"

summary(dataset2$resident)
dataset2[ , c("resident", "resident2")]


# age 코딩 변경
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55] <- "장년층"

dataset2[ , c("age", "age2")]
head(dataset2)

# ★ 역 코딩 : 1~5 매우만족~불만족 인 경우를 1~5 불만족~매우만족 으로 바꿀 경우를 역코딩이라고 한다
survey <- dataset2$survey
survey
csurvey <- 6 - survey   # 5가 최대수인 경우 6에서 각 원소 값을 빼주면 역 코딩이 된다 (6-1 = 5, 6-5 = 1)
csurvey

dataset2$survey <- csurvey   # 기존 dataset2$survey 에 역 코딩한 csurvey를 덮어 씌운다. 한 번 진행하면 되돌릴 수 없다.
head(dataset2)
