# 신뢰구간 추정
# 모평균의 구간 추정

# ★ 모평균의 신뢰 구간
n <- 10000
xbar <- 165.1
S <- 2
low <- xbar - 1.96 * (S / sqrt(n))
high <- xbar + 1.96 * (S / sqrt(n))
low; high

# 표본오차 
(low - xbar) * 100
(high - xbar) * 100


# ★ 모비율의 구간 추정
# A 반도체 회사의 150명을 표본조사하여 90명이 여자임을 확인했다. 이 회사 전체의 여자 비율을 추정하라.(신뢰구간 95%)
n <- 150
pbar <- 0.6
qbar <- 1 - pbar
low <- pbar - 1.96 * (qbar / sqrt(n))
high <- pbar + 1.96 * (qbar / sqrt(n))
low; high

low <- pbar - 1.96 * (sqrt((pbar * qbar) / n))
low

# 풀이
n <- 150
pbar <- 90 / 150
qbar <- 1 - pbar
low <- pbar - 1.96 * sqrt((pbar * qbar) / n)
high <- pbar + 1.96 * sqrt((pbar * qbar) / n)
low; high


# ★ 고객만족도
getwd()
setwd("C:/bigdataR")
data <- read.csv("one_sample.csv", header = T)
head(data)
x <- data$survey
table(x)

install.packages("prettyR")   # freq() 함수 사용 패키지
library(prettyR)

freq(x)

n <- 150
binom.test(14, n, p = 0.2)   # 기존 불만율 20% 불만율을 기준으로 검정 실시 (p = 0.2) / 양측검정(차이만 검정)

binom.test(14, n, p = 0.2, alternative = "two.sided")   # alternative = "two.sided" 가 default(양측검정) 설정이다.

binom.test(14, n, p = 0.2, alternative = "two.sided", conf.level = 0.95)   # confidential level = 0.95 <- 95% 신뢰수준으로 진행되는 default 설정

binom.test(14, n, p = 0.2, alternative = "greater", conf.level = 0.95)   # alternative = "greater" 를 통해 14명의 불만족 고객이 전체비율의 20%보다 더 큰 비율인가를 검정하기 위함

binom.test(14, n, p = 0.2, alternative = "less", conf.level = 0.95)


# ★ 노트북 사용시간 검정(국내와 A사 비교 / p.417)
data <- read.csv("one_sample.csv")
head(data)
str(data)

x <- data$time
x
mean(x, na.rm = T)   # NA 값 제거 방법 1
x1 <- na.omit(x)   # NA 값 제거 방법 2
mean(x1)

# 정규성 검정 : '정규분포와 다르지 않다' 라는 귀무가설을 채택 해야 한다. = p-value가 0.05보다 커야 한다.
shapiro.test(x1)

# 정규분포 시각화
par(mfrow = c(1,2))
hist(x1)
qqnorm(x1)
qqline(x1, lty = 1, col = "blue")   # lty (= line type) 

# 평균 차이 검정
t.test(x1, mu = 5.2)   # mu 속성 : 비교할 기존 모집단의 평균값 지정
t.test(x1, mu = 5.2, alternative = "two.sided", conf.level = 0.95)   # 기본값 속성
t.test(x1, mu = 5.2, alternative = "greater", conf.level = 0.95)
t.test(x1, mu = 5.2, alternative = "less", conf.level = 0.95)
# t값이 귀무가설 임계값(절대값) 보다 클 경우 귀무가설을 기각한다(t 테스트 검정)


# ★ 두 집단 비율 검정
# 교육 방법에 따른 만족도 차이 (불만족 : 0, 만족 : 1)
data <- read.csv("two_sample.csv", header = T)
head(data)

x <- data$method
y <- data$survey
table(x)
table(y)

table(x, y, useNA = "ifany")   # useNA = "ifany" : 결측치까지 출력
prop.test(c(110, 135), c(150, 150), alternative = "two.sided", conf.level = 0.95)
prop.test(c(110, 135), c(150, 150), alternative = "greater", conf.level = 0.95)
prop.test(c(110, 135), c(150, 150), alternative = "less", conf.level = 0.95)


# ★ 교육 방법에 따른 실기시험 성적 차이 검정
data <- read.csv("two_sample.csv", header = T)
result <- subset(data, !is.na(score), c(method, score))

a <- subset(result, method == 1)   # PT 교육
b <- subset(result, method == 2)   # 코딩 교육
a1 <- a$score
b1 <- b$score
a1
b1

var.test(a1, b1)   # 분산 비교 : p-value 가 0.05보다 클 경우 분산 비교 결과가 비슷하다는 뜻(동질성 있음)

t.test(a1, b1, conf.int = T, conf.level = 0.95)


# ★ 대응표본 T 검정
# 교수법 프로그램 진행 전과 후의 평균 차이
data <- read.csv("paired_sample.csv", header = T)
head(data)
result <- subset(data, !is.na(after), c(before, after))
result

x <- result$before
y <- result$after
mean(x)
mean(y)

var.test(x, y)
t.test(x, y, paired = T)

# ★ 세 집단 비율(만족율) 검정
data <- read.csv("three_sample.csv", header = T)
head(data)

method <- data$method
survey <- data$survey
method
survey

table(method)
table(survey)   # 의미없음
table(method, survey)

prop.test(c(34, 37, 39), c(50, 50, 50))


# ★ 분산분석(3개 이상의 집단간 평균 검정에 사용)
# 3가지 교육 방법에 따라 측정한 실기시험의 평균에 차이가 있다. 를 검정(연구가설)

data <- read.csv("three_sample.csv", header = T)
data <- subset(data, !is.na(score), c(method, score))   # subset(원본데이터, 가져갈 데이터)
length(data$score)
par(mfrow = c(1,2))
plot(data$score)
barplot(data$score)
mean(data$score)

data2 <- subset(data, score <= 14)
par(mfrow = c(1, 1))
boxplot(data2$score)
boxplot(data$score)

# 보기 쉽게 리코딩 진행
data2$method2[data2$method == 1] <- "방법 1"
data2$method2[data2$method == 2] <- "방법 2"
data2$method2[data2$method == 3] <- "방법 3"

head(data2)
x <- table(data2$method2)
x
help(tapply)

y <- tapply(data2$score, data2$method2, mean)
y
df <- data.frame(교육방법 = x, 성적 = y)
df

bartlett.test(score ~ method, data = data2)

help(aov)
head(data2)
result <- aov(score ~ method2, data = data2)
result
summary(result)

TukeyHSD(result)   # 분산분석의 사후검정(집단별 평균의 차)
plot(TukeyHSD(result))   # 사후검정 시각화


# ▣ 기존 구매비율보다 15% 향상되었는지 분석 및 검정
hdtv <- read.csv("hdtv.csv", header = T)
head(hdtv)

table(hdtv)
freq(hdtv)

x <- hdtv$buy
table(x)
freq(x)

binom.test(10, 50, p = 0.1, alternative = "two.sided", conf.level = 0.95)
binom.test(10, 50, p = 0.1, alternative = "greater")
binom.test(10, 50, p = 0.1, alternative = "less")

binom.test(10, 50, p = 0.15, alternative = "two.sided", conf.level = 0.95)
binom.test(10, 50, p = 0.15, alternative = "greater", conf.level = 0.95)
binom.test(10, 50, p = 0.15, alternative = "less", conf.level = 0.95)


# ▣ 학생 평균키 모집단의 평균과 차이가 있는지 분석 수행 후 검정
stheight <- read.csv("student_height.csv", header = T)
height <- stheight$height
head(height)
head(stheight)
mean(height)

summary(height)
shapiro.test(height)

par(mfrow = c(1, 2))
hist(height)
qqnorm(height)
qqline(height, lty = 1, col = "red")

x1 <- mean(height)
shapiro.test(height)
wilcox.test(height)

wilcox.test(x1, mu = 148.5)


# ▣ 대학에 진학한 남학생, 여학생을 대상으로 진학한 대학에 대해 만족도에 차이가 있는가를 검정하라.
data <- read.csv("two_sample.csv", header = T)
head(data)
length(data$gender)
x <- data$gender
y <- data$survey

table(x, y)
prop.test(c(138, 107), c(300, 300))


# ▣ 교육 방법에 따라 시험 성적에 차이가 있는지 검정하라.
data <- read.csv("twomethod.csv", header = T)
head(data)
table(data)
table(data$score)
summary(data$score)
summary(data$method)

result <- subset(data, !is.na(score), c(method, score))
result

a <- subset(result, method == 1)
b <- subset(result, method == 2)
a; b

a1 <- a$score
b1 <- b$score
a1; b1

var.test(a1, b1)
t.test(a1, b1)
