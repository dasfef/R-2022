# ▣ 중소기업에서 생산한 HDTV 판매율 높이기 위해 프로모션을 진행한 결과 기존 구매비율보다 15% 향상되었는가(p.447)
# 연구가설 : 프로모션 후 기존 구매비율보다 향상되었다
# 귀무가설 : 프로모션 전과 후 구매비율 차이가 없다

setwd("C:/bigdataR")
hdtv <- read.csv("hdtv.csv", header = T)
head(hdtv)
x <- hdtv$buy
summary(x)
table(x)
length(x)
library(prettyR)
freq(x)
binom.test(10, 50, p = 0.15)   # 양측검정(이항분포)
binom.test(10, 50, p = 0.15, alternative = "greater")   # 단측검정
binom.test(10, 50, p = 0.15, alternative = "less")   # 단측검정

# 최종검정 결과 : 프로모션 전과 후 구매비율 차이가 없다


# ▣ 우리나라 전체 중2 여학생 평균 키 148.5cm, A중학교 2학년 전체 500명을 대상의 10% 50명을 표본으로 표본평균 신장 계산, 모집단의 평균과 차이가 있는지
# 연구가설 : 수집한 표본이 우리나라 전체 평균과 차이가 있다
# 귀무가설 : 수집한 표본이 우리나라 전체 평균간 차이가 없다
# 정규성 검정
# 전체학생 : 148.5cm
# 표본 : 50명

stheight <- read.csv("student_height.csv", header = T)
head(stheight)
height <- stheight$height
summary(height)
table(height)
length(height)

N <- 50   # 표본수
X <- mean(height)   # 표본평균
S <- sd(height)   # 표본 표준편차(모집단 표준편차 모를 경우)
X
# X 표본 표준편차 - 149.4 와 전체 평균 키 148.5 가 차이가 있는지?

low <- X - (1.96 * S) / sqrt(N)
high <- X + (1.96 * S) / sqrt(N)
low; high
# 정규성 검정
# 귀무가설 : 정규분포와 차이가 없다
# 대립가설 : 정규분포와 차이가 있다(p-value < 0.05)
shapiro.test(height)
# 정규분포와 차이가 있다
hist(height)   # 정규분포가 아니란 걸 확인할 수 있음
qqnorm(height)
qqline(height, lty = 1, col = "blue")   # 해당 라인이 꼭짓점을 연결하는 45도 정도 되는 그림이 나와야 정규성 분포라고 볼 수 있다
# 평균차이 검정 : t-test
t.test(height, mu = 148.5)
# p-value 가 0.05 보다 큰 값이기 때문에 귀무가설 채택, 모집단과 차이가 없다




# ▣ 남학생과 여학생 두 집단의 비율차이 검정
# 대학에 진학한 남학생, 여학생 만족도 차이
# 연구가설 : 남학생과 여학생의 만족도 차이가 있다
# 귀무가설 : 남학생과 여학생의 만족도 차이가 없다
# 만족 : 1, 불만족 : 0
# 남자 : 1, 여자 : 2
data <- read.csv("two_sample.csv", header = T)
head(data)
summary(data)
x <- data$gender
y <- data$survey
table(x, y)
table(x)
table(y)
prop.test(c(138, 107), c(174, 126))   # 만족도 비율 검정 : p-value = 0.2765 = 귀무가설 채택, 남녀 만족도에 차이가 없다



# ▣ 교육 방법에 따라 시험 성적에 차이가 있는지
# 연구가설 : 교육 방법에 따른 시험 성적 차이가 있다
# 귀무가설 : 교육 방법에 따른 시험 성적 차이가 없다
# method : 교육 방법, score : 시험 성적

data <- read.csv("twomethod.csv", header = T)
head(data)
summary(data)
result <- subset(data, !is.na(score), c(method, score))
result
a <- subset(result, method == 1)   # 교육 방법 1 추출
b <- subset(result, method == 2)   # 교육 방법 2 추출
a1 <- a$score   # 교육 방법 1로 진행한 성적
b1 <- b$score   # 교육 방법 2로 진행한 성적
a1
b1
length(a1)
length(b1)
mean(a1)
mean(b1)
# 동질성 검정(두 집단일 경우 필수 사항 = 두 집단의 분산 비교)
# 대립가설 : 두 집단 간 분산 차이가 있다
# 귀무가설 : 두 집단 간 분산 차이가 없다
var.test(a1, b1)
# p-value = 0.8494 = 두 집단 간 분산 차이가 없다
t.test(a1, b1)
t.test(a1, b1, alternative = "greater")
t.test(a1, b1, alternative = "less")   # a1이 b1 보다 작다는 단측 검정 채택
# 방법1이 방법2에 비해서 성적이 낮다
