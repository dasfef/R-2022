# ◎ 요인분석
# ▣ 공통요인으로 변수 정제
# 1) 변수와 데이터프레임 생성
# 6개 과목의 점수, 5점만점 ()= 5점척도)
s1 <- c(1, 2, 1, 2, 3, 4, 2, 3, 4, 5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4)
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2)
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1)
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3)
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2)
name <- 1:10

subject <- data.frame(s1, s2, s3, s4, s5, s6)
subject
str(subject)
#==============================================
pc <- prcomp(subject)   # 주성분 분석 수행 함수
summary(pc)
plot(pc)
#==============================================
# 고유값과 고유벡터 
en <- eigen(cor(subject))   # 고유값 추출 (통계와 거리가 멀어짐)
en$values
en$vectors
plot(en$values, type = "o")

cor(subject)   # 상관계수 / 상관관계 분석 (절대값 자체 비교값이 낮아질 시 상관관계도가 떨어진다)

result <- factanal(subject, factors = 2, rotation = "varimax")   # 요인분석
result
# 요인분석 결과 p-value가 0.05 보다 작을 경우 요인수를 증가 후 재분석
# p-value가 0.023이 나오기 때문에 요인 수가 부족하다는 의미, 팩터를 1개 더 늘려서 분석 진행

result <- factanal(subject, factors = 3, rotation = "varimax", scores = "regression")   # 요인분석
result
result$loadings
print(result, digits = 2, cutoff = 0.5)
result$scores
plot(result$scores[ , c(1:2)])
text(result$scores[ , 1], result$scores[, 2], labels = name, cex = 0.7, pos = 3, col = "blue")

points(result$loadings[, c(1:2)], pch = 19, col = "red")
text(result$loading[, 1], result$loading[, 2], labels = rownames(result$loadings), cex = 0.8, pos = 3, col = "red")

install.packages("scatterplot3d")
library(scatterplot3d)
result
result$scores
Factor1 <- result$scores[, 1]
Factor2 <- result$scores[, 2]
Factor3 <- result$scores[, 3]
d3 <- scatterplot3d(Factor1, Factor2, Factor3, type = "p")

loadings1 <- result$loadings[, 1]
loadings2 <- result$loadings[, 2]
loadings3 <- result$loadings[, 3]
d3$points3d(loadings1, loadings2, loadings3, bg = "red", pch = 21, cex = 2, type = "h")


# 요인분석(요인제거, spss 데이터 활용)
install.packages("memisc")
library(memisc)

data.spss <- as.data.set(spss.system.file("drinking_water.sav"))
data.spss[1:11]   # 1번 ~ 11번 문항 데이터셋

drinking_water <- data.spss[1:11]
drinking_water_df <- as.data.frame(drinking_water)
drinking_water_df

result2 <- factanal(drinking_water_df, factors = 3, rotation = "varimax")   # 요인분석, 요소 3개
result2

dw_df <- drinking_water_df[-4]
str(dw_df)
dim(dw_df)
s <- data.frame(dw_df$Q8, dw_df$Q9, dw_df$Q10, dw_df$Q11)
c <- data.frame(dw_df$Q1, dw_df$Q2, dw_df$Q3)
p <- data.frame(dw_df$Q5, dw_df$Q6, dw_df$Q7)
satisfaction <- round((s$dw_df.Q8 + s$dw_df.Q9 + s$dw_df.Q10 + s$dw_df.Q11) / ncol(s), 2)
closeness <- round((c$dw_df.Q1 + c$dw_df.Q2 + c$dw_df.Q3) / ncol(c), 2)
pertinence <- round((p$dw_df.Q5 + p$dw_df.Q6 + p$dw_df.Q7) / ncol(p), 2)

satisfaction
closeness
pertinence

drinking_water_factor_df <- data.frame(satisfaction, closeness, pertinence)
colnames(drinking_water_factor_df) <- c("제품만족도", "제품친밀도", "제품적절성")
cor(drinking_water_factor_df)



# ▣ 상관분석
product <- read.csv("product.csv", header = T, fileEncoding = "euc-kr")
product

cor(product$제품_친밀도, product$제품_적절성)   # 바로 상관계수가 나오고, 0.7 이상 정도가 높은 측에 속한다

cor(product, method = "pearson") # 두 항목의 상관계수가 아닌 전체 변수 간의 상관관계 분석

install.packages("corrgram")
library(corrgram)
corrgram(product)
corrgram(product, upper.panel = panel.conf)
corrgram(product, lower.panel = panel.conf)


install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(product, hist = , pch = "+")



# ▣ 과제 연습풀이(p.472)
data.spss <- as.data.set(spss.system.file("drinking_water_example.sav"))
drinking_water_exam <- data.spss[1:7]
drinking_water_exam_df <- as.data.frame(drinking_water_exam)
str(drinking_water_exam_df)
drinking_water_exam_df

result3 <- factanal(drinking_water_exam_df, factor = 2, rotation = "varimax")
result3
head(drinking_water_exam_df)

s <- data.frame(drinking_water_exam_df$Q1, drinking_water_exam_df$Q2, drinking_water_exam_df$Q3)
s1 <- data.frame(drinking_water_exam_df$Q4, drinking_water_exam_df$Q5, drinking_water_exam_df$Q6, drinking_water_exam_df$Q7)
s
s1

colnames(s) <- c("제품친밀도")
colnames(s1) <- c("제품만족도")

drinking_water_exam_df <- data.frame(s, s1)
colnames(drinking_water_exam_df) <- c("제품친밀도", "제품만족도")
cor(drinking_water_exam_df)

satis <- round((s$drinking_water_exam_df.Q1 + s$drinking_water_exam_df.Q2 + s$drinking_water_exam_df.Q3) / ncol(s), 2)
satis2 <- round((s1$drinking_water_exam_df.Q4 + s1$drinking_water_exam_df.Q5 + s1$drinking_water_exam_df.Q6 + s1$drinking_water_exam_df.Q7) / ncol(s1), 2)
satis
satis2

drinking_water_exam_df <- data.frame(satis, satis2)
colnames(drinking_water_exam_df) <- c("제품친밀도", "제품만족도")
cor(drinking_water_exam_df)
drinking_water_exam_df

result3$loadings
print(result3, digits = 2, cutoff = 0.5)

plot(result3$scores[, c(1:2)])
