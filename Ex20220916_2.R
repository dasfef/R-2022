# ◈ 고급 시각화 분석

getwd()
setwd("C:/bigdataR")
library(lattice)

install.packages("mlmRev")
library(mlmRev)
str(Chem97)
head(Chem97)

# 분포 그래프
histogram(~gcsescore, data = Chem97)
histogram(~gcsescore | factor(score), data = Chem97)


# 밀도 그래프
densityplot(~gcsescore | factor(score), data = Chem97, groups = gender, plot.points = F, auto.key = T)


# 막대 그래프
help(barchart)
data(VADeaths)
VADeaths
str(VADeaths)
summary(VADeaths)

dft <- as.data.frame.table(VADeaths)   # 데이터프레임을 table 구조로 변경
str(dft)
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1), origin = 0)   # layout : 4개의 패널을 1행에 나타내주는 역할 제공
                                                                         # origin : x축의 구간을 0부터 표시해주는 역할

# 점 그래프
dotplot(Var1 ~ Freq | Var2, data = dft)
dotplot(Var1 ~ Freq, data = dft, groups = Var2, type = "o", auto.key = list(space = "right", points = T, lines = T))


# 산점도 그래프
xyplot(Ozone ~ Wind, data = airquality)   # xyplot(y축 ~ x축)
xyplot(Ozone ~ Wind | factor(Month), data = airquality, layout = c(5, 1))

head(quakes)
xyplot(lat ~ long, data = quakes, pch = ".")   # pch : point character(점 모양)
tplot <- xyplot(lat ~ long, data = quakes, pch = ".")
tplot <- update(tplot, main = "1964년 이후 태평양에서 발생한 지진 위치")
tplot
print(tplot)

range(quakes$depth)
summary(quakes$depth)
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6
head(quakes)

xyplot(lat ~ long | factor(depth2), data = quakes, pch = ".")


# 두 개의 변수값 동일한 패널에 표현하기
head(airquality)
xyplot(Ozone + Solar.R ~ Wind | factor(Month), data = airquality, pch = "o", col = c("red", "blue"), layout = c(5, 1), auto.key = list(space = "right", points = T))


# 데이터 범주화
numgroup <- equal.count(1:150, number = 4, overlap = 0)
numgroup

depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup

xyplot(lat ~ long | depthgroup, data = quakes, pch = ".")   # 위 수작업으로 했던 범주화를 equal.count 함수를 통해 손쉽게 할 수 있다


# 진도 데이터와 depth 데이터를 동시에 그래프 작성해보기

range(quakes$mag)
maggroup <- equal.count(quakes$mag, number = 2, overlap = 0)
maggroup

xyplot(lat ~ long | maggroup, data = quakes, pch = ".")
