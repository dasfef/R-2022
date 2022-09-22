# 1) quakes 데이터셋에서 depth / magnitude 가 동일한 패널에 지진의 발생지를 산점도로 시각화

quakes
depthgroup <- equal.count(quakes$depth, number = 3)
depthgroup

maggroup <- equal.count(quakes$mag, number = 2)
maggroup

xyplot(lat ~ long | depthgroup * maggroup, data = quakes)


# 2) iris 데이터셋의 Sepal.Length 와 Sepal.Width 와 관계를 나타내는 그래프를 그리고, 품종별로 다른 색상과 도형으로 표시(ggplot 함수 활용)
install.packages("ggplot2")
library(ggplot2)

iris
summary(iris)
str(iris)

p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = c(factor(Species)), shape = c(factor(Species))))
p + geom_point()


# 3) iris 품종별로 Petal.Length(x축) 와 Petal.Width(y축) 의 관계를 서로 다른 패널에 표시 (xyplot 함수 활용)
xyplot(Petal.Width ~ Petal.Length | factor(Species), data = iris, col = c("red", "blue"))

# ) SeatacWeather 데이터 셋에서 월별 최저, 최고 기온을 선 그래프로 플로팅
install.packages("latticeExtra")
library(latticeExtra)

SeatacWeather
head(SeatacWeather)
summary(SeatacWeather)

