library(lattice)
library(stringr)
head(quakes)
summary(quakes$depth)


# 진앙지 깊이 그룹화
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup
xyplot(lat ~ long | depthgroup, data = quakes)


# 지진강도 그룹화
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)
magnitudegroup
xyplot(lat ~ long | magnitudegroup, data = quakes)


# 깊이와 진도 합성 그룹화
xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes)

quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5] <- 'd5'
head(quakes)

quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.45] <- 'm2'
head(quakes)

convert <- transform(quakes, depth3 = factor(depth3), mag3 = factor(mag3))
str(convert)   # 2개의 열을 factor type 으로 변환

xyplot(lat ~ long | depth3 * mag3, data = convert, col = c("red", "blue"))


# 조건 그래프
coplot(lat ~ long | depth, data = quakes, overlap = 0.1)
coplot(lat ~ long | depth, data = quakes, number = 5, rows = 1, overlap = 0.1, col = "blue", bar.bg = c(num = "green"))


# 진도를 5개 구간으로 조건 그래프 생성
coplot(lat ~ long | mag, data = quakes, number = 5, rows = 1, overlap = 0.1, bar.bg = c(num = "red"))


# 3차원 산점도 그래프
cloud(depth ~ lat * long , data = quakes, screen = list(z = 45, x = -25))




# ★ ggplot2
install.packages("ggplot2")
library(ggplot2)


# ▣ quakes, iris, mpg, mtcars, diamonds <- 특징이 있는 data set이기 때문에 기억해두면 좋음


# mpg 데이터셋
help(mpg)
head(mpg)
summary(mpg)
str(mpg)

qplot(hwy, data = mpg, fill = drv, binwidth = 2, facets = .~drv)   # 열단위 표현
qplot(hwy, data = mpg, fill = drv, binwidth = 2, facets = drv~.)   # 행단위 표현

qplot(displ, hwy, data = mpg)   # 앞에 오는 값이 x축, 뒤의 값이 y 축
qplot(displ, hwy, data = mpg, color = drv, facets = .~drv)

help(mtcars)
str(mtcars)
qplot(wt, mpg, data = mtcars, size = qsec, color = factor(carb), shape = factor(cyl))

# diamonds 데이터셋
help(diamonds)
head(diamonds)
str(diamonds)

qplot(clarity, data = diamonds, geom = "bar", color = cut, fill = cut)   # 축 하나만 지정을 했을 때는 막대 그래프

str(mtcars)
qplot(wt, mpg, data = mtcars, size = qsec)   # 두 개의 비교대상이 있을 때는 산점도
qplot(wt, mpg, data = mtcars, size = factor(cyl), col = factor(carb))
qplot(wt, mpg, data = mtcars, size = factor(cyl))

qplot(wt,mpg, data = mtcars, size = qsec, color = factor(carb), shape = factor(cyl))

qplot(wt, mpg, data = mtcars, geom = c("point", "smooth"), color = factor(cyl))
qplot(wt, mpg, data = mtcars, geom = c("point", "line"), col = factor(cyl))


# ★ ggplot 그래프

ggplot(data = diamonds, aes(carat, price, col = cut))  # 축설정만 진행한 것

p <- ggplot(data = diamonds, aes(carat, price, col = cut))
p + geom_point()

# ggplot 과 qplot의 차이 : ggplot은 다른 설정값과 결합할 수 있다

p <- ggplot(mtcars, aes(mpg, wt, col = c(factor(cyl))))
p + geom_line()

p + geom_point() + geom_line()

p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "bar")   # 히스토그램 + 추가요인 합성
p + stat_bin(aes(fill = ..density..))   # stat_bin() 의 기본값은 geom = "bar"와 같음

p + stat_bin(aes(fill = cut), geom = "area")
p + stat_bin(aes(col = cut, size = ..density..), geom = "point")



library(UsingR)
help(galton)
sunflowerplot(galton)
str(galton)

p <- ggplot(data = galton, aes(x = parent, y = child))
p <- p + geom_count() + geom_smooth(method = lm)


# ggsave() 함수로 생성한 그래프 이미지로 저장하기
getwd()
setwd("C:/bigdataR")

p
ggsave(file = "diamonds.jpg", plot = p)


# ★ 지도 공간 기법 시각화
install.packages("ggmap")
library(ggmap)

# 서울 지역 지도 시각화
seoul <- c(left = 126.77, right = 127.17, bottom = 37.40, top = 37.70)
map <- get_stamenmap(seoul, zoom = 12, maptype = 'terrain')
ggmap(map)


# 대구를 중심으로 한 대한민국 지도 시각화
pop <- read.csv(file.choose(), header = T, fileEncoding = "euc-kr")
pop

region <- pop$지역명
lon <- pop$LON
lat <- pop$LAT
tot_pop <- as.numeric(str_replace_all(pop$총인구수, ',', ''))
tot_pop   # 총 인구수의 ,(comma)를 제외하고 다시 숫자로 만든 변수

df <- data.frame(region, lat, lon, tot_pop)
df
df <- df[1:17, ]
df

daegu <- c(left = 123.44, bottom = 32.85, right = 131.60, top = 38.87)
map <- get_stamenmap(daegu, zoom = 7, maptype = 'watercolor')
layer1 <- ggmap(map)
layer1

layer2 <- layer1 + geom_point(data = df, aes(x = lon, y = lat, col = factor(tot_pop), size = factor(tot_pop)))
layer2   # layer1(배경) 위에 layer2로 geom_point(산점도)를 그려 넣는다

layer3 <- layer2 + geom_text(data = df, aes(x = lon + 0.01, y = lat + 0.08, label = region), size = 3)
layer3
ggsave("pop.png", width = 10.24, height = 7.68)
