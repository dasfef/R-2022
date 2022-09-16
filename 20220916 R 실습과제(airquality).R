airquality
table(airquality$Temp)
summary(airquality$Temp)

library(reshape2)
max(airquality$Temp)

apply(airquality, 2, max)

high_temp <- max(airquality$Temp)
# temp <- c[(airquality$Month == high_temp, airquality$Day == high_temp)]

p <- subset(airquality, airquality$Temp == 97)
p
p[c(5, 6)]   # 기온이 가장 높은 월, 일


# 6월달에 발생한 가장 강한 바람의 세기 추출
head(airquality)
a <- max(airquality$Wind, na.rm = T)
high_wind <- subset(airquality, airquality$Wind == 20.7)
high_wind
