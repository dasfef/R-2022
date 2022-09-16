airquality
str(airquality)
head(airquality)

# 1) 기온이 가장 높은 날은 언제인지 월, 일 추출
max.temp <- max(airquality$Temp)   # 가장 높은 기온 체크크
max.temp
airquality[airquality$Temp == max.temp, c(5,6,4) ]   # == subset(airquality, airquality$Temp == 97)

subset(airquality, airquality$Temp == max.temp)
subset(airquality, Temp == max.temp, select = c("Month", "Day", "Temp"))


# 2) 6월달에 발생한 가장 강한 바람의 세기 추출
max.wind <- max(airquality[airquality$Month == 6, c(3)])


# 3) 7월달의 평균 기온 추출
head(airquality)
mean(airquality[airquality$Month == 7 , c(4)])


# 4) NA를 제외한 5월의 평균 오존 농도 추출
mean(airquality[airquality$Month == 5, c(1)], na.rm = T)


# 5) 오존 농도가 100을 넘는 날은 며칠인지 추출
nrow(airquality[airquality$Ozone > 100 & !is.na(airquality$Ozone), ])

nrow(subset(airquality, Ozone > 100))
