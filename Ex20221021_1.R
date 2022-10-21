# ======================================= 시계열 데이터 ==============================================

AirPassengers
ts.plot(AirPassengers)                              # 시계열 데이터 plot(time slot plot)
diff <- diff(AirPassengers)                         # 앞 데이터와 뒷 데이터간의 차이를 확인(차분 수행 : diff 활용)
diff
plot(diff)
log <- diff(log(AirPassengers))                     # log() 함수를 통해 매우 큰 값의 폭을 비슷한 수준으로 만들어준다
log
plot(log)                                           # 정상성 시계열에 부합하는 그래프의 모습으로 나타난다


# 분당 인터넷 사용량 데이터 예시
data(WWWusage)
WWWusage
ts.plot(WWWusage, type = "l", col = "red")

# 유럽 주식시장 관련 데이터 예시
data(EuStockMarkets)
dim(EuStockMarkets)
head(EuStockMarkets)
EuStock <- data.frame(EuStockMarkets)
head(EuStock)
plot(EuStock$DAX[1:1000], type = "l", col = "red")              # 독일 주식 선그래프

# 다중 시계열 자료 추세선 시각화
plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]))          # 분리해서 그래프를 시각화할 수 있음


# 시계열 데이터 요소 분해
data <- c(19, 20, 18, 14, 16, 17, 18, 18, 18, 19, 14, 16, 18, 18, 18, 17, 15, 14, 13, 13,       # 데이터 생성
          12, 12, 11, 10, 10, 13, 14, 17, 18, 18, 19, 15, 14, 13, 11, 10)
length(data)
tsdata <- ts(data, start = c(2016, 1), frequency = 12)            # data 변수의 값을 시계열 데이터로 객체 생성
tsdata
ts.plot(tsdata)
plot(stl(tsdata, "periodic"))



input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)
tsdata
acf(tsdata, col = "red")

plot(diff(tsdata, differences = 1))                               # 차분 시각화


# ===== 지수평활법 =====
# 전체 시계열 자료의 평균을 구하고, 최근 시계열에 더 큰 가중치를 적용하는 방법

data <- c(19, 20, 18, 14, 16, 17, 18, 18, 18, 19, 14, 16, 18, 18, 18, 17, 15, 14, 13, 13,       # 데이터 생성
          12, 12, 11, 10, 10, 13, 14, 17, 18, 18, 19, 15, 14, 13, 11, 10)
length(data)
tsdata <- ts(data, start = c(2016, 1), frequency = 12)

install.packages("TTR")
library(TTR)
par(mfrow = c(2, 2))
plot(SMA(tsdata, n = 1), main = "1년단위 이동평균법")
plot(SMA(tsdata, n = 2), main = "2년단위 이동평균법")
plot(SMA(tsdata, n = 3), main = "3년단위 이동평균법")




# ======================================= 종합주가지수 예측 ===========================================
# 한국거래소 접속 및 데이터 가져오기

library(nnet)
data <- read.csv(file.choose(), header = T, fileEncoding = "euc-kr")
head(data)

# 1) 파일 구조 확인 및 변형하고자 하는 데이터형으로 변형
data$종가 <- as.numeric()                                       # csv 파일 중 숫자형으로 되어있지 않을 수 있기에 숫자형 데이터로 변형
df <- data.frame(일자 = data$일자, 종가 = data$종가)
head(df)
str(df)

# 2) 데이터 정렬(오름차순, 내림차순 등)
df <- df[order(df$일자), ]                                      # order() 함수를 활용하여 일자 기준 오름차순으로 재정렬
head(df)

# 3) 행 번호 정렬
n <- nrow(df)                                                   # nrow() 함수로 행 개수 저장
n
rownames(df) <- c(1:n)                                          # rownames() 함수로 1 ~ n 까지로 재지정
head(df)

# 4) 데이터 값 정규화(데이터 값을 0과 1 사이의 값으로 바꾸어 너무 큰 차이가 나지 않도록 정제)
Pnorm <- ((df$종가 - min(df$종가)) / (max(df$종가) - min(df$종가))) * 0.9 + 0.05           # 정규화 수식(음수는 절대 나올 수 없음)
                                                                                # 신경망에 0이란 수를 넣지 않아야 하기 때문에 90% 값으로 축소하여 적용한다
                                                                                # 0 ~ 1 = 100% , 일반 정규화 수식에서 0.9를 곱한 것이(* 0.9) 90% 값으로 축소한 값
Pnorm

df <- cbind(df, 종가norm = Pnorm)
head(df)

n80 <- round(n * 0.8, 0)                        # 80% 마지막 위치 인덱스
n80
df.learning <- df[1:n80, ]                      # 학습데이터셋
df.learning
df.test <- df[n80 + 1:n, ]                        # 테스트데이터셋
df.test

# RNN model 생성
INPUT_NODES <- 10                               # 입력 레이어
HIDDEN_NODES <- 10                              # 히든 레이어(은닉레이어)
OUTPUT_NODES <- 5                               # 출력 레이어
ITERATION <- 100                                # 학습 회수

# 데이터셋 재구성 함수
getDataSet <- function(item, from, to, size) {  # from ~ to : df$종가norm, 1, 92, 10 = 1부터 92까지 10개씩)
    dataframe <- NULL
    to <- to - size + 1                         # input 의 마지막 데이터 첫번째 인덱스(83)를 만들기 위한 식 : to = 92 / size = 10 / to - size + 1 = 83
    for (i in from:to) {                        # size 분량 벡터 생성
        start <- i
        end <- start + size - 1
        temp <- item[c(start:end)]              # 행단위 데이터 배열
        dataframe <- rbind(dataframe, t(temp))  # 같은 구조로 만들어주어야 하기 때문에 t() 함수로 비틀어준다 = t() : 데이터프레임 처럼 형식을 만들어준다(단일 벡터가 아닌 데이터프레임)
    }
    return(dataframe)
}
                                                # df.test$종가norm, 11, 97, 5 : 결과 부분은 11번째부터 97번째까지 5개씩 묶는다

test01 <- getDataSet(df.learning$종가norm, 1, 11, INPUT_NODES)
test01

in_learning <- getDataSet(df.learning$종가norm, 1, 92, INPUT_NODES)         # 입력데이터셋
head(in_learning)
tail(in_learning)

out_learning <- getDataSet(df.learning$종가norm, 11, 97, OUTPUT_NODES)      # 정답데이터셋
head(out_learning)
tail(out_learning)


# 신경망 구성
model <- nnet(in_learning, out_learning, size = HIDDEN_NODES, maxit = 100)

# 테스트데이터셋
in_test <- getDataSet(df.test$종가norm, 1, 19, INPUT_NODES)                # TEST 입력 데이터셋
head(in_test)


# 생성된 신경망에 테스트데이터를 입력해서 예측값을 추출
predicted_values <- predict(model, in_test, type = "raw")
predicted_values


# 데이터 복원 후 비교
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted

Vreal <- getDataSet(df.test$종가, 11, 24, OUTPUT_NODES)                     # 역변환 필요 없음
Vreal

# 가장 많이 쓰이는 비교 방법 MAPE(절대값을 비교하지 않고 백분율로 환산해서 비교)
ERROR <- abs(Vreal - Vpredicted)
ERROR
MAPE <- rowMeans(ERROR / Vreal) * 100                                       # 백분율로 환산
MAPE            # 개별 행단위 평가
mean(MAPE)      # 전체 평가

# 일부 데이터 예측
in_forecasting <- df$종가norm[91:100]
head(in_forecasting)

predicted_values <- predict(model, in_forecasting, type = "raw")
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted
df$종가[101:105]

par(mfrow = c(1, 1))
plot(71:100, df$종가[71:100], xlim = c(71, 105), ylim = c(1000, 3300), type = "o")      # 관측값 시각화
lines(101:105, Vpredicted, type = "o", col = "red")                                   # 예측값 시각화
abline(v = 100, col = "blue", lty = 2)      # 수직선 그리기


# ========== 삼성전자 종가 (2022.1.3 ~ 2022.9.30) 를 이용, 신경망 구성, 평가 및 특정구간 예측 ==========

library(nnet)
getwd()
ssdata <- read.csv(file.choose(), header = T, fileEncoding = "euc-kr")
