# ========== 삼성전자 종가 (2022.1.3 ~ 2022.9.30) 를 이용, 신경망 구성, 평가 및 특정구간 예측 ==========

# 파일 불러오기
library(nnet)
getwd()
ssdata <- read.csv(file.choose(), header = T, fileEncoding = "euc-kr")
str(ssdata)

# 필요한 컬럼 추출
df <- data.frame(일자 = ssdata$일자, 종가 = ssdata$종가)
df

# 일자 및 종가 내림차순으로 정렬
df <- df[order(df$일자), ]
df

n <- nrow(df)
n
rownames(df) <- c(1:n)
head(df)

# 데이터 값 정규화
pnorm <- ((df$종가 - min(df$종가)) / (max(df$종가) - min(df$종가))) * 0.9 + 0.05
pnorm

df <- cbind(df, 종가norm = pnorm)
head(df)

n80 <- round(n * 0.8, 0)                # 마지막 80% 인덱스 위치
n80
df.learning <- df[1:n80, ]
df.learning
df.test <- df[n80 + 1 : n, ]
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

in_learning <- getDataSet(df.learning$종가norm, 1, 142, INPUT_NODES)         # 입력데이터셋
head(in_learning)
tail(in_learning)

out_learning <- getDataSet(df.learning$종가norm, 11, 147, OUTPUT_NODES)      # 정답데이터셋
head(out_learning)
tail(out_learning)

# 신경망 구성
model <- nnet(in_learning, out_learning, size = HIDDEN_NODES, maxit = 100)

in_test <- getDataSet(df.test$종가norm, 1, 37, INPUT_NODES)                # TEST 입력 데이터셋
head(in_test)

# 생성된 신경망에 테스트데이터를 입력해서 예측값을 추출
predicted_values <- predict(model, in_test, type = "raw")
predicted_values

# 데이터 복원 후 비교
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted

Vreal <- getDataSet(df.test$종가, 11, 24, OUTPUT_NODES)                     # 역변환 필요 없음
Vreal

# 일부 데이터 예측
in_forecasting <- df$종가norm[91:100]
head(in_forecasting)

predicted_values <- predict(model, in_forecasting, type = "raw")
Vpredicted <- (predicted_values - 0.05) / 0.9 * (max(df$종가) - min(df$종가)) + min(df$종가)
Vpredicted
df$종가[101:105]

par(mfrow = c(1, 1))
plot(71:100, df$종가[71:100], xlim = c(71, 105), ylim = c(10000, 70000), type = "o")      # 관측값 시각화
lines(101:105, Vpredicted, type = "o", col = "red")                                   # 예측값 시각화
abline(v = 100, col = "blue", lty = 2)      # 수직선 그리기