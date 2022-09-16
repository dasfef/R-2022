# ★ 교차 검정 샘플링

name <- c("a","b","c","d","e","f")
score <- c(90, 85, 99, 75, 65, 88)
df <- data.frame(Name = name, Score = score)
df

install.packages("cvTools")
library(cvTools)

cross <- cvFolds(n = 6, K = 3, R = 1, type = "random")
cross
cross$subsets[cross$which == 1 , 1]   # 행 자리에 K == 1 인 값을 추출하라.

# 데이터 프레임의 관측치 적용

r = 1
K = 1:3
for (i in K) {
  cat(i, "\n")
}

for (i in K) {
  datas_idx <- cross$subsets[cross$which == i, r]
  cat(i, "\n")
}

for (i in K) {
  datas_idx <- cross$subsets[cross$which == i, r]
  cat(i, "검정데이터셋\n")
  print(df[datas_idx, ])
  cat(i, "훈련데이터셋\n")
  print(df[-datas_idx, ])
  
}
