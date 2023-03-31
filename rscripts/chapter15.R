# Chapter 15

# 벡터 : 선형 데이터 컬럼을 만들때는 벡터를 써야한다.
# 매트릭스
# 테이블 : 매트릭스와 테이블의 차이는 헤드 유무 차이
# 데이터 프레임 : 테이블이 모여있는데 데이터를 들여다 볼수 있는 메서드를 제공한다.

# 데이터 가져오기전처리
# 독립변수 종속변수 생성/ 모델 생성
# 잔차 확인
# 평가 지표 확인 ex.) RMSE


# 실습 1 : 단순 선형 회귀분석 수행
# 단계 1: 데이터 가져오기 (전처리 NA) multibyte 한국 중국 일본 한자권 언어
product <- read.csv("./data/Part-3/product.csv",
                    header = TRUE ,
                    fileEncoding = 'euc-kr')
str(product)
View(product)

# 단계 2: 독립변수와 종속변수 생성
y = product$제품_만족도 # 종속변수
x = product$제품_적절성 # 독립변수
df <- data.frame(x, y)  # 
View(df)

# 단계 3: 단순 선형회귀 "모델" 생성 선형회귀 분석은 stats 패키지에서 제공하는 lm() 함수 사용
result.lm <- lm(formula = y ~ x, data = df)

# 단계 4: 회귀분석의 절편과 기울기
result.lm  # intercept : y절편 0.7789  / x : 기울기  0.7393
View(result.lm)

# 단계 5: 모델의 잔차 보기 
residuals(result.lm)[1:2] # -0.7359630 -0.9966869 

# 단계 5-6: 모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.7359630 + 3.735963   # 3 


# 실습 1-1  : 선형 회귀분석 모델 시각화
# 단계 1: x, y 산점도 그리기 
plot(formula = y ~ x, data = product)

View(product)

# 단계 2: 선형 회귀모델 생성
result.lm <- lm(formula = y ~ x, data = product)
#result.lm <- lm(formula = y ~ x, data = df)

# 단계 3: 회귀선
abline(result.lm, col = "red")

# r-squared : 데이터 중 58% 정도의 확률로는 설명이 가능 (설명력)
# p-value : 유의값. 유의값은 0.05 보다 작으면 관계가 있다고 볼수 있음

# 단계 4: 선형 회귀분석 결과보기 
summary(result.lm)


# ========== 로지스틱스 회귀

# 실습: 날씨 관련 요인 변수로 비(rain) 유뮤 예측
# 단계 1: 데이터 가져오기 
weather = read.csv("C:/Rwork/Part-IV/weather.csv", stringsAsFactors = F)
dim(weather)
head(weather)
str(weather)

# 단계 2: 변수 선택과 더비 벼수 생성
weather_df <- weather[ , c(-1, -6, -8, -14)]
str(weather_df)

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# 단계 3: 학습데이터와 검정데이터 생성(7:3 비율)
idx <- sample(1:nrow(weather_df), nrow(weather_df) * 0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]

# 단계 4: 로지스틱 회귀모델 생성
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
weather_model
summary(weather_model)


# 단계 5: 로지스틱 회귀모델 예측치 생성
pred <- predict(weather_model, newdata = test, type = "response")
pred

# 시그모이드 함수
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)


# 단계 6: 모델 평가 - 분류정확도 계산
table(result_pred, test$RainTomorrow)

# 단계 7: ROC Curve를 이용한 모델 평가
install.packages("ROCR")
library(ROCR)

pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.maeasure = "fpr")
plot(prf)



# 실습: 의사결정 트리 생성: ctree() 함수 이용 
# 단계 1: party 패키지 설치 
install.packages("party")
library(party)

# 단계 2: airquality 데이터 셋 로딩
#install.packages("datasets")
library(datasets)

# 샘플예제
str(airquality)
View(airquality)

# 단계 3: formula 생성
formula <- Temp ~ Solar.R + Wind + Ozone

# 단계 4: 분류모델 생성 - formula를 이용하여 분류모델 생성
air_ctree <- ctree(formula, data = airquality)
air_ctree

# 단계 5: 분류분석 결과
plot(air_ctree)


# =======디시젼 트리 결측치 제거X
# markdown 보고서 작성

# 실습: 학습데이터와 검정데이터 샘플링으로 분류분석 수행
# 단계 1: 학습데이터와 검정데이터 샘플링
set.seed(42)
idx <- sample(1:nrow(iris), nrow(iris) * 0.7) # 7 : 3 데이터 분류
View(idx)
train <- iris[idx, ] # 학습데이터
test <- iris[-idx, ] # 검정데이터
View(train)

# 단계 2: formula(공식) 생성
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 단계 3: 학습데이터 이용 분류모델 생성
iris_ctree <- ctree(formula, data = train)
iris_ctree

# 단계 4-1: 간단한 형식으로 시각화 
plot(iris_ctree, type = "simple")

# 단계 4-2: 의사결정 트리로 플로팅
plot(iris_ctree)


# 단계 5: 분류모델 평가
# 단계 5-1: 모델의 예측치 생성과 혼돈 매트릭스 생성
pred <- predict(iris_ctree, test)

table(pred, test$Species)

# 단계 5-2: 분류 정확도 - 96%
(18 + 13 + 13) / nrow(test)


# 실습: K겹 교차 검정 샘플링으로 분류 분석하기 
# 단계 1: K겹 교차 검정을 위한 샘플링 - 3겹, 2회 반복
library(cvTools)
cross <- cvFolds(nrow(iris), K = 3, R = 2)

# 단계 2: K겹 교차 검정 데잍 보기 
str(cross)
cross
length(cross$which)
dim(cross$subsets)
table(cross$which)


# 단계 3: K겹 교차 검정 수행
R = 1:2
K = 1:3
CNT = 0
ACC <- numeric()

for(r in R) {
  cat('\n R = ', r, '\n')
  for(k in K) {
    
    datas_ids <- cross$subsets[cross$which == k, r]
    test <- iris[datas_ids, ]
    cat('test : ', nrow(test), '\n')
    
    formual <- Species ~ .
    train <- iris[-datas_ids, ]
    cat('train : ', nrow(train), '\n')
    
    model <- ctree(Species ~ ., data = train)
    pred <- predict(model, test)
    t <- table(pred, test$Species)
    print(t)
    
    CNT <- CNT + 1
    ACC[CNT] <- (t[1, 1] + t[2, 2] + t[3, 3]) / sum(t)
  }
  
}

CNT


# 단계 4: 교차 검정 모델 평가
ACC
length(ACC)

result_acc <- mean(ACC, na.rm = T)
result_acc


## =====민감한 데이터에 대해
## 나무를 여러개 심자? 나무 여러개에 데이터를 조금씩 나눠서 주자.
## 여러 분류기들의 값중 더 많은 값을 채택

# 실습: 랜덤 포레스트 기본 모델 생성
# 단계 1: 패키지 설치 및 데이터 셋 가져오기 
install.packages("randomForest")
library(randomForest)
data(iris)
View(iris)
# 단계 2: 랜덤 포레스트 모델 생성
model <- randomForest(Species ~ ., data = iris)
model
plot(model)


# 실습: 파라미터 조정 - 트리 개수 300개, 변수 개수 4개 지정  
# 트리 갯수랑 상관없이 알아서 분산 계산해줌
model2 <- randomForest(Species ~ ., data = iris,
                       ntree = 300, mtry = 4, na.action = na.omit)
model2
plot(model2)

# 실습: 중요 변수를 생성하여 랜덤 포레스트 모델 생성 
# 단계 1: 중요 변수로 랜덤 포레스트 모델 생성
model3 <- randomForest(Species ~ ., data = iris,
                       importance = T, na.action = na.omit)

# 단계 2: 중요 변수 보기 
importance(model3)
# 단계 3: 중요 변수 시각화
varImpPlot(model3)


### 엔트포리(Entropy): 불확실성
x1 <- 0.5; x2 <- 0.5 
e1 <- -x1 * log2(x1) - x2 * log2(x2)
e1

x1 <- 0.7; x2 <- 0.3               
e2 <- -x1 * log2(x1) - x2 * log2(x2)
e2


# 실습: ;최적의 파라미터(ntree, mtry) 찾기 
# 단계 1: 속성값 생성
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n = ntree, m = mtry)
param

# 단계 2: 이중 for() 함수를 이용하여 모델 생성
for(i in param$n) {
  cat('ntree =', i, '\n')
  for(j in param$m) {
    cat('mtry =', j, '\n')
    model_iris <- randomForest(Species ~ ., data = iris,
                               ntree = i, mtry = j, na.action = na.omit)
    print(model_iris)
  }
}


# 실습: 다향 분류 xgboost 모델 생성
# 단계 1: 패키지 설치
install.packages("xgboost")
library(xgboost)

# 단계 2: y 변수 생성
iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
table(iris_label)
iris$label <- iris_label

# 전처리 X
# 단계 3: 데이터 셋 생성
idx <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[idx, ] 
test <- iris[-idx, ]

# 단계 4: matrix 객체 변환
train_mat <- as.matrix(train[-c(5:6)])
dim(train_mat)

train_lab <- train$label
length(train_lab)


# xgboost 는 내가 정한 매트릭스를 만들어야함
# 단계 5: xgb.DMatrix 객체 변환
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

# 단계 6: model 생성 - xgboost matrix 객체 이용
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2,
                     objective = "multi:softmax", 
                     num_class = 3,
                     verbose = 0)
xgb_model

# 단계 7: testset 생성
test_mat <- as.matrix(test[-c(5:6)])
dim(test_mat)
test_lab <- test$label
length(test_lab)

# 단계 8: model prediction
pred_iris <- predict(xgb_model, test_mat)
pred_iris

# 단계 9: confusion matrix
table(pred_iris, test_lab)

# 단계 10: 모델 성능평가1 - Accuracy
(19 + 13 + 12) / length(test_lab)

# 단계 11: model의 중요 변수(feature)와 영향력 보기 
importance_matrix <- xgb.importance(colnames(train_mat), 
                                    model = xgb_model)
importance_matrix

# 단계 12: 중요 변수 시각화 
xgb.plot.importance(importance_matrix)



# 실습: 간단한 인공신경망 모델 생성
# 단계 1: 패키지 설치 
install.packages("nnet")
library(nnet)

# 단계 2: 데이터 셋 생성
df = data.frame(    # 데이터프레임 생성 - 입력 변수(x)와 출력변수(y)
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
  )

str(df)

# 단계 3: 인공신경망 모델 생성
model_net = nnet(y ~ ., df, size = 1)

# 단계 4: 모델 결과 변수 보기 
model_net

# 단계 5: 가중치(weights)보기 
summary(model_net)

# 단계 6: 분류모델의 적합값 보기 
model_net$fitted.values

# 단계 7: 분류모델의 예측치 생성과 분류 정확도
p <- predict(model_net, df, type = "class")
table(p, df$y)



# 실습: iris 데이터 셋을이용한 인공신경망 모델 생성
# 단계 1: 데이터 셋 생성
data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
nrow(training)
nrow(testing)

# 단계 2: 인공신경망 모델(은닉층 1개와 은닉층 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3

# 단계 3: 가중치 네트워크 보기 - 은닉층 1개 신경망 모델 
summary(model_net_iris1)

# 단계 4:가중치 네트워크 보기 - 은닉층 3개 신경망 모델 
summary(model_net_iris3)


# 단계 5: 분류모델 평가 
table(predict(model_net_iris1, testing, type = "class"), testing$Species)

table(predict(model_net_iris3, testing, type = "class"), testing$Species)



# 실습: neuralnet 패키지를 이용한 인공신경망 모델 생성
# 단계 1: 패키지 설치 
install.packages("neuralnet")
library(neuralnet)

# 단계 2: 데이터 셋 생성
data("iris")
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)

# 단계 3: 수치형으로 칼럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3

training_iris$Species <- NULL
head(training_iris)

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3

testing_iris$Species <- NULL
head(testing_iris)


# 단계 4: 데이터 정규화
# 단계 4-1: 정규화 함수 정의 
normal <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 단계 4-2: 정규화 함수를 이용하여 학습데이터와/검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)


# 단계 5: 인공신경망 모델 생성 - 은닉 노드 1개
model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)


# 단계 6: 분류모델 성능 평가
# 단계 6-1: 모델의 예측치 생성 - compute() 함수 이용 
model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result

# 단계 6-2: 상관관계 분석 - 상관계수로 두 변수 간 선형관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 단계 7: 분류모델 성능 향상 - 은닉층 노드 2개 지정, backprop 속성 적용
# 단계 7-1: 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backprop", learningrate = 0.01)

# 단계 7-2: 분류모델 예측치 생성과 평가 
model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)


