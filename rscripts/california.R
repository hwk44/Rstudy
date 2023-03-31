# 생산성과 협력을 위해 보고서를 써야함.
# 보고서는 이해가능해야함

# 회귀 => 값을 예측하는 것
## 단순 회귀

# 캘리포니아 집값 예측

## 과정
# 1. 데이터를 불러오기 확인 -> 시각적 확인
# 2-1. 전처리 과정 -> 결측치 처리
# 2-2. 후처리 과정 -> 표준화 정규화
# 3. 데이터를 분리 -> 학습 AND 검증
# 4. 학습을 통해 기울기와 절편을 구함
# 5. 검증 -> 모델을 검증

# 자료출처
# https://www.kaggle.com/datasets/camnugent/california-housing-prices

housing <- read.csv("./data/housing.csv",
                    fileEncoding = 'euc-kr')

head(housing)    # 자료 앞단 확인
colnames(housing)# 컬럼네임
str(housing)     # 자료구조 확인

# y 값 (집 값)의 대략적인 정보 확인
summary(housing) # 결측치 확인
library(tidyverse)
library(reshape2)


# 1. 데이터를 불러오기 -> 시각적 확인
ggplot(data = melt(housing), mapping = aes(x= value)) +
         geom_histogram(bins=30) +
         facet_wrap(~variable,scale='free_x')


# mean 평균 // median 중앙값

# 2-1 전처리
bedroom_mean <- mean(housing$total_bedrooms, na.rm=TRUE)
bedroom_median <- median(housing$total_bedrooms, na.rm=TRUE)
ggplot(housing, aes(x = total_bedrooms)) +
  geom_histogram(bins = 40, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "Mean"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "Median"), lwd = 1.5) +
  xlab("Total Bedrooms") +
  ylab("Frequency") +
  ggtitle("Histogram of Total Bedrooms (noncontinuous variable)") +
  scale_color_manual(name = "Summary Stats", labels = c("Mean", "Median"), values = c("red", "green"))

housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm=TRUE)
sum(is.na(housing))

# 2-2 후처리  Post - Cleaning

# 데이터를 정리 후 구조를 보면 factor 변수인 ocean_porximity 외에도
# 불연속적인(discrete) 값 int  나이 ,룸 갯수 베드룸 인구 => 히스토그램
# 연속적인 값 float => 산점도

## 데이터를 정리한 후 데이터셋의 구조를 보면 factor 변수인 
# `ocean_proximity` 외에도 9개의 numeric 변수가 있음을 알 수 있습니다. 
# 이 중 3개는 연속적(continuous)(`longitude`, `latitude`, `median_income`)이고
# 6개는 불연속적(discrete)(`housing_median_age`, `total_rooms`, `total_bedrooms`,
# `population`, `households`, `median_house_value`) 입니다..

housing$mean_bedrooms <- housing$total_bedrooms/housing$households
housing$mean_rooms <- housing$total_rooms/housing$households
head(housing)
summary(housing)


  
# 안쓰는 데이터 지워
  drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[ , !(names(housing) %in% drops)]
head(housing)

### 범주형 변수(1)

# 범주형 변수 처리를 위해서 별도의 데이터 프레임을 생성합니다.
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

#모든 값이 `0`으로 채워진 데이터 프레임을 생성합니다.
for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing)

#필요한 데이터만 `1`로 업데이트 합니다.

for(i in 1:length(cat_housing$ocean_proximity)){
  cat <- as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] <- 1
}

head(cat_housing)


#기존 특징은 사용하지 않기 때문에 삭제합니다.
cat_columns <- names(cat_housing)
keep_columns <- cat_columns[cat_columns != 'ocean_proximity']
cat_housing <- select(cat_housing,one_of(keep_columns))
tail(cat_housing)

### 수치형 변수 처리

# 수치의 단위(unit)이 일정하지 않기 때문에 수치형 변수를 
#일괄로 처리하도록 하겠습니다. 먼저 특징을 확인합니다.
colnames(housing)

#명목형 변수(`ocean_proximity`)와 예측 변수(`median_house_value`)는 대상에서 제외하도록 하겠습니다.
drops <- c('ocean_proximity','median_house_value')
housing_num <-  housing[ , !(names(housing) %in% drops)]
head(housing_num)

# 해당 데이터는 정규화가 아니라 '표준화'를 사용합니다.
scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)

### 정리된 데이터 결합
cleaned_housing <- cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)


# 3. 테이터를 분리 -> 학습과 검증
## 검증 데이터

#이번 단계에서는 전체 데이터에서 학습 데이터(`train`)와 
#검증 데이터(`test`)를 분리합니다. 
#검증 데이터는 학습된 모델의 평가에만 사용되며, 
#학습/검증 데이터 분리를 통해 예측 결과의 객관성을
#확보할 수 있습니다.

# 일반적으로 학습용 7 결과확인 3
# 여기선 8:2

set.seed(42)
sample <- sample.int(n = nrow(cleaned_housing), 
          size = floor(.8*nrow(cleaned_housing)), replace = F)
train <- cleaned_housing[sample, ] #just the samples
test  <- cleaned_housing[-sample, ] #everything but the samples
head(train)

# 분리된 데이터가 전체 데이터를 반영하고 있는지 확인합니다.
nrow(train) + nrow(test) == nrow(cleaned_housing)

#간단한 선형 모형 테스트를 위해 아래 3개 변수를
#선택하여 분석에 적용합니다. 3개 중 y에 가장 영향을 미치는거 확인 
#- 소득(중앙값) : `median_income`
#- 방 수(평균값) : `mean_rooms`
#- 인구 : `population`

# 또한, 모델의 과적합(`overfit`) 문제를 피하기 위해
# 과적합? 그럴 듯한 검증 데이터를 주면 합격. 아니면 불합격.
# 즉 과적합을 피해야함
# `cv.glm` 함수를 이용하여 교차 검증(k_fold)를 수행하며,
# 여기서는 모델 테스트에 전처리된 데이터 자체를 사용합니다.

glm_house = glm(median_house_value~median_income+mean_rooms+population,
                data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5) # 5번 돌려라
k_fold_cv_error$delta

??cv.glm
