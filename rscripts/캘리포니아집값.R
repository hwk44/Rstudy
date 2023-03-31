##### 많이 쓰이는 데이터들 
# 1. 통계청 출산 데이터 (시계열, 회귀)
# 2. ~ 집 값 데이터 
# 3. 손글씨(비정형 데이터) 분류 => 분류 (0 ~ 9 분류) / iris 데이터 분류 
# 4. Fashion mnist

# 캘리포니아 집값 데이터
#  자료 출처 
# https://www.kaggle.com/datasets/camnugent/california-housing-prices


# 가설
# 바다 근처라면 집값이 비쌀것이다
library(tidyverse)
library(reshape2)

house <- read.csv("./data/housing.csv",
                    header = TRUE ,
                    fileEncoding = 'euc-kr')
str(house)
View(house)
head(house)

summary(house)
colnames(house)

## 데이터 시각화(데이터 확인을 위한)
par(mfrow=c(2,5))

# library(ggplot)
ggplot(house)
ggplot(data = melt(house),
       mapping = aes(x= value)) +
         geom_histogram(bins=30) +
         facet_wrap(~variable,scale='free_x')


??ggplot

## 결측값 처리

house$mean_bedrooms = house$total_bedrooms / house$households
house$mean_rooms = house$total_rooms / house$households

drop = c('total_bedrooms', 'total_rooms') # 컬럼 드랍할거 저장
house = house[ , !(names(house) %in% drop)] # house 에서 드랍 데이터 빼고 가져오기 

head(house)
str(house)
summary(house)



## 전처리(상식을 사용해서 가정에 대한 데터를 별도로 분리)
categories = unique(house$ocean_proximity)
head(categories)
categories

cat_house = data.frame(ocean_proximity = house$ocean_proximity)
head(cat_house)
str(cat_house)
cat_house[1,]

for(cat in categories) {
  cat_house[,cat] = rep(0, times=nrow(cat_house))
}

for(i in 1:length(cat_house$ocean_proximity)){
  cat = as.character(cat_house$ocean_proximity[i])
  cat_house[,cat][i] = 1
}

head(cat_house)
View(cat_house)







