getwd()

# setwd("C:/Rwork/Part-II")

dataset <- read.csv("./data/Part-2/dataset.csv", header = T) 
# header =T : table 사이즈로 가져와라

head(dataset)
# 실습: price 변수의 데이터 정제와 시각화

# subset("subset할 데이터", 조건)
dataset2 <- subset(dataset, price >= 2 & price <= 8)

pos <- dataset2$position
cpos <- 6 - pos   # 읽기 힘든 컬럼을 
#보기 쉽게 직관적으로 표현 명목변수임

dataset2$position <- cpos
dataset2$position[dataset2$position == 1] <- '1급'
dataset2$position[dataset2$position == 2] <- '2급'
dataset2$position[dataset2$position == 3] <- '3급'
dataset2$position[dataset2$position == 4] <- '4급'
dataset2$position[dataset2$position == 5] <- '5급'

View(dataset2)

range(dataset2$resident, na.rm = T)
dataset2 <- subset(dataset2, !is.na(dataset2$resident))
head(dataset2)

dataset2$gender2[dataset2$gender == 1] <- '남자'
dataset2$gender2[dataset2$gender == 2] <- '여자'

pie(table(dataset2$gender2))
table(dataset2$gender2)
dataset2$gender2


View(dataset2)

############8장

data(quakes)

library(lattice)

# 카운트 갯수를 3개 구간으로 나눔
# 일반적으로 3이나 5구간으로 나눠준다
equal.count(quakes$depth, number=3, overlap=0)
??equal.count

depthgroup <- equal.count(quakes$depth,
                          number=3,
                          overlap=0)
magnitudegroup <- equal.count(quakes$mag,
                              number=2,
                              overlap=0)
## 조건식을 준다
xyplot(lat ~ long| magnitudegroup*depthgroup, data = quakes)

install.packages("latticeExtra")
library(latticeExtra)

xyplot(min.temp + max.temp ~ day| month,
       data = SeatacWeather)


# -----------------------------------------------------------------------------------
# 0324 수업내용
# 교차 분석은 무엇?
# p. 두개이상 "범주형 변수"  억지로 만들기 -> equal.count
# x 값에 의해 y가 변하면 됨. 확인해야함

# GOAL : P. 394 논문 / 보고서 교차 분할표 해석

getwd()
data <- read.csv("./data/Part-3/cleanDescriptive.csv",
                 header = TRUE,
                 fileEncoding = "euc-kr")
head(data)

View(data)

# 단계 2: 변수 리코딩
x <- data$level2
y <- data$pass2

# 단계 3: 데이터프레임 생성
result <- data.frame(Level = x, Pass = y)
dim(result)
View(result)

# result 데이트를 범주형 데이터로 만들어야한다. 아마 다 숫자로
# 


# 실습: 교차 분할표 작성
# 단계 1: 기본함수를 이용한 교차 분할표 작성
table(result)

# 단계 2: 교차 분할표 작성을 위한 패키지 설치
install.packages("gmodels")
library(gmodels)
install.packages("ggplot2")
library(ggplot2)



# 단계 3: 패키지를 이용한 교차 분할표 작성
CrossTable(x = diamonds$color, y = diamonds$cut)

# 실습: 패키지를 이용한 교차 분할표 작성: 부모의 학력수준과 자녀 대학 진학여부
# 부모님의 학력이 자녀의 대학 진학 성공을 결정하는가?
x <- data$level2
str(x)
y <- data$pass2
str(y)
x
y
CrossTable(x, y, chisq = TRUE)

# 검증. 내말이 맞다고 말할 수 있게 하는 것이 카이제곱이다.

# 실습: CrossTable() 함수를 이용한 카이제곱 검정
CrossTable(x = diamonds$cut, 
           y = diamonds$color, chisq = TRUE)

## 손실함수 의 p값이랑 비슷함 -- 머신러닝
## p 값(유의값) 은 0.05 이하여야 유의미한 데이터 이다.


------------------------------------------------------------------------------------------------------
data <- read.csv("./data/Part-3/homogenity.csv")
head(data)

data <- subset(data, !is.na(survey), c(method, survey)) # 결측치 빼고 메소드 서베이만

# 단계 2: 코딩 변경(변수 리코딩)
data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"
data

data$survey2[data$survey == 1] <- "1.매우만족"
data$survey2[data$survey == 2] <- "2.만족"
data$survey2[data$survey == 3] <- "3.보통"
data$survey2[data$survey == 4] <- "4.불만족"
data$survey2[data$survey == 5] <- "5.매우불만족"

data

# 단계 3: 교차 분할표 작성
table(data$method2, data$survey2) # 교차표 생성 table(행,렬)


# 단계 4: 동질성 검정 - 모든 특성치에 대한 추론검정

chisq.test(data$method2, data$survey2)

CrossTable(x=data$method2, y=data$survey2)

















