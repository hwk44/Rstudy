getwd()

setwd("C:/Rwork/Part-II")

dataset <- read.csv("./data/Part-II/dataset.csv", header = T) 
# header =T : table 사이즈로 가져와라

head(dataset)
# 실습: price 변수의 데이터 정제와 시각화

# subset("subset할 데이터", 조건)
dataset2 <- subset(dataset, price >= 2 & price <= 8)

pos <- dataset2$position
cpos <- 6 - pos   # 읽기 힘든 컬럼을 보기 쉽게 직관적으로 표현

dataset2$position <- cpos
dataset2$position[dataset2$position == 1] <- '1급'
dataset2$position[dataset2$position == 2] <- '2급'
dataset2$position[dataset2$position == 3] <- '3급'
dataset2$position[dataset2$position == 4] <- '4급'
dataset2$position[dataset2$position == 5] <- '5급'

View(dataset2)



length(dataset2$price)
str(dataset)


View(dataset2)


# 7-2 문제 resident 칼럼 na 값 제거 후 resident2 에 저장
resident2 <- na.omit(dataset2$resident)
# View(resident2) # 결과 확인


# 7-3 gender1 칼럼 추가 파이차트로 확인
dataset2$gender1[dataset2$gender == 1] <- '남자'
dataset2$gender1[dataset2$gender == 2] <- '여자'

View(dataset2)

# head(dataset2[c("gender","gender1")])

pie(table(dataset2$gender1))  # table을 안써줘서 오류 났었음


######### 교재 내용 age2컬럼 을 만드는...
# 나이를 나타내는 age 칼럼을 대상으로 코딩 변경하기 
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"
head(dataset2)
##########

# 7-4
#
dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30 & dataset2$age < 55] <- 2
dataset2$age3[dataset2$age >= 55] <- 3

dataset2$age3

dataset2 <- na.omit(dataset2)
summary(dataset2)



## 8-1

install.packages("lattice")
library(lattice)

head(quakes)
str(quakes)

depthgroup <- equal.count(quakes$depth, number = 3, overlap=0)
View(depthgroup) # View 함수로 잘 안보임

xyplot(lat ~ long | depthgroup, data = quakes,
       main="Fiji Earthquakes(depthgoup)",
       ylab="latitude", xlab="longitude",
       pch="@", col="red")

magnitudegroup <- equal.count(quakes$mag, number = 2, overlap =0)
magnitudegroup
View(magnitudegroup)# View 함수로 잘 안보임

# 단계 2: magnitudegroup 변수를 기준으로 산점도 그리기 
xyplot(lat ~ long | magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes(magnitude)",
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = "blue")



# 단계 3: 수심과 리히터 규모를 동시에 표현(3행 2열 패널 구현)
xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = c("red", "blue"),
       layout=c(2:3)
       )

install.packages("latticeExtra")
library(latticeExtra)
head(SeatacWeather)
View(SeatacWeather)
str(quakes)
