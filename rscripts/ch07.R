# r 은 헤리티지 때문에 배운다.
# EDA 탐색적 자료분석. 정제하고, 시각화 하고
# 탐색은 안된다.
# 기술적으로만 경향성을 파악해서 분석. 눈으로 봐야하기 때문에 시각화를 잘해야함
# 정제되지 않은 데이터는 데이터 자체로 가치가 없다.

getwd() # 항상 어디 디렉토리에서 진행하는지 확인

# 헤더는 탭 사이즈 규격이다.
dataset <- read.csv("./data/Part-II/dataset.csv", header=T) 

dataset
View(dataset)

# 앞 뒤 데이터를 보는 이유 
# 일반적으로 초기 데이터일 확률이 높은 head
# 앞에 비어있고 뒤쪽 데이터가 꽉 차있다면 
# 뒤쪽은 양질의 데이터일 확률이 높음
head(dataset)
tail(dataset)

# EDA 는 데이터의 경향성을 보는 것이다.
# 데이터를 직접 보는 것은 도메인 날리지가 있을때
# p.196 표 7.1 에 내용은 리드미에 있어야 한다.

names(dataset)
attributes(dataset)

str(dataset) # 데이터의 자료구조를 확인

# p.197 컬럼값 접근
dataset$age # 데이터 셋에서 age 변수값 출력
length(dataset$age) # age 데이터 수 확인

dataset$resident

x <- dataset$age
str(x) # 연속된 데이터가 들어있는 배열

plot(dataset$price) # 그래프 표시

dataset["gender"] # python의 딕셔너리다. 칼럼명을 이용해 특정 변수 조회
dataset[2]

dataset[0] # 접근 불가능 R 은 1부터 시작 이렇게 안씀. 키 값으로 접근

# p.199
dataset[c("job", "price")] # c 는 리스트를 만드는 명령어
# job, price 열 조회


# p.200 결측치는 없는 값 또는 모르는 값
# 분석자가 데이터를 코딩하는 과정에서 실수로 입력하지 않거나
# 응답자가 응답을 피한 경우

summary(dataset) # 아무 도움이 안됨. 읽을 수가 없음.
# 여기서 중요한것 : na 값을 지워야함

# 결측치 제거 방법 p.201~202
sum(dataset$price) # NA가 나옴
sum(dataset$price, na.rm = T) # na.rm = T 속성을 이용해 결측치 제거 

price2 <- na.omit(dataset$price) # 뭔지 모르겠다.
price2
length(price2) # 결측치 30개 제거

# 배열과 리스트의 차이는 뭔가요?
# JS 에서 오브젝트와 MAP의 차이는?



# p.391
getwd()
data <- read.csv("./data/Part-III/cleanDescriptive.csv")
data

head(data)


x <- data$level2
y <- data$pass2


install.packages("gmodels")

library(gmodels)
CrossTable(x,y, chisq=TRUE)




























