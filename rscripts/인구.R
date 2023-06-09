install.packages("tidyverse")
install.packages("ggplot2")


library(tidyverse)
library(ggpplot2)
library(readxl)

df <- read_excel("./data/시군구_출생_20230324104737.xlsx")

View(df)

# 월별 출생아수를 알고 싶음

# 원본 데이터 df를 건들지 말고 df2를 만들어서 넣어라
df2 <- df %>%
  filter(!is.na(시점)) %>% # 남녀 출생아수를 알고 싶은게 아니라서 시점이 NA인것은 삭제 
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월")) # 시점을 년도,월로 나눔

View(df2)

df2 %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  qplot(x=월, y=평균출생수, data=.)


#### 교수님 깃헙 내용
getwd()

# 월별 신생아 출생 평균을 구해보자.
## - 기간 : 1997년 1월 ~ 2021년 12월
## - 데이터 : 통계청
## - tidyverse를 사용한 데이터 분석

# 라이브러리 설치
install.packages("tidyverse")

# 1. 데이터 불러오기
library(tidyverse)
library(readxl)

birth_df <- read_excel("practice/01_시군구_성_월별_출생.xlsx")

# 1. 데이터 확인
birth_df %>% 
  dim()

View(birth_df)

colSums(is.na(birth_df))

birth_df$시점 %>% 
  head()

is.na(birth_df$시점) %>% 
  head()
!is.na(birth_df$시점) %>% 
  head()

# 2. 결측치(NA) 처리
birth_df %>%
  filter(!is.na(시점)) %>%
  head()

birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  head()

## 컬럼 정리
birth_df <- birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월"))

birth_df %>% 
  head()

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국))

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  arrange(평균출생수)

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  arrange(desc(평균출생수))

# 4. ggplot2를 사용한 시각화
birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  qplot(x = 월, y = 평균출생수, geom = "col", data = .) +
  labs(title = "월별 신생아 출생 평균", subtitle = "1997년 1월 ~ 2021년 12월") +
  theme_bw(base_size = 15)

# Q.부산지역 출생아수는?
birth_df <- read_excel("practice/01_시군구_성_월별_출생.xlsx")
birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  head()

busan_birth <- birth_df %>%
  fill(시점, .direction = 'downup') %>%
  select(시점, 항목, 부산광역시) %>%
  separate(시점, into = c("년도", "월")) %>%
  filter(항목 %in% "남자 (명)")

busan_birth %>%
  group_by(년도) %>%
  qplot(x = 년도, y = 부산광역시, geom = "col", data = .)











