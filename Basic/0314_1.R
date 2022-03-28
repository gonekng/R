library(dplyr)
library(ggplot2)
library(readxl)

who_disease <- read_xlsx("who_disease.xlsx")
glimpse(who_disease)

# scatter plot
ggplot(who_disease, aes(x=year, y=cases)) +
  geom_point()

ggplot(who_disease, aes(x=year, y=cases)) +
  geom_point(alpha=0.3) # 투명도

ggplot(who_disease, aes(x=year, y=cases)) +
  geom_point(alpha=0.3, colour="red") # 색상

ggplot(who_disease, aes(x=year, y=cases)) +
  geom_point(alpha=0.3, colour="red", size=10) # 크기

ggplot(who_disease, aes(x=year, y=cases, colour=region))
  geom_point() + # 그룹화

##### Univariate #####
# ---- Categorical ----
# pie chart
region_counts <- who_disease %>% 
  group_by(region) %>% 
  summarise(total_cases = sum(cases))

ggplot(region_counts, aes(x = 1, y = total_cases, fill = region)) +
  geom_col() + # x축에 임의의 값을 넣고 그래프 생성
  coord_polar(theta = "y") + # y를 기준으로 원형 좌표계 변환
  theme_void() + # 테마 배경 제거
  ggtitle("Proportion of Region") # 그래프 제목 추가

# waffle chart
install.packages("waffle")
library(waffle)

region_counts <- who_disease %>%
  group_by(region) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100)) # 비율변수 추가

case_counts <- region_counts$percent
names(case_counts) <- region_counts$region

waffle(case_counts) # 백분율 기준 와플 차트 생성

# ---- Quantitative ----
# 'AMR' 지역의 1980년 'pertussis' 데이터
amr_pertussis <- who_disease %>% 
  filter(   # filter data to our desired subset
    region == 'AMR', 
    year == 1980, 
    disease == 'pertussis', 
    cases > 0
  )

# Histogram
ggplot(amr_pertussis, aes(x = cases)) +
  geom_histogram(fill = "skyblue2",
                 color = "white") +
  theme_minimal()

# Kernel Denstity Plot
ggplot(amr_pertussis, aes(x = cases)) + 
  geom_density(fill = "skyblue2", 
               color = "white", bw = 1000) + 
  theme_minimal()

# 'AMR'과 'EUR'의 차이점 비교하는 그래프
amr_eur_pertussis <- who_disease %>% 
  filter(
    region %in% c('AMR', 'EUR'), 
    year == 1980, 
    disease == 'pertussis', 
    cases > 0
  )

ggplot(amr_eur_pertussis, aes(x = cases)) + 
  geom_density(aes(fill = region), # fill에 그룹을 지정할 경우 자동 그룹화
               color = "white", alpha = 0.5) + 
  theme_minimal()

# 문자일 경우 : 막대그래프, 파이차트, 와플차트 등
# 숫자일 경우 : 히스토그램, 밀도(Density) 차트 등