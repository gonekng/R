library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 전처리
seoulair <- read_xlsx("서울대기오염_2019.xlsx")
seoulair <- seoulair %>%
  rename(date="날짜", district="측정소명", pm10="미세먼지", pm2.5="초미세먼지") %>%
  select(date, district, pm10, pm2.5)

table(seoulair$date)
table(seoulair$district)
summary(seoulair$pm10)
summary(seoulair$pm2.5)

seoulair <- seoulair %>%
  filter(date!="전체" & district!="평균")
seoulair$month <- as.numeric(substr(seoulair$date,6,7))
seoulair$day <- as.numeric(substr(seoulair$date,9,10))
seoulair$season <- ifelse(seoulair$month %in% c(3,4,5), "spring",
                          ifelse(seoulair$month %in% c(6,7,8), "summer",
                          ifelse(seoulair$month %in% c(9,10,11), "fall", "winter")))

# 연간 미세먼지 평균
mean(seoulair$pm10, na.rm = TRUE)

# 미세먼지가 가장 심했던 날짜
seoulair %>%
  filter(!is.na(pm10)) %>%
  filter(pm10 == max(pm10)) %>%
  select(date, district, pm10)

# 구별 미세먼지 평균 비교
seoulair %>%
  filter(!is.na(pm10)) %>%
  group_by(district) %>%
  summarise(avg=mean(pm10)) %>%
  arrange(avg) %>%
  head(5)

# 계절별 분석
seoulair %>%
  filter(!is.na(pm10)&!is.na(pm2.5)) %>%
  group_by(season) %>%
  summarise(avg1=mean(pm10), avg2=mean(pm2.5)) %>%
  arrange(avg1, avg2)

# 미세먼지 등급 분석
seoulair %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10<=30, "good",
                           ifelse(pm10<=80, "normal",
                                  ifelse(pm10<=150, "bad", "worst")))) %>%
  group_by(pm_grade) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  select(pm_grade, freq, pct) %>%
  arrange(desc(freq))

# 구별 미세먼지 등급 비교
seoulair %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10<=30, "good",
                           ifelse(pm10<=80, "normal",
                                  ifelse(pm10<=150, "bad", "worst")))) %>%
  group_by(district, pm_grade) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  filter(pm_grade=="good") %>%
  select(district, freq, pct) %>%
  arrange(desc(pct)) %>%
  head(5)

# 1년간 미세먼지 추이 그래프
ggplot(data=seoulair, aes(x=date, y=pm10)) +
  geom_line()

# 계절별 미세먼지 등급 비율 그래프
season_grade <- seoulair %>%
  filter(!is.na(pm10)) %>%
  mutate(pm_grade = ifelse(pm10<=30, "good",
                           ifelse(pm10<=80, "normal",
                                  ifelse(pm10<=150, "bad", "worst")))) %>%
  group_by(season, pm_grade) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  select(season, pm_grade, pct)

ggplot(data=season_grade, aes(x=season, y=pct, fill=pm_grade)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("spring","summer","fall","winter")) +
  scale_fill_discrete(limits=c("good", "normal", "bad", "worst")) +
  ggtitle("2019년 서울의 계절별 미세먼지 실태") +
  xlab("계절") + 
  ylab("등급별 비율")

#### 연습문제 ####

# 1
mean(seoulair$pm2.5, na.rm = TRUE)

# 2
seoulair %>%
  filter(!is.na(pm2.5)) %>%
  group_by(season) %>%
  filter(pm2.5==max(pm2.5)) %>%
  select(season, date, district, pm2.5)

# 3
district_pm2.5 <- seoulair %>%
  filter(!is.na(pm2.5)) %>%
  group_by(district) %>%
  summarise(avg = mean(pm2.5))

ggplot(data=district_pm2.5, aes(x=reorder(district, avg), y=avg)) +
  geom_col() +
  coord_flip()

# 4
seoulair %>%
  filter(!is.na(pm2.5)) %>%
  group_by(district) %>%
  summarise(avg = mean(pm2.5)) %>%
  filter(avg==max(avg)|avg==min(avg))

min_max_pm2.5 <- seoulair %>%
  filter(district %in% c("성북구","영등포구"))

t.test(data=min_max_pm2.5, pm2.5~district)
