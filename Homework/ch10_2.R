library(dplyr)
library(ggplot2)

# 데이터 전처리
subway <- read.csv("subway_2005.csv", stringsAsFactors = FALSE)
subway <- subway %>%
  rename(date="사용일자", line="노선명", station="역명",
         on_psg="승차총승객수", off_psg="하차총승객수") %>%
  select(-"등록일자")

summary(subway)
table(subway$line)
table(subway$station)

subway$day <- as.integer(substr(subway$date,7,8))
subway$line <- ifelse(subway$line=="9호선2~3단계","9호선",subway$line)
subway$tot_psg <- subway$on_psg + subway$off_psg

# 전체 하루 평균 승하차 승객 수
subway %>%
  summarise(on_avg = mean(on_psg), off_avg = mean(off_psg))

# 승차 승객 수가 가장 많았던 역
subway %>%
  filter(on_psg==max(on_psg)) %>%
  select(date, line, station, on_psg)

# 역별 하루 평균 전체 승객 수
passenger10 <- subway %>%
  group_by(station) %>%
  summarise(tot_avg=mean(tot_psg)) %>%
  arrange(desc(tot_avg)) %>%
  head(10)

head(passenger10,3)

# 역별 하루 평균 전체 승객 수 상위 10개 역 그래프
ggplot(data=passenger10, aes(x=reorder(station, tot_avg), y=tot_avg)) +
  geom_col() +
  coord_flip()

# 날짜별 전체 승객 분석
subway %>%
  group_by(date) %>%
  summarise(total = sum(tot_psg)) %>%
  arrange(desc(total)) %>%
  head(3)

# 1호선 하루 평균 전체 승객 수 가장 많았던 역
subway %>%
  filter(line=="1호선") %>%
  filter(tot_psg==max(tot_psg)) %>%
  select(station, date, on_psg, off_psg, tot_psg)

# 주중과 휴일의 전체 승객 수 비교
subway$week <- ifelse(subway$day %in% c(2,3,5,9,10,16,17,23,24,30,31),
                      "weekend", "weekday")
t.test(data=subway, tot_psg~week)


# 노선별 전체 승객 비율 비교
line_pct <- subway %>%
  group_by(line) %>%
  summarise(total=sum(tot_psg)) %>%
  mutate(all=sum(total), pct=round(total/all*100,1))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)

# 노선별 전체 승객 비율 막대그래프
line_pct10 <- line_pct %>%
  filter(line %in% c("1호선","2호선","3호선","4호선","5호선","6호선",
                     "7호선","8호선","9호선","분당선"))

ggplot(data=line_pct10, aes(x=reorder(line, pct), y=pct)) +
  geom_col() +
  coord_flip() +
  xlab("노선") +
  ylab("이용비율") +
  ggtitle("서울지하철 노선 이용비율")

# 날짜별 전체 승객 선 그래프
day_graph <- subway %>%
  group_by(day) %>%
  summarise(total = sum(tot_psg))

ggplot(day_graph, aes(x=day, y=total, group=1)) +
  geom_line() +
  ggtitle("수도권 지하철 요일별 이용승객수") +
  xlab("요일") +
  ylab("이용승객")

#### 연습문제 ####

# 1
subway %>%
  filter(line=="4호선"&station=="혜화") %>%
  summarise(on_m=mean(on_psg), of_m=mean(off_psg), tot_m=mean(tot_psg))

# 2
subway %>%
  filter(date==20200505&station=="잠실(송파구청)") %>%
  select(line, station, on_psg, off_psg, tot_psg)

# 3
seoulstation <- subway %>%
  filter(station=="서울역" & line %in% c("1호선","4호선"))

t.test(data=seoulstation, tot_psg~line)

# 4
subway$period <- ifelse(subway$day<=10, "first",
                        ifelse(subway$day<=20, "second", "third"))
period_psg <- subway %>%
  group_by(period) %>%
  summarise(avg=mean(tot_psg))

period_psg %>% arrange(desc(avg))

# 5
ggplot(data=period_psg, aes(x=period, y=avg)) +
  geom_col() +
  scale_x_discrete(limits=c("first", "second", "third"))
