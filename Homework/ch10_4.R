library(dplyr)
library(ggplot2)

# ---- 데이터 전처리 ----
foodshop <- read.csv("6110000_서울특별시_07_24_04_P_일반음식점.csv", na="")
str(foodshop)

foodshop <- foodshop %>%
  rename(open_date=인허가일자,
         status=상세영업상태명,
         close_date=폐업일자,
         name=사업장명,
         type=업태구분명,
         address=소재지전체주소) %>%
  select("name", "type", "status", "open_date", "close_date", "address")
foodshop$close_date <- as.integer(foodshop$close_date)

summary(is.na(foodshop))

table(foodshop$type)
table(foodshop$status)
range(foodshop$open_date, na.rm=TRUE)
range(foodshop$close_date, na.rm=TRUE)

foodshop$type <- ifelse(foodshop$type
        %in% c("까페","다방","라이브카페","커피숍"), "카페", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("통닭(치킨)","호프/통닭"), "치킨", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("일식","회집","횟집"), "회집", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("경양식","패밀리레스트랑"), "레스토랑", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("정종/대포집/소주방"), "소주방", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("외국음식전문점(인도,태국등)"), "외국음식전문점", foodshop$type)
foodshop$type <- ifelse(foodshop$type
        %in% c("기타", "193959.1505"), NA, foodshop$type)

foodshop$open_date <- ifelse(foodshop$open_date<19480815 |
        foodshop$open_date>20201231, NA, foodshop$open_date)
foodshop$open_year <- as.integer(substr(foodshop$open_date,1,4))
foodshop$close_date <- ifelse(foodshop$close_date<19480815 |
        foodshop$close_date>20201231, NA, foodshop$close_date)
foodshop$close_year <- as.integer(substr(foodshop$close_date,1,4))

foodshop$district <- substr(foodshop$address,7,9)
foodshop$district <- ifelse(foodshop$district
        %in% c("강남구","강동구","강북구","강서구","관악구","광진구","구로구",
               "금천구","노원구","도봉구","동대문","동작구","마포구","서대문",
               "서초구","성동구","성북구","송파구","양천구","영등포","용산구",
               "은평구","종로구","중구 ","중랑구"), foodshop$district, NA)

str(foodshop)

# ---- 데이터 분석 ----

# 오래된 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>%
  filter(open_date==min(open_date)) %>%
  select(name, type, open_date, address)

foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>%
  filter(type %in% c("분식","레스토랑","치킨","회집",
                     "중국식","카페","패스트푸드")) %>%
  group_by(type) %>%
  filter(open_date==min(open_date)) %>%
  arrange(open_date) %>%
  select(name, type, open_date, address)

# 개업, 영업, 폐업 현황
foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  group_by(type) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  arrange(desc(freq)) %>%
  head(10)

foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  filter(status=="영업") %>%
  group_by(type) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  arrange(desc(freq)) %>%
  head(5)

foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  group_by(status) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1))

foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  filter(type %in% c("한식","분식","레스토랑","치킨","회집","카페")) %>%
  group_by(type, status) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  filter(status=="영업") %>%
  arrange(desc(pct))

# 연도별 분석
foodshop %>%
  filter(!is.na(open_date) & !is.na(district)) %>%
  group_by(open_year) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq)) %>%
  head(5)

foodshop %>%
  filter(!is.na(close_date) & !is.na(district)) %>%
  group_by(close_year) %>%
  summarise(freq=n()) %>%
  arrange(desc(freq)) %>%
  head(5)

open_trend <- foodshop %>%
  filter(!is.na(open_date) & !is.na(district)) %>%
  group_by(open_year) %>%
  summarise(open_n = n())

ggplot(data=open_trend, aes(x=open_year, y=open_n)) +
  geom_col() +
  xlab("연도") +
  ylab("개업수")

close_trend <- foodshop %>%
  filter(!is.na(close_date) & !is.na(district)) %>%
  group_by(close_year) %>%
  summarise(close_n = n())

ggplot(data=close_trend, aes(x=close_year, y=close_n)) +
  geom_col() +
  xlab("연도") +
  ylab("개업수")

open_trend2 <- rename(open_trend, year=open_year)
close_trend2 <- rename(close_trend, year=close_year)
oc_trend <- left_join(open_trend2, close_trend2, by="year")

ggplot(data=oc_trend) +
  geom_line(aes(year, open_n), color="steelblue") +
  geom_line(aes(year, close_n), color="coral") +
  xlab("연도") +
  ylab("개업수")

oc_trend %>% filter(close_n > open_n)

# 지역별 분석
district_business <- foodshop %>%
  filter(!is.na(open_date) & !is.na(district) & status=="영업") %>%
  group_by(district) %>%
  summarise(freq=n())

district_business %>%
  arrange(desc(freq)) %>%
  head(5)

ggplot(data=district_business, aes(x=reorder(district, freq),freq)) +
  geom_col() +
  coord_flip() +
  xlab("구 이름") +
  ylab("영업 음식점 수")

foodshop %>%
  filter(!is.na(open_date) & !is.na(district)) %>%
  filter(type %in% c("한식","분식","치킨","레스토랑",
                     "회집","중국식","카페","패스트푸드")) %>%
  filter(status=="영업") %>%
  group_by(type, district) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  group_by(type) %>%
  filter(pct==max(pct))

# ---- 연습문제 ----

# 1
foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  filter(district=="종로구" & status=="영업") %>%
  group_by(type) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  arrange(desc(pct)) %>%
  head(5)

# 2
foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  filter(open_year==2020) %>%
  group_by(status) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1))

# 3
foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  filter(open_year==2020) %>%
  group_by(type) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  arrange(desc(pct)) %>%
  head(5)

# 4
foodshop$open_month <- as.integer(substr(foodshop$open_date,5,6))
monthly_open <- foodshop %>%
  filter(!is.na(open_month)) %>%
  group_by(open_month) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1)) %>%
  arrange(desc(freq))
monthly_open

# 5
ggplot(data=monthly_open, aes(x=open_month, y=freq)) +
  geom_line() +
  xlab("월") +
  ylab("개업 음식점수") +
  scale_x_continuous(breaks=c(2,4,6,8,10,12))

# 6
foodshop$open_season <-
  ifelse(foodshop$open_month %in% c(3,4,5), "spring",
     ifelse(foodshop$open_month %in% c(6,7,8), "summer",
        ifelse(foodshop$open_month %in% c(9,10,11), "autumn", "winter")))
seasonal_open <- foodshop %>%
  filter(!is.na(open_date) & !is.na(type) & !is.na(district)) %>%
  group_by(open_season) %>%
  summarise(freq=n())
seasonal_open %>% arrange(desc(freq))

ggplot(seasonal_open, aes(x=reorder(open_season,-freq),y=freq)) +
  geom_col()
