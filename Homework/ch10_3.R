install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)

# ---- 데이터 전처리 ----
mental <- read.spss("한국행정연구원_사회통합실태조사_데이터_2019.sav")
mental <- as.data.frame(mental)
mental <- mental %>%
  select(q32_2, q1_4, q32_1, q34_1, q52, d17, d1, d2, ara) %>%
  rename(suicide=q32_2, satisfaction=q1_4, loneliness=q32_1,
         family_belief=q34_1, wealth=q52, health=d17, sex=d1, age=d2, area=ara)

mental$suicide <- as.integer(mental$suicide)
mental$satisfaction <- as.integer(mental$satisfaction)-1
mental$loneliness <- as.integer(mental$loneliness)
mental$family_belief <- as.integer(mental$family_belief)
mental$wealth <- as.integer(mental$wealth)-1
mental$health <- as.integer(mental$health)
mental$age <- as.character(mental$age)
mental$sex <- as.character(mental$sex)
mental$area <- as.character(mental$area)
mental$age <- ifelse(mental$age == "19~29세", "20대",
                   ifelse(mental$age == "60~69세", "60대", mental$age))
summary(mental)

# ---- 데이터 분석 ----
# 빈도분석
mental %>%
  group_by(sex) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1))

mental %>%
  group_by(age) %>%
  summarise(freq=n()) %>%
  mutate(total=sum(freq), pct=round(freq/total*100,1))

# 교차분석
table(mental$sex, mental$age)
round(prop.table(table(mental$sex, mental$age),1)*100,1)

chisq.test(mental$sex, mental$age)

# 평균분석
mental %>%
  summarise(m1=mean(suicide), m2=mean(satisfaction), m3=mean(loneliness),
            m4=mean(family_belief), m5=mean(wealth), m6=mean(health))

# 연구문제 1: 삶의 만족도와 외로움이 자살충동에 미치는 영향
RA <- lm(data=mental, suicide~satisfaction+loneliness)
summary(RA)

# 연구문제 2: 삶의 만족도와 외로움의 상관관계
cor.test(mental$satisfaction, mental$loneliness)

# 연구문제 3: 가족신뢰도, 경제안정도, 건강상태가
#            삶의 만족도와 외로움에 미치는 영향
RA <- lm(data=mental, satisfaction~family_belief+wealth+health)
summary(RA)

RA <- lm(data=mental, loneliness~family_belief+wealth+health)
summary(RA)

# 연구문제 4: 성, 연령, 지역별 삶의 만족도 차이
t.test(data=mental, satisfaction~sex)

mental %>%
  group_by(age) %>%
  summarise(avg=mean(satisfaction)) %>%
  arrange(desc(avg))

area_safisfaction <- mental %>%
  group_by(area) %>%
  summarise(avg=mean(satisfaction)) %>%
  arrange(desc(avg))

ggplot(data=area_safisfaction, aes(x=reorder(area, avg), y=avg)) +
  geom_col() +
  ggtitle("지역별 만족도") +
  xlab("지역") +
  ylab("만족도") +
  coord_flip()


# ---- 연습문제 ----

# 1
mental %>%
  group_by(age, sex) %>%
  summarise(avg=mean(satisfaction)) %>%
  arrange(desc(avg)) %>%
  head(4)

# 2
mental %>%
  group_by(area, age, sex) %>%
  summarise(avg=mean(satisfaction)) %>%
  arrange(desc(avg)) %>%
  head(5)

# 3
age_3060 <- mental %>%
  filter(age %in% c("30대","60대"))
t.test(data=age_3060, satisfaction~age)  

# 4
RA <- lm(data=mental, suicide~family_belief+wealth+health)
summary(RA)
