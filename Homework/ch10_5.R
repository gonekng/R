library(dplyr)
library(ggplot2)
library(foreign)
library(readxl)

# ---- 데이터 전처리 ----
koweps19 <- read.spss("koweps_hpwc14_2019_beta1.sav", to.data.frame = TRUE)
str(koweps19)

welfare19 <- koweps19 %>%
  select(h14_g3, h14_g4, h14_g6, h14_reg5, h14_eco9, h14_inc2, h14_inc3) %>%
  rename(sex=h14_g3, birth=h14_g4, edu=h14_g6, region=h14_reg5,
         job_code=h14_eco9, p_sal=h14_inc2, t_sal=h14_inc3)
summary(welfare19)

welfare19$sex <- ifelse(welfare19$sex==1,"male","female")
welfare19$age <- 2019 - welfare19$birth + 1

welfare19$edu_grade <- ifelse(welfare19$edu %in% c(1,2,3,4), "중학 이하",
                        ifelse(welfare19$edu==5, "고교",
                         ifelse(welfare19$edu==6, "전문대","대학 이상")))

region_name <- data.frame(region=c(1,2,3,4,5),
                          region1=c("서울","광역시","시","구","도농복합군"))
welfare19 <- left_join(welfare19, region_name, id="region")

job_name <- read_xlsx("2019년 14차 한국복지패널조사 조사설계서.xlsx", sheet=6)
welfare19 <- left_join(welfare19, job_name, id="job_code")

welfare19$p_sal <- ifelse(welfare19$p_sal==0, NA, welfare19$p_sal)
welfare19$t_sal <- ifelse(welfare19$t_sal==0, NA, welfare19$t_sal)

welfare19 <- welfare19 %>%
  select(sex, age, edu, edu_grade, region1, job, p_sal, t_sal)
str(welfare19)

# ---- 데이터 분석 ----

# 상용직과 일용직의 평균 총급여 비교
mean(welfare19$p_sal, na.rm=TRUE)
mean(welfare19$t_sal, na.rm=TRUE)

# 성별 평균 총급여 차이 검정
t.test(data=welfare19, p_sal~sex)
t.test(data=welfare19, t_sal~sex)

# 성별 최대 총급여 상용직 근로자
welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(sex) %>%
  filter(p_sal==max(p_sal)) %>%
  select(sex, age, edu, edu_grade, region1, job, p_sal)

# 연령별 상용직 평균 총급여 비교
age_sal <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(age) %>%
  summarise(avg=mean(p_sal))

age_sal %>%
  arrange(desc(avg)) %>%
  head(3)

ggplot(data=age_sal, aes(x=age, y=avg)) +
  geom_line() +
  xlab("연령") +
  ylab("총급여")

age_sal2 <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(age, sex) %>%
  summarise(avg=mean(p_sal))

ggplot(data=age_sal2, aes(x=age, y=avg, col=sex)) +
  geom_line() +
  xlab("연령") +
  ylab("총급여")

# 교육수준별 상용직 평균 총급여 비교
edu_sal <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(edu_grade) %>%
  summarise(avg=mean(p_sal))

edu_sal %>% arrange(desc(avg))
ggplot(data=edu_sal, aes(x=reorder(edu_grade, avg), y=avg)) +
  geom_col() +
  xlab("교육수준") +
  ylab("총급여")

edu_sal2 <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(edu_grade, sex) %>%
  summarise(avg=mean(p_sal))

edu_sal2 %>% arrange(desc(avg))
ggplot(data=edu_sal2, aes(x=edu_grade, y=avg, fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("중학 이하", "고교", "전문대", "대학 이상")) +
  xlab("교육수준") +
  ylab("총급여")

# 권역별 평균 총급여 비교
region_sal <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(region1) %>%
  summarise(avg=mean(p_sal))

region_sal %>% arrange(desc(avg))
ggplot(data=region_sal, aes(x=reorder(region1, avg), y=avg)) +
  geom_col() +
  xlab("권역") +
  ylab("총급여")

# 직종별 평균 총급여 비교
job_sal <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(job) %>%
  summarise(avg=mean(p_sal))

job_sal %>% arrange(desc(avg)) %>% head(10)
job_sal15 <- job_sal %>% arrange(desc(avg)) %>% head(15)
ggplot(data=job_sal15, aes(x=reorder(job, avg), y=avg)) +
  geom_col() +
  coord_flip() +
  xlab("직종") +
  ylab("총급여")

# ---- 연습문제 ----

# 1
welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(sex) %>%
  filter(age==max(age)) %>%
  select(sex, age, edu_grade, region1, job, p_sal)

welfare19 %>%
  filter(!is.na(t_sal)) %>%
  group_by(sex) %>%
  filter(age==max(age)) %>%
  select(sex, age, edu_grade, region1, job, t_sal)

# 2
welfare19$age_grade <- ifelse(welfare19$age<30, "young",
                              ifelse(welfare19$age<60, "middle","old"))
age_grade_sal <- welfare19 %>%
  filter(!is.na(p_sal)) %>%
  group_by(age_grade, sex) %>%
  summarise(avg=mean(p_sal))
age_grade_sal %>% arrange(desc(avg))

# 3
ggplot(age_grade_sal, aes(x=age_grade, y=avg, fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("young","middle","old"))

# 4
ind_sal <- read.spss("koweps_hpwc14_2019_beta1.sav", to.data.frame = TRUE)
ind_sal <- ind_sal %>%
  select(h14_eco8, h14_inc2) %>%
  rename(ind_code=h14_eco8, p_sal=h14_inc2)
ind_name <- read_xlsx("2019년 14차 한국복지패널조사 조사설계서.xlsx", sheet=4) %>%
  rename(ind_code=industry_code)
ind_sal <- left_join(ind_sal, ind_name, id="ind_code")

ind_sal_top10 <- ind_sal %>%
  filter(!is.na(p_sal)) %>%
  group_by(industry) %>%
  summarise(avg=mean(p_sal)) %>%
  arrange(desc(avg)) %>%
  head(10)
ind_sal_top10

# 5
ggplot(ind_sal_top10, aes(x=reorder(industry, avg), y=avg)) +
  geom_col() +
  coord_flip() +
  xlab("업종") +
  ylab("평균 총급여")
