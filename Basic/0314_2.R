library(dplyr)
library(ggplot2)
library(readxl)

who_disease <- read_xlsx("who_disease.xlsx")
glimpse(who_disease)

#### Bivariate - Categorical VS. Quantitative ####
# ---- Bar Chart: Point & theme ----
# 'EUR' 지역의 1980년 'pertussis' 데이터
eur_pertussis <- who_disease %>% 
  filter(
    region == 'EUR', 
    year == 1980, 
    disease == 'pertussis'
  )

# 국가별 case 비교 분석
ggplot(eur_pertussis, aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip()

eur_pertussis %>% 
  filter(cases > 0) %>%
  ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank() # x축 선 제거
  )

eur_pertussis %>% 
  filter(cases > 0) %>% 
  ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_col(fill = "skyblue2") +
  coord_flip() + 
  theme_minimal() # 배경 색상 제거

eur_pertussis %>% 
  filter(cases > 0) %>% 
  ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_point(size = 3) +
  scale_y_log10() + # y축 조정
  theme_minimal() + 
  coord_flip() # x,y 바꾸기

# ---- Box Plox: Various alternatives ----
# Box Plot
install.packages("carData")
library(carData)
data(Salaries, package="carData")
glimpse(Salaries)

ggplot(Salaries, aes(x = rank, y = salary)) +
  geom_boxplot()

# rank?? ?׷??
rank_data <- Salaries %>%
  group_by(rank) %>%
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd / sqrt(n))

ggplot(rank_data, aes(x = rank, y = mean, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)

# gender
gender_data <- Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd/sqrt(n))

ggplot(gender_data, aes(x = rank, y = mean, group = sex, colour = sex)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)

# position_dodge를 활용하여 그래프 간격 조정
pd <- position_dodge(0.3)
ggplot(gender_data, 
       aes(x = rank, 
           y = mean, 
           group=sex, 
           color=sex)) +
  geom_point(position = pd, 
             size = 3) +
  geom_line(position = pd,
            size = 1) +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), 
                width = .1, 
                position= pd) +   
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette="Set1") +
  theme_minimal()


## ggplot cheet sheet
glimpse(economics)
a <- ggplot(economics, aes(date, unemploy))
a + geom_path()

glimpse(seals)
b<- ggplot(seals, aes(x=long, y=lat))
b + geom_ribbon(aes(xmin=long, ymin=lat, xmax=long+1, ymax=lat+1))
