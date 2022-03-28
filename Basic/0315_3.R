# ---- One sample T-test (표본평균) ----

# 00 과자의 무게는 150g으로 표시가 되어 있음
# 총 10개의 과자를 구매한 결과, 평균 145g, 표준편차는 7.5g으로 판명됨
# 실제 과자의 평균 무게가 150g인지 검정한다. 

pop_mean    = 150
sample_mean = 145
sd          = 7.5
N           = 10

t_val = (sample_mean - pop_mean) / (sd / sqrt(N))
t_val

p.value <- pt(t_val, df=N-1) * 2
p.value

set.seed(1234)
data = rnorm(N, mean = sample_mean, sd = pop_mean)
t.test(data, mu = 150)

# ---- One sample T-test (표본비율) ----

# 귀무가설: 핸드폰 액정의 불량률은 10% 미만이다(=0.1)
# 대립가설: 핸드폰 액정의 불량률은 10%를 넘는다(>0.1)
# 데이터 현황
# 표본의 수는 200개, 총 22개가 불량으로 확인됨

N               = 200
def_prod      = 22 
pop_rate    = 0.1
sample_rate = 22 / 200

z = (sample_rate - pop_rate) / sqrt(pop_rate*(1 - pop_rate) / N)
alpha <- 0.05
(c.u <- qnorm(1-alpha) )
(p.value <- 1 - pnorm(z) )

# prop.test
prop.test(def_prod, N, p = 0.1, alternative = "greater", correct = FALSE)


# ---- Two sample T-test (대응표본) ----

# 쥐 실험 몸무게 데이터 샘플
data <- read.csv("paired_t_test.csv")
print(data)

# 데이터 전처리
library(dplyr)
library(reshape2)
library(ggplot2)

data2 = melt(data, 
             measure.vars = c("Prior", "Post"),
             variable.name = "group", 
             value.name= "weight")

# 시각화
data2 %>% 
  group_by(group) %>% 
  summarise(count = n(), mean = mean(weight), sd = sd(weight))

ggplot(data2, aes(x = group, y = weight)) +
  geom_boxplot()

# 정규성 검정(n<30)

data3 <- data %>% 
  mutate(differences = Prior - Post)

shapiro.test(data3$differences)

# p-value: 0.5156
# 귀무가설 채택, 데이터는 정규분포를 이룬다.

# T-test (양측 검정)
t.test(data$Prior, data$Post, paired = TRUE, alternative = "two.sided")

# 검정통계량: -4.1849
# p-value: 0.0007003
# 귀무가설 기각, 해당 치료법은 쥐 몸무게 변화에 영향을 준다.

# ---- Two sample T-test (독립표본) ----

women_height <- c(138.9, 161.7, 172.3, 120.8, 165.4,
                  163.6, 147.4, 147.8, 148.3)
men_height <- c(167.2, 160.5, 166.4, 176, 189.3,
                173.2, 167.1, 161.3, 162.4) 

data <- data.frame(group = rep(c("Woman", "Man"), each = 9),
                   height = c(women_height,  men_height))

# 시각화
library(dplyr)
library(reshape2)
library(ggplot2)

data %>% 
  group_by(group) %>% 
  summarise(count = n(), mean  = mean(height), sd = sd(height))

ggplot(data, aes(x = group, y = height)) + 
  geom_boxplot()

# 정규성 검정 (귀무가설: 데이터는 정규성을 만족한다.)
shapiro.test(data[data$group == "Man", ]$height) # p = 0.09 (H0 채택)
shapiro.test(data[data$group == "Woman", ]$height) # p = 0.5347 (H0 채택)


# 등분산성 검정(귀무가설: 두개 그룹의 분산은 서로 같다.)
var.test(height ~ group, data = data) # p-value = 0.1382 (H0 채택)

# T.test (귀무가설: 남녀의 평균 키는 서로 같다.)
t.test(data$height ~ data$group, mu = 0, alternative = "two.sided",
       var.equal = TRUE)

# 검정 통계량: 2.8606
# p-value: 0.01133
# 귀무가설 기각, 남녀 평균 키는 통계적으로 유의하게 다르다.