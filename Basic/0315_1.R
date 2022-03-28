#### 모수와 통계량 ####

albums <- read.csv("album.csv")

# 기술통계량 (Descriptive Analysis)

# 최소, 최대, 최빈
sort(albums$sales)[1] # min(albums$sales)
sort(albums$sales)[200] # max(albums$sales)
sort(table(albums$sales), decreasing = TRUE)[1]

# 평균, 중앙값
mean(albums$sales)
median(albums$sales)

# 결측치 처리
sales2 = c(albums$sales, NA)
mean(sales2)
mean(sales2, na.rm = TRUE)

# 평균의 가장 큰 단점: 극단적인 값에 민감
sales3 = c(albums$sales, 300000000000)
mean(sales3)
median(sales3) # 중앙값: Robust(강건함)

# 분산, 표준편차
weight = c(64,68,70,72,76)
w_mean = mean(weight)
w_dev = weight - mean(weight)
w_dev2 = w_dev ^2
mean(w_dev2)
sqrt(mean(w_dev2))

var(weight) # 표본분산(통계량)
sd(weight) # 표본표준편차(통계량)

# 변동계수
cv_fun <- function(data){ # 사용자 정의 함수
  result = sd(data)/mean(data)*100
  return(result)
}

# 주식 데이터 변동성 비교
install.packages("tidyquant")
install.packages("purrr")
library(tidyquant)
library(quantmod)
library(purrr)
library(ggplot2)
library(tibble)

tickers = c("AAPL","TSLA")
getSymbols(tickers, from="2022-01-02", to="2022-01-30")

stock <- map(tickers, function(x) Ad(get(x)))
stock <- reduce(stock, merge)
colnames(stock) <- tickers
head(stock)

stock_df <- stock %>% data.frame(date=index(stock))
stock_df

cv_fun(stock_df$AAPL)
cv_fun(stock_df$TSLA)

ggplot(stock_df, aes(x=date)) +
  geom_line(aes(y=AAPL, colour="Apple")) +
  geom_line(aes(y=TSLA, colour="Tesla")) +
  scale_color_manual(name="Company", 
                     values=c("Apple"="red", "Tesla"="darkblue")) +
  theme_bw()

# 사분위수
qs_df <- quantile(albums$sales)
qs_df[4] - qs_df[2] # IQR(albums$sales)

boxplot(albums$sales)

# 이상치 판별
sales2 <- c(albums$sales, 450, 460, -100, -1000)
q2 <- quantile(sales2)
boxplot(sales2)

bottom = q2[2] - 1.5 * (q2[4] - q2[2]) # 상한: q1 - 1.5 * (q3-q1)
top = q2[4] + 1.5 * (q2[4] - q2[2]) # 하한: q3 + 1.5 * (q3-q1)

sales2[sales2 < bottom | sales2 > top]
