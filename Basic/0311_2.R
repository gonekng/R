# library(hflights) -> 패키지를 REM에 적재해놓고 사용
# hflights::hflights -> 임시로 패키지 속 객체 및 함수 사용

# p.98

library(dplyr)
mpg1 <- read.csv("solution/data/public_dataset/mpg1.csv", header = TRUE,
                 stringsAsFactors = FALSE)
str(mpg1)

mpg1_new <- mpg1 %>% rename(manuf=manufacturer, tr=trans)
# mpg1_new <- rename(mpg1, manuf=manufacturer, tr=trans)
str(mpg1_new)

count(mpg1, trans)
class(count(mpg1, trans))
table(mpg1$trans)
class(table(mpg1$trans))

mpg1_1 <- mpg1 %>% select(manufacturer, trans, cty)
# mpg1_1 <- select(mpg1, manufacturer, trans, cty)
str(mpg1_1)

mpg1_2 <- mpg1 %>% select(-c(cty, hwy))
# mpg1_2 <- mpg1 %>% select(-cty, -hwy)
# mpg1_2 <- select(mpg1, -c(cty, hwy))
str(mpg1_2)
