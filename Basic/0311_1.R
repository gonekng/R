install.packages("dplyr")
library(dplyr)

name <- c("Joy","James","Daniel")
age <- c(27, 30, 33)
late <- c(TRUE, FALSE, FALSE)

student <- data.frame(name=name, age=age, late=late)
str(student)

# 경로 확인
write_xlsx(student, path='학생.xlsx')

getwd()

# csv파일 저장
write.csv(student, file="학생.csv", row.names = FALSE)

# 엑셀로 내보내기
library(writexl)

# 모든 객체 삭제
rm(list = ls())

# csv파일 불러오기
students = read.csv("학생.csv")

# 엑셀파일 불러오기
library(readxl)
students2 = read_xlsx("학생.xlsx", sheet=1)

# dplyr 패키지
library(dplyr)
iris <- iris
str(iris)

iris %>%
  select(Sepal.Length, Sepal.Width) %>%
  filter(Sepal.Length > 5) %>%
  head(10) -> iris2

