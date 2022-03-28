#### Chapter 1. 기초 문법 ####

a <- 1 / 100 * 30
b <- 1 / 1000

groupA <- "A그룹"
group_A <- "A그룹"
group.A <- "A그룹"

r_basics <- 3
class(r_basics)
class(group_A)

temp <- TRUE
class(temp)

#### Chapter 2. 벡터 만들기 ####

num_vector = c(1,2,3)
print(num_vector)
class(num_vector)

char_vector = c("A","B","C")
print(char_vector)
class(char_vector)

logical_vector = c(TRUE, FALSE, TRUE)
print(logical_vector)
class(logical_vector)

#### (1) 예외 ####

temp1 = c(1,"1",2) # num < char
print(temp1)
class(temp1)

temp2 = c(1, FALSE, TRUE) # bool < num
print(temp2)
class(temp2)

temp3 = c("A", FALSE, TRUE) # bool < char
print(temp3)
class(temp3)

#### (2) 범주형 변수 ####

loc_vector = c("서울", "경기", "대구", "대전", "광주", "부산")

# 비서열 변수(명목 척도) -> (python) one-hot encoding
fct_vector = factor(loc_vector)
print(fct_vector)
class(fct_vector)

# 서열 변수 -> (python) ordinary encoding
fct_vector2 = factor(loc_vector, ordered = TRUE)
print(fct_vector2)
class(fct_vector2)
