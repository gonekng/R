#### facet_* 함수 ####
install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(gridExtra)

head(tips)
glimpse(tips)

p1 <- ggplot(tips, aes(x = total_bill, y = tip)) + 
  geom_point(shape = 1, size = 2) + 
  theme_bw() +
  ggtitle("Basic Graph")

p2 <- ggplot(tips, aes(x = total_bill, y = tip)) + 
  geom_point(shape = 1, size = 2) + 
  facet_grid(sex ~ .) + # 행 기준 화면 분할
  theme_bw() +
  ggtitle("Facet_grid divided by a row")

p3 <- ggplot(tips, aes(x = total_bill, y = tip)) + 
  geom_point(shape = 1, size = 2) + 
  facet_grid(. ~ sex) + # 열 기준 화면 분할
  theme_bw() +
  ggtitle("Facet_grid divided by a column")


p4 <- ggplot(tips, aes(x = total_bill, y = tip, colour = time)) + 
  geom_point(shape = 1, size = 2) + 
  facet_grid(day ~ sex) + # 행 기준 day / 열 기준 tip
  theme_bw() +
  ggtitle("Facet_grid divided by row - day and column - sex")

grid.arrange(p1, p2, p3, p4)


#### 그래프에 주석 추가하기 ####
library(ggplot2)
library(dplyr)

p <- ggplot(iris %>% filter(Species != "virginica"), aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(colour = Species)) + 
  theme_classic()
p

p1 <- p + annotate(geom = "text", x = 4.7, y = 4, label = "setosa", size = 5) + 
  ggtitle("Using annoate() - 1")
p1

p2 <- p + 
  annotate(geom = "text", x = 4.7, y = 4, label = "setosa", size = 5) + 
  annotate(geom = "text", x = 6.5, y = 3.5, label = "versicolor", size = 5) + 
  ggtitle("Using annoate() - 2")
p2

g <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point(alpha = .5, colour = "red")
g

g2 <- g + 
  geom_text(aes(label = rownames(mtcars)), hjust = 0, nudge_x = 0.05)
g2

g3 <- g + 
  geom_text(aes(label = rownames(mtcars)), check_overlap = TRUE, hjust = 0, nudge_x = 0.05) + # overlaps 제거하기
  ggtitle("Using geom_text()")
g3

install.packages("ggrepel")
library(ggrepel)
set.seed(42)
g4 <- g + 
  geom_label_repel(aes(label = rownames(mtcars), fill = factor(cyl))) + 
  ggtitle("Using geom_label_repel()")
g4 

library(gridExtra)
grid.arrange(p, p2, g3, g4)


#### 그래프의 축 다루기 ####
library(ggplot2)
library(gridExtra)

# ---- 1) x축과 y축 데이터 뒤바꾸기 ----

p1 <- ggplot(mtcars, aes(x = reorder(rownames(mtcars), mpg), y = mpg)) + 
  geom_bar(stat = "identity", fill = "purple") + 
  theme_classic() + 
  ggtitle("Basic Graph")
p1

p2 <- ggplot(mtcars, aes(x = reorder(rownames(mtcars), mpg), y = mpg)) + 
  geom_bar(stat = "identity", fill = "tomato2") + 
  coord_flip() + 
  theme_classic() + 
  ggtitle("Using coord_flip")
p2

grid.arrange(p1, p2, ncol = 2)


# ---- 2) 연속적인 축의 범위 설정 ----
p <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot() +
  theme_bw()
p

p1 <- p + ggtitle("Basic Graph")

p2 <- p + 
  ylim(0, max(PlantGrowth$weight)) + 
  theme_bw() + 
  ggtitle("By Max Value of Weight")

p3 <- p + 
  ylim(0, 10) + 
  ggtitle("The Range 0 to 10")

p4 <- p + 
  coord_cartesian(ylim = c(4, 6.5)) + 
  ggtitle("Using coord cartesian (4 ~ 6.5)")

grid.arrange(p1, p2, p3, p4)


# ---- 3) 눈금 표시 방법 ----
ggplot(PlantGrowth, aes(x = group, y = weight)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0, 10), # 축 범위
                     breaks = c(1, 3, 5, 7, 9), # 축의 숫자 지정 
                     labels = c("1st","three","five","seven","nine"))

# ---- 4) 범주형 축 항목 순서 변경하기 ----
p <- ggplot(PlantGrowth, aes(x = group, weight)) + 
  geom_boxplot() + 
  theme_bw() + 
  ggtitle("Basic Graph")

p1 <- p + 
  scale_x_discrete(limits = c("trt1", "trt2", "ctrl")) + 
  ggtitle("Change the order of X-Values")

grid.arrange(p, p1)


# ---- 5) 로그 축에 눈금 표시 목적 ----
library(MASS)
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) + 
  geom_text(size = 3) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 24), 
        axis.text.y = element_text(size = 24))

p1 <- p + ggtitle("Basic Graph")
p2 <- p + scale_x_log10() + scale_y_log10() +
  ggtitle("Basic Graph with log10 Scale")

library(scales)
p3 <- p + 
  scale_x_log10(breaks = 10^(-1:5), 
                labels = trans_format("log10", 
                                      math_format(10^.x))) + 
  scale_y_log10() + 
  ggtitle("Basic Graph Using trans_format function")

p4 <- p + 
  scale_x_continuous(trans = log_trans(), 
                     breaks = trans_breaks("log", function(x) exp(x)), 
                     labels = trans_format("log", math_format(e^.x))) + 
  scale_y_continuous(trans = log2_trans(), 
                     breaks = trans_breaks("log2", function(x) 2^x), 
                     labels = trans_format("log2", math_format(2^.x))) + 
  ggtitle("Basic Graph Using log on X log2 on Y")

grid.arrange(p1, p2, p3, p4)


# ---- 6) 축에 날짜 사용하기 ----
library(lubridate)
temp_date <- Sys.Date()
ymd(temp_date)

temp_date <- "07.01.2020"
dmy(temp_date)

temp_date <- "Sep, 12th 2020 14:00"
mdy_hm(temp_date)

str(economics)

p1 <- ggplot(economics, aes(x = date, y = uempmed)) + 
  geom_line() + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 24), 
        axis.text.y = element_text(size = 24)) + 
  ggtitle("Basic Graph")

# 데이터를 2000년 1월 ~ 2010년 12월 데이터를 가져오자.
economics_2010s <- subset(economics, 
                          date >= as.Date("2000-01-01") & 
                            date < as.Date("2010-12-01"))

# 데이터를 6개월 단위로 표시하자
datebreaks <- seq(from = as.Date("2000-01-01"), 
                  to = as.Date("2010-12-01"), 
                  by = "6 month")

p2 <- ggplot(economics_2010s, aes(x = date, y = uempmed)) + 
  geom_line() + 
  ggtitle("Basic Graph with 2010s Data")

p3 <- ggplot(economics_2010s, aes(x = date, y = uempmed)) + 
  geom_line() + 
  scale_x_date(breaks = datebreaks, labels = date_format("%Y-%b")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        axis.text.y = element_text()) + 
  ggtitle("Date_format on X-Axis in English")

p4 <- ggplot(economics_2010s, aes(x = date, y = uempmed)) + 
  geom_line() + 
  scale_x_date(breaks = datebreaks, date_labels = paste0("%Y","년 " ,"%m", "월")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, family = "AppleGothic"), 
        axis.text.y = element_text()) + 
  ggtitle("Date_format on X-Axis in Korean")
grid.arrange(p1, p2, p3, p4)

#### 그래프의 범례 다루기 ####

library(ggplot2)
library(dplyr)

glimpse(mpg)

# 1. 범례 제거
ggplot(mpg, aes(x = class, y = hwy, fill = class)) + 
  geom_boxplot() + 
  theme(legend.position = "none")

# 2. 범례의 위치 변경하기
p <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Pastel2") 

p + theme(legend.position = "top")
p + theme(legend.position = "bottom")
p + theme(legend.position = c(0.8,0.2))

# 3. 범례의 제목 변경하기
p + scale_fill_discrete(name = "Condition_1") # fill=group
p + labs(fill = "Condition_2")

install.packages("gcookbook")
library(gcookbook)
p <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) + 
  geom_point(aes(size = weightLb)) + 
  scale_size_continuous(range = c(1, 4))
p 
p + labs(colour = "Male/Female", size = "Weight\n(pounds)")

# 4. 범례에 속한 라벨 변경하기
p <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_boxplot()

p + scale_fill_discrete(labels=c("Control","Treatment 1","Treatment 2"))

p + scale_fill_discrete(labels=c("Control","Treatment 1","Treatment 2")) +
  scale_x_discrete(breaks = c("ctrl", "trt1", "trt2"), 
                   labels = c("Control", "Treatment 1", "Treatment 2"))

# 5. 범례제목 지우기
p + guides(fill = guide_legend(title = NULL))

#### 그래프의 테마 다루기 ####

# 1. 그래프의 제목 
library(gcookbook)
library(ggplot2)

p <- ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point()

p + labs(title = "Title Text")
p + labs(title = "Title Text") +
  theme_bw() + 
  theme(plot.title = element_text(vjust = -8, hjust = 0.01))

# 2. 텍스트의 외형 변경하기
p + theme(axis.title = element_text(size = 13, lineheight = .9,
                  family = "Times", face = "bold.italic", colour = "red"))

# 3. 테마 사용하기
p + theme_classic()
p + theme_bw()
p + theme_gray()

theme_set(theme_bw()) # 기본 테마 설정하기
p

install.packages("ggthemes") # 외부 테마 불러오기
library(ggthemes)
p + theme_solarized()

# 4. 테마 요소의 외형 변경하기 예제
p <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) +
  geom_point()
p

## 1. 그래프 관련 옵션
p + theme(
  panel.grid.major = element_line(colour = "red"), 
  panel.grid.minor = element_line(colour = "red", linetype = "dashed"),
  panel.background = element_rect(fill = "lightblue"), 
  panel.border = element_rect(colour = "blue", fill = NA, size = 2)
)

## 2. 텍스트 항목 관련 옵션들
p + labs(title = "Plot Title Here") + 
  theme(
    axis.title.x = element_text(colour = "red", size = 14), 
    axis.text.x = element_text(colour = "blue"), 
    axis.title.y = element_text(colour = "red", size = 14, angle = 90), 
    axis.text.y = element_text(colour = "blue"), 
    plot.title = element_text(colour = "red", size = 20, face = "bold")
  )

## 3. 범례 관련 옵션들
p + theme(
  legend.background = element_rect(fill = "grey85", colour = "red", size = 1), 
  legend.title = element_text(colour = "blue", face = "bold", size = 14), 
  legend.text = element_text(colour = "red"), 
  legend.key = element_rect(colour = "blue", size = 0.25)
)

## 4. 면 분할 관련 옵션들
p + facet_grid(sex ~ .) + 
  theme(
    strip.background = element_rect(fill = "pink"), 
    strip.text.y = element_text(size = 14, angle = -90, face = "bold")
  )

# 5. 사용자 정의 테마 설정
my1stTheme <- theme_bw() + 
  theme(
    axis.title.x = element_text(colour = "grey71", size = 15), 
    axis.text.x = element_text(colour = "skyblue"), 
    axis.title.y = element_text(colour = "grey71", size = 15, angle = 90), 
    axis.text.y = element_text(colour = "skyblue"), 
    plot.title = element_text(colour = "grey", size = 15, face = "bold")
  )

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() +
  ggtitle("IRIS Plot") +
  my1stTheme


