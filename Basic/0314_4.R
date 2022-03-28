# 외부 라이브러리를 활용하여 다양한 형태의 그래프 만들기

library(ggplot2)
library(ggridges)
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(gridExtra)

p1 <- ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  geom_density_ridges()

p2 <- ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  geom_boxplot()

grid.arrange(p1,p2)
