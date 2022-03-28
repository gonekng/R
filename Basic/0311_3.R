library(ggplot2)

iris <- iris
str(iris)

theme_set(theme_bw())
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point() +
  xlim(4,8) +
  ylim(2,5) +
  labs(subtitle = "IRIS", y="width", x="length", title="Scatterplot")
