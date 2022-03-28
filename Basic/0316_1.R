library(dplyr)
library(ggplot2)

my_data = PlantGrowth
my_data$group <- ordered(my_data$group, levels=c("ctrl", "trt1", "trt2"))

my_data %>%
  group_by(group) %>%
  summarise(cnt=n(), avg=mean(weight, na.rm=TRUE), sd=sd(weight, na.rm=TRUE))

ggplot(my_data, aes(x=group, y=weight)) +
  geom_boxplot()


# ---- One-Way ANOVA Test ----
# 오차제곱합(SSE), 평균오차제곱(MSE)
ctrl <- my_data$weight[my_data$group=="ctrl"]
trt1 <- my_data$weight[my_data$group=="trt1"]
trt2 <- my_data$weight[my_data$group=="trt2"]

ctrl_m = mean(ctrl)
trt1_m = mean(trt1)
trt2_m = mean(trt2)

ctrl_sse = sum((ctrl-ctrl_m)^2)
trt1_sse = sum((trt1-trt1_m)^2)
trt2_sse = sum((trt2-trt2_m)^2)

sse = ctrl_sse + trt1_sse + trt2_sse
df_e <- (length(ctrl)-1) + (length(trt1)-1) + (length(trt2)-1)
mse <- sse / df_e

# 처리제곱합(SST), 평균처리제곱(MST)
tot_m = mean(my_data$weight)
ctrl_sst = length(ctrl)*sum((ctrl_m - tot_m)^2)
trt1_sst = length(trt1)*sum((trt1_m - tot_m)^2)
trt2_sst = length(trt2)*sum((trt2_m - tot_m)^2)

sst = ctrl_sst + trt1_sst + trt2_sst
df_t = length(levels(my_data$group)) - 1
mst = sst / df_t

# 총제곱합(SST) 확인
tsq = sum((my_data$weight - tot_m)^2)
ss = sst + sse
all.equal(tsq, ss)

# F 통계량, p-value
f = mst / mse

alpha = 0.5
tol <- qf(1-alpha, df_t, df_e)
p.value = 1-pf(f, df_t, df_e)

# ANOVA 함수
res.aov <- aov(data=my_data, weight~group)
summary(res.aov)

# F-분포 그래프
ggplot(data.frame(x=c(0,5)), aes(x=x)) +
  stat_function(fun="df", args=list(df1=df_t, df2=df_e), geom="line") +
  geom_point(aes(x=f, y=df(f, df1=df_t, df2=df_e)), colour="red", size=2) +
  ggtitle("F Distribution") +
  ylab("density")
