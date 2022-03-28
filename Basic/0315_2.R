# ---- 1. Z-value ----

mean_15 = 120
mean_20 = 125
sd_20   = 15
N       = 30
z_value = (125 - 120) / (15 / sqrt(N))

# ---- 2. Z-distribution ----

qt(0.025, df = N-1) # 유의수준 0.05 양측검정
par(mar=c(0,1,1,1))

x <- seq(-3, 3, by=0.001)
y <- dt(x, df=N)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38),
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
upper_lim <- round(qt(1-(alpha/2), df=N), 2)
lower_lim <- -upper_lim
polygon(c(-3, x[x<lower_lim], lower_lim), c(0, y[x<lower_lim], 0), col=2)
polygon(c(upper_lim, x[x>upper_lim], 3), c(0, y[x>upper_lim], 0), col=2)

arrows(z_value, 0.05, z_value, 0, length=0.1, col = "red")
text(z_value, 0.07, paste("t=", round(z_value, 3)))

text(lower_lim, -0.02, expression(-t[0.025]==-2.05))
text(upper_lim, -0.02, expression(t[0.025]==2.05))

# ---- 3. Z-value Location ----

x <- seq(-3, 3, by=0.001)
y <- dt(x, df=N-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38),
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
upper_lim <- round(qt(1-(alpha/2), df=N), 2)
lower_lim <- -upper_lim
polygon(c(-3, x[x<lower_lim], lower_lim), c(0, y[x<lower_lim], 0), col=2)
polygon(c(upper_lim, x[x>upper_lim], 3), c(0, y[x>upper_lim], 0), col=2)

arrows(z_value, 0.05, z_value, 0, length=0.1, col = "red")
text(z_value, 0.07, paste("t=", round(z_value, 3)))

text(lower_lim, -0.02, expression(-t[0.025]==-2.05))
text(upper_lim, -0.02, expression(t[0.025]==2.05))

# ---- 4. P-value ----

par(mar=c(0,1,1,1))

x <- seq(-3, 3, by=0.001)
y <- dt(x, df=N-1)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38),
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
upper_lim <- round(qt(1-(alpha/2), df=N-1), 2)
lower_lim <- -upper_lim
polygon(c(-3, x[x<lower_lim], lower_lim), c(0, y[x<lower_lim], 0), col=2)
polygon(c(upper_lim, x[x>upper_lim], 3), c(0, y[x>upper_lim], 0), col=2)
text(lower_lim, -0.02, expression(-t[0.025]==-2.05))
text(upper_lim, -0.02, expression(t[0.025]==2.05))

p.value <- 1 - pt(z_value, df=N-1)
polygon(c(z_value, x[x>z_value], 3), c(0, y[x>z_value], 0),
        density=20, angle=45)
text(z_value-0.3, 0.04, paste("t=", round(z_value, 3)))
text(2.3, 0.1, paste("P(T>t)=",round(p.value, 3)))

# 유의확률 해석
# 양쪽 검증 실 시, 유의 확률은 0.5
# 그러나 통상 절반의 값만 나오기 때문에 a * 2를 하거나 유의확률을 절반으로 나눠서 구함

# 최종 분석 해석
# 질문: Q. 2020년 만 7세 여자 어린이의 평균 키는 2015년과 다른가요?
# 결론: 2020년 만 7세 여자 어린이의 평균 키는 유의수준 0.05 수준에서 2015년 평균 키와 다르지 않다는 귀무가설을 기각할 수 없다. 즉, 2020년 만 7세 여자 어린이의 평균 키는 2015년과 동일한 120cm로 봐야 한다. 

