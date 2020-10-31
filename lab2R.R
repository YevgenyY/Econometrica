library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# генерируем случайные величины
# Z_1, ...... Z_100 ~ N(5, 9)

z <- rnorm(100, mean=5, sd=3)
qplot(z)

# построим функцию плотности
x <- seq(-10, 15, by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y,geom="line")

# посчитаем вероятность того, что z < 3
# P(Z<3) = F(3)
pnorm(3, mean=5, sd=3)

# P(Z in [4:9])
# P(Z<9 - P(Z<4))
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# найдём квантиль распределения
# P(Z<a) = 0.7, a?
qnorm(0.7, mean=5, sd=3)

# другие распределения
# rchisq, dchisq, pchisq, qchisq - функции для распределения хи квадрат
# rt, dt, pt, qt - функции для t-распределения
# r - генерирование распределения
# d - плотость
# p - вероятность
# q - квантиль

