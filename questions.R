# y = 7.3 + 5*x - 6*z
# se(kx) = 1, найти 90% confint
b <- 5
s <- 1
n <- 30
er <- qnorm(0.9)*s/sqrt(n)
c_left  <- b - er
c_rigth <- b + er
round(c_left, digits=2)

# pchisq
# Specify x-values for pchisq function
p9 <- pchisq(4, df=2)
round(p9, 2)

# стоимости
library("ggplot2")
data <- diamonds

ideal <- filter(data, cut=="Ideal")
nrow(ideal)

model <- glm(data=data, price~carat+table)
summary(model)

model_carat <- glm(data=data, price ~ carat)
model_mul <- glm(data=data, price ~ carat + y )
compar <- mtable(model_carat, model_mul)

model <- glm(data=data, price~carat + y + x)

