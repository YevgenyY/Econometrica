# Week #3 tests
library("psych")
library("dplyr")
library("memisc")
library("ggplot2")
library("lmtest")
library("foreign")
library("vcd") # визуализация множества переменных на одном графике
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")



# 1
# wage = b0 + b1*schooling + b2*expirience + b3*mschooling + b4*fschooling
# Answer: coef_num = 2

# 2
RSS_restr <- 210 - 130
RSS_unres <- 210 - 180
k_unres <- 5
n <- 35
r <- 2 # в H_0 проверяем два ограничения b4 & b5 == 0

F <- ( (RSS_restr - RSS_unres)/r ) / (RSS_unres/(n-k_unres))
F

# 3
# price = -80 + 1.9*livesp
# E(price | livesp=70)
# sd^2 = 1259.265
# Q: find var_hat(price_hat | X)
# var_hat(price_hat|X) = var(b_0 + b_1*livesp) =
#     = var(b_0|X) + livesp^2*var(b_1) + 2*livesp*cor(b_0, b_1)
n <- 2040
sigma_sq <- 1259.265
livesp <- 80
df <- data.frame(c(21.9, -0.46), c(-0.46, 0.01))
names(df) <- c("Intercept", "livesp")

var_hat <- 21.9 + livesp^2*0.01 + 2 * livesp * (-0.46)
var_hat

# 4
# price = -100 + 1.7*livesp
# E(price | livesp=80)
# sd^2 = 1259.265
# Q: find var_sub_hat(price_hat | X)
# var_hat(price - price_hat|X) = sd^2 + var_hat(price_hat|X) =
n <- 2040
sigma_sq <- 1259.265
livesp <- 70
df <- data.frame(c(21.9, -0.46), c(-0.46, 0.01))
names(df) <- c("Intercept", "livesp")
var_hat <- 21.9 + livesp^2*0.01 + 2 * livesp * (-0.46)
var_hat
var_sub_hat <- sigma_sq + var_hat 
var_sub_hat

# 11
df <- diamonds
glimpse(df)
is.fact <- sapply(df, is.factor)
df[, is.fact]

# 12
model_0 <- lm(data=df, price~log(carat))
summary(model_0)

# 13
model_0 <- lm(data=df, price~carat)
model_1 <- lm(data=df, price~carat + x)
summary(model_0)
summary(model_1)

# 14
library(memisc)
model_1 <- lm(data=df, price~carat+clarity)
model_2 <- lm(data=df, price~carat)
summary(model_1)
summary(model_2)

# 15
model_3 <- lm(data=df, price~carat+depth)
mtable(model_3, summary.stats=c("Deviance","AIC","N"))

# 16
model_1 <- lm(data=df, price~carat)
model_2 <- lm(data=df, price~carat + depth)
model_3 <- lm(data=df, price~carat + depth + clarity)
summary(model_4)
mtable(model_1, model_2, model_3, summary.stats=c("Deviance","BIC","N"))

# 17
# compare model_1 and model_4 
waldtest(model_1, model_2)

# 18
# Тест Рамсея
resettest(model_2)

# 19
qplot(data=df, log(price), fill=cut, geom="density",alpha=0.5) + 
  facet_grid(~clarity)

# 20
qplot(data=df, log(carat),mean(log(price)), color=clarity) + facet_wrap(~cut)

