library("ggplot2")
library("dplyr")
library("erer")
library("lmtest")
library("caret")
library("sandwich")
library("forecast")

# task 3
TSS <- 100
RSS <- 10
ESS <- TSS - RSS
R_2 <- ESS/TSS
R_2

# task 5
# wage_i = 150 + 20*expirience_i
# b2_hat - t_crit*se(b2_hat)) < b2_true < b2_hat + t_crit*se(b2_hat)
# find p-value for b2_hat
b2_H0 <- 0
t_crit <- qt(0.995, df = 1000 - 2)
n <- 1000
se_b2_hat <- 4
b2_hat <- 20
b2_hat_min <- b2_hat - t_crit*se_b2_hat
b2_hat_min

t_obs <- (b2_hat - b2_H0)/se_b2_hat
t_obs
p_value <- 2*(1-pt(t_obs, df=1000-2))
p_value

# task 6
n <- 1000
t_crit <- qt(0.95, df = n - 2)
se_b2_hat <- 4
b2_hat <- 20
b2_hat_min <- b2_hat - t_crit*se_b2_hat
b2_hat_min


# task 9
n <- 1000
# frogs_i <- 2 + 3.4*t_i
# var(theta - theta_hat|X) = var(theta|X) + sigma_squared =
# = var(a*b1_hat + b*b2_hat|X) + sigma_squared =
# = a*var(b1|X) + b^2*var(b2_hat|X) + 2 * a * b*cov(b1_hat,b2_hat|X) + sigma_squared
4*4.5 + (3.4^2)*0.05 + 2*2*3.4*(-0.23) + 1200


# task 15
library(lmtest)
data(mtcars)
h <- mtcars
model_15 <- lm(data=h, mpg ~ hp + wt + am)
summary(model_15)
bptest(model_15, data=h, varformula = ~ hp + I(hp^2) + wt + I(wt^2))

# task 16
gqtest(model_15, order.by = ~wt, data=h, fraction = 0.3)
gqtest(model_15, order.by = ~hp, data=h, fraction = 0.3) # <- 
gqtest(model_15, order.by = ~wt, data=h, fraction = 0.2)
gqtest(model_15, order.by = ~hp, data=h, fraction = 0.2)

# task 17
library("sandwich")
vcov(model_15) # <- неправильные оценки для ковариационной матрицы
vcovHC(model_15, type="HC0")[3,2]
vcovHC(model_15, type="HC1")[3,2]
vcovHC(model_15, type="HC2")[3,2]
vcovHC(model_15, type="HC3")[3,2] # <--
coeftest(model_15)
coeftest(model_15, vcov. = vcovHC(model_15, type="HC3") )

# task 19
l_max <- -250
l_H0 <- -285
LR <- 2*(l_max - l_H0)
LR
qchisq(p=0.95, df=1)

# task 21
library("dplyr")
t <- read.csv("data/titanic3.csv")

t <- mutate(t, sex=as.factor(sex), pclass=as.factor(pclass), 
            survived=as.factor(survived))

model_21 <- glm(data=t, survived ~ age+I(age^2)+sex+pclass+sibsp,
                  family=binomial(link="logit"), x=TRUE)
summary(model_21)
coeftest(model_21)
summary(model_21)$coeff[5,2]

# task 22
library("erer")
model_22 <- glm(data=t, survived ~ age+I(age^2)+sex+pclass+sibsp,
                family=binomial(link="probit"), x=TRUE)
summary(model_22)
summary(model_22)$coeff[5,2]

maBina(model_22)

# task 25
set.seed(12)
y <- arima.sim(model=list(ar=c(0.1,0.6), ma=-0.3), n=100)
x1 <- rnorm(100, 15, 5)
x2 <- runif(100, 45, 50)
h <- data.frame(y, x1, x2)
head(h)
model_25 <- lm(data=h, y~x1+x2)
vcovHAC(model_25)
coeftest(model_25, vcov. = vcovHAC(model_25))

# task 26
bgtest(model_25, order=3)

# task 27
library("ggplot2")
y1 <- y[2:100]
y1[100] <- 0
plot(y1, y)

# task 31
set.seed(123)
y <- arima.sim(model = list(ar = c(0.5, 0.1), ma = c(0.3,0.2)), n = 100)
mod31_111 <-  Arima(y, order = c(1, 1, 1))
mod31_012 <-  Arima(y, order = c(0, 1, 2))
mod31_110 <-  Arima(y, order = c(1, 1, 0))
mod31_112 <-  Arima(y, order = c(1, 1, 2))

summary(mod31_111)
summary(mod31_012)
summary(mod31_110)
summary(mod31_112)

# task 32
mod31_303 <- Arima(y, order = c(3, 0, 3), fixed=c(0,NA,NA,0,NA,NA,NA))
summary(mod31_303)

# task 35
library("AER")
library("dplyr")
library("caret")

data("CollegeDistance")
h <- CollegeDistance
h
glimpse(h)

set.seed(42)
in_train <- createDataPartition(y = h$wage, p = 0.90, list = FALSE)
h_train <- h[in_train,]  
h_test <- h[-in_train,]

model_35 <- lm(data=h_train, wage~gender+ethnicity+unemp+education+region)
summary(model_35)

# task 36
model_36 <- ivreg(data=h_train, wage~gender+ethnicity+unemp+education+region |
                    distance + gender+ethnicity+unemp+region)
summary(model_36)

# task 37
pw <- predict(model_36, h_test)
