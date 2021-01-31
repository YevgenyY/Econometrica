library("dplyr")
library("broom")
library("mfx")
library("vcd")
library("erer")

# 1 LR-stat
2*(-300 + 310)
qchisq(p=0.99, df=1)

# 2 probit model
# y_hat_start =2 - 0.3*x_i + eps_i # это оценка скрытой переменной модели, которая максимизируется
# eps_i ~ N(0, 1)
# 2 Calculate Probability
# вычисляем значение функции в точке b_1 + b_2*x_i

p <- pnorm(2 - 0.3*6, 0, 1)
round(p, 2)

# 3
# I = -l''(theta_hat)
E <- 7
var_theta_hat <- 1/25
z_crit <- qnorm(0.975, mean=0, sd=1) # 1.96 and we use 0.975 as N(0,1) has two tails
left_ci <- E - z_crit * sqrt(var_theta_hat)
right_ci <- E + z_crit * sqrt(var_theta_hat)
left_ci
right_ci

# 4
# y_hat = -1 + 0.25*x_i + 0.05*z_i
# find x_i for max( dP(y_i = 1)/dz) if z_i = 10
# используем правило вычисление производной сложной функции
# dP(y_i=1) = dF(-1+0.25x_i+0.05z_i)/dz = f(-1+0.25x_i+0.05z_i)*0.05
# Найти x_i при котором предельный эффект (производная) максимален
u <- seq(from = -10, to=10, by=0.1)
y <- exp(-u)/(1+exp(-u))^2
qplot(u, y, geom="line")

# u should be zero then
# -1 + 0.25*x + 0.05*10 = 0 => x=2


# 5
# ln( f(x) )

# 6
# L(a) = nln(a) + (a-1)sum( ln(x) ) - an(ln(2)) 
# mul(x_i) = 7.78 * 10^(-20)
# calculate derivative by a and find extremum
p <- 7.78 * ( 10^-20)
n <- 100
a_hat <- n / ( n*log(2) - log(p) )
a_hat

# 11
require(graphics)
## Full mosaic
mosaicplot(HairEyeColor)

# 12
options(stringsAsFactors = FALSE)

t <- read.csv("data/titanic3.csv")
glimpse(t)

t <- mutate(t, sex=as.factor(sex), pclass=as.factor(pclass), 
            survived=as.factor(survived))
glimpse(t)
summary(t)

m_logit <- glm(data=t, survived ~ age + sex + fare + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)

# 13
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare  + I(fare^2) + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)
b <- m_logit$coefficients[2] # age_i
a <- m_logit$coefficients[3] # age_i^2
-b/(2*a)

# 14
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare  + I(fare^2) + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)
confint(m_logit)

# 15
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)
newdata <- data.frame(age=40, sex=as.factor("male"), fare=200, sibsp=2)
pr <- predict(m_logit, newdata, se=TRUE)
pr
plogis(pr$fit)

# 16
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)
newdata <- data.frame(age=50, sex=as.factor("male"), fare=200, sibsp=2)
pr <- predict(m_logit, newdata, se=TRUE)
pr
ci_left <- pr$fit - 1.96*pr$se.fit
plogis(ci_left)

# 17
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp + parch,
               family=binomial(link="logit"), x=TRUE)

mr_eff <- maBina(m_logit, x.mean=FALSE)
mr_eff
round( mr_eff$out[1]$effect[6], 2 )
logitmfx(data = t, survived ~ age + I(age^2) + fare + sibsp + parch)


# 18
d <- dplyr::select(t, age, sibsp, sex, fare, parch, survived) %>% na.omit()
m_logit <- glm(data=d, survived ~ age + I(age^2) + 
                 sex + fare + 
                 I(fare^2) + sibsp + parch,
               family=binomial(link="logit"), x=TRUE)
m_logit2 <- glm(data=d, survived ~ age + I(age^2) + 
                 sex + sibsp + parch,
               family=binomial(link="logit"), x=TRUE)

summary(m_logit2)
lrtest(m_logit, m_logit2)

# 19
d <- dplyr::select(t, age, sibsp, sex, fare, parch, survived) %>% na.omit()
m_logit <- glm(data=d, survived ~ age + I(age^2) + 
                 sex + fare + sibsp, family=binomial(link="logit"), x=TRUE)
summary(m_logit)

newdata <- d
pr_logit <- predict(m_logit, newdata, se=TRUE)
newdata_pr <- cbind(newdata, pr_logit)
head(newdata_pr)

newdata_pr <- mutate(newdata_pr, prob=plogis(fit), 
                     left_ci = plogis(fit-1.96*se.fit),
                     right_ci= plogis(fit+1.96*se.fit))
head(newdata_pr)
h <- dplyr::select(newdata_pr, survived, prob)
h
roc.data <- roc(h$prob, h$survived)
qplot(roc.data$cutoffs, y=roc.data$tpr)
qplot(roc.data$cutoffs, y=roc.data$fpr)
qplot(roc.data$fpr, y=roc.data$tpr)

cutoff <- 0.65
sv <- subset(h, prob >= 0.65)
di <- subset(h, prob < 0.65)
sum(sv$survived==1) / sum(d$survived==1) 

# 20
m_logit <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp,
               family=binomial(link="logit"), x=TRUE)
summary(m_logit)
