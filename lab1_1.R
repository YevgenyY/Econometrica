library("psych")
library("dplyr")
library("ggplot2")
library("GGally")

t <- swiss
help(swiss)
glimpse(t)
describe(t)

# посмотрим на данные с помощью диаграмм рассеяния
ggpairs(t)
model2 <- lm(data=t,
             Fertility~Agriculture+Education+Catholic)
coef(model2)
fitted(model)
residuals(model2)
deviance(model2)

# вычисляем R2
report <- summary(model2)
report
report$r.squared

cor(t$Fertility, fitted(model2))^2

nd2 <- data.frame(Agriculture=0.5,Catholic=0.5,Education=20)
predict(model2, nd2)

