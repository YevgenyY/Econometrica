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

h <- diamonds
glimpse(h)

qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("../data/flats_moscow.txt", sep="\t", header=TRUE, dec=".")
glimpse(f)
qplot(data=f, totsp, price)
qplot(data=f, log(totsp), log(price))
mosaic(data=f, ~walk+brick+floor, shade=TRUE)

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position = "dodge")
qplot(data=f, log(price), fill=brick, geom="density")
qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

# МНК
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))

summary(model_0)
summary(model_1)
summary(model_2)
mtable(model_2)
sjp.lm(model_2)

# Построение прогнозов
nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))
predict(model_2, newdata=nw)
exp(predict(model_2, newdata=nw))

# строим доверительный интервал - для средних значений
predict(model_2, newdata=nw, interval="confidence")
exp(predict(model_2, newdata=nw, interval="confidence"))

# строим предиктивный интервал - для конкретного случая
predict(model_2, newdata=nw, interval="prediction")
exp(predict(model_2, newdata=nw, interval="prediction"))

# проверка гипотезы о линейных ограничениях, представление результатов
# waldtest, F-stat
# show p-value
waldtest(model_0, model_1) # H_0: true model_0 is rejected
waldtest(model_1, model_2) # H_0: true model_1 is rejected
waldtest(model_0, model_2) # H_0: true model_0 is rejected

# Draw a base graph
gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method="lm") + facet_grid(~walk)
gg0 + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk)

# Посмотрим, что будет, если сделаем неправильно -
# добавим лишнюю дамми-переменную
# есть переменная brick, добавим её инверсию nonbrick
f$nonbrick <- memisc::recode(f$brick, 1<-0, 0<-1)
model_wrong <- lm(data=f, log(price)~log(totsp)+brick+nonbrick)
# вывод R выкинул лишнюю дамми-переменную

# посмотрим AIC/BIC
mtable(model_0, model_1, model_2, summary.stats=c("Deviance","AIC","N"))

# теперь сделаем тест Рамсея или RESET
resettest(model_2)