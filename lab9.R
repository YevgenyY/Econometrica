library("dplyr")
library("caret")
library("AER")
library("ggplot2")
library("sandwich")
library("ivpack")
library("memisc")

# Разделим на обучающую и тестовую части выборки
h <- read.table("flats_moscow.txt", header=TRUE, sep="\t", dec=".")
glimpse(h)

in_train <- createDataPartition(y = h$price, p=0.75, list=FALSE)
h_train <- h[in_train,]
h_test <- h[-in_train,]

nrow(h)
nrow(h_train)
nrow(h_test)

# Обучаем
model_1 <- lm(data=h_train, log(price) ~ log(totsp)+log(kitsp)+log(livesp))
model_2 <- lm(data=h_train, log(price) ~ log(totsp)+brick)

# Прогнозируем
y <- log(h_test$price)
y_hat_1 <- predict(model_1, h_test)
y_hat_2 <- predict(model_2, h_test)

sum( (y-y_hat_1)^2 )
sum( (y-y_hat_2)^2 )

# эндогенность
data("CigarettesSW")
h <- CigarettesSW
help("CigarettesSW")
glimpse(h)

h2 <- mutate(h, rprice = price/cpi, rincome=income/cpi/population, rtax=tax/cpi)
h3 <- filter(h2, year=="1995")

model_0 <- lm(data=h3, log(packs)~log(rprice))
summary(model_0)

# two satage OLS / двухшаговый метод наименьших квадратов
st_1 <- lm(data=h3, log(rprice)~rtax)
# добавляем в набор новую переменную для второго шага
h3$log_price_hat <- fitted(st_1)
# второй шаг
st_2 <- lm(data=h3, log(packs)~log_price_hat)
summary(st_2)

# то же самое, но одной командой R
model_iv <- ivreg(data=h3, log(packs)~log(rprice)|rtax)
summary(model_iv)

mtable(model_0, st_2, model_iv)

# Нюансы двухшагового метода
# тест на коэффициенты с устойчивостью к гетероскедастичности
coeftest(model_iv, vcov=vcovHC)

# наличие экзогенных регрессоров 
model_iv_2 <- ivreg(data=h3, log(packs)~log(rprice)+log(rincome)|log(rincome)+rtax)
coeftest(model_iv_2, vcov=vcovHC)

# несколько инструментальных переменных
h3 <- mutate(h3, rtax2 = (taxs-tax)/cpi)
head(h3)
glimpse(h3)

model_iv_3 <- ivreg(data=h3, 
                    log(packs)~log(rprice)+log(rincome)|log(rincome)+rtax + rtax2)
coeftest(model_iv_3, vcov=vcovHC)

