library("devtools")
#install_github("dgrtwo/broom")
#install_github("cran/bstats")

library("lubridate") # работа с датами

library("sandwich") #vcovHC, vcovHAC
library("lmtest") # tests
library("car") # more tests
library("bstats") # more more tests
library("zoo") # временные ряды
library("xts") # ещё ряды
library("dplyr") # манипуляции с данными
library("broom") # просто манипуляции
library("ggplot2") # графики

library("quantmod") # загрузка данных с finance.google.com
library("rusquant") # загрузка данных с finam.ru
library("sophisthse") # загрузка данных с sophist.hse.ru
library("Quandl")

# Begin here <----
x <- c("2020-04-15", "2011-08-07")
y <- ymd(x) # отконвертировали вектор строк в вектор с датами
y + days(21)
y - years(10)
day(y)
month(y)
year(y)
vignette("lubridate") # показать документацию

#
x <- rnorm(5)
x
y <- ymd("2014-01-01")+days(0:4)
y

# zoo
ts <- zoo(x, order.by=y)
ts

stats::lag(ts, -1) 
diff(ts)

# поквартально, 4 наблюдения в год
ts2 <- zooreg(x, start=as.yearqtr("2014-01"), freq=4)
ts2

ts3 <- zooreg(x,start=as.yearmon("2014-01"), freq=12)
ts3

data("Investment")
help("Investment")

start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

#
dna <- Investment
dna[1,2] <- NA
dna[5,3] <- Na
dna
# искусственно заполним пропущенные значения
na.approx(dna) # Линейная интерполяция заполнения пропущенных значений
na.locf(dna)   # Заполняем последним не пропущенным значением

# Рассмотрим открытые источники финансовых данных в Интернете
# finance.yahoo.com
# google.com/finance
# www.quandl.com
# finam.ru
# sophist.hse.ru
a <- sophisthse("POPNUM_Y")
a

# Quandl
b <- Quandl("FRED/GNP")

# finance.google.com
Sys.setlocale("LC_TIME", "C") # для интерпретации формата даты в US
getSymbols(Symbols = "AAPL",from="2010-01-01", to="2020-01-01", src="yahoo")
head(AAPL)
tail(AAPL)

# finam.ru
getSymbols(Symbols = "GAZP",from="2014-01-01", to="2020-09-09", src="Finam")
head(GAZP)
tail(GAZP)

plot(GAZP)
autoplot(GAZP[,1:4], facet=NULL)
chartSeries(GAZP)

# Проанализируем какой-нибудь набор данных
data("Investment")
d <- as.zoo(Investment)
autoplot(d[,1:2], facet=NULL)

model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model)
coeftest(model) # тест Вальда на равеноство коэффициентов нулю
confint(model)  # смотрим доверительные интервалы
d_aug <- augment(model, as.data.frame(d))

# смотрим на разницу в корреляционных матрицах
vcov(model)
vcovHAC(model)

# сделаем проверку гипотез с учётом автокорреляции
coeftest(model, vcov. = vcovHAC(model))
coeftest(model)

# найдёт доверительные инфтервалы
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_ac=conftable[,2])
ci
ci <- mutate(ci, left_95=estimate-1.96*se_ac, 
                 right_95= estimate+1.96*se_ac)

# сравним доверительные интервалы для модели без учёта корреляции
# и для модели с учётом корреляции
confint(model)  # старая модель - без автокорреляции
ci # интервалы для модели с учётом корреляции

# формальные тесты на автокорреляцию
# Тест Дарбина-Уотсона / Durbin-Watson
# H0: нет автокорреляции
# H1: автокорреляция 1-го порядка
dwt(model)
res <- dwt(model)
res$dw
res$p # if p-value < 0.05 - There is an autocorrelation between eps & eps-1
res$r # оценка корреляции остатков

# BG-test Бройша-Годфри ( Breusch-Godfrey )
# H0: нет автокорреляции
# Ha: автокорреляция k-го порядка
bgtest(model, order = 2)
# H0 не отвергается (вероятно не хватило данных)
res <- bgtest(model, order = 2)
res$statistic
res$p.value # if p-vaule < 0.05 there is an autocorrelation

