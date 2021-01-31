library("lubridate")

library("zoo")
library("xts")
library("dplyr")
library("ggplot2")
library("forecast")
library("quantmod")
library("sophisthse")

# сгенерируем ra-процесс, посмотрим на его корреляционную функции 
# и частную корреляционную функцию
y <- arima.sim(n=100, list(ar=0,7))
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y)

# построим белый шум
y <- arima.sim(n=100, list(order=c(0,0,0)))
tsdisplay(y)

# посмотрим ma-процесс и его корреляционые функции
y <- arima.sim(n=100, list(ma=0.8))
tsdisplay(y)

# построим процесс arma
y <- arima.sim(n=100, list(ma=-0.8, ar=0.5))
tsdisplay(y)

# построим процесс arma(1,1)
y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5))
tsdisplay(y)

# построим нестационарный процесс
y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)

y <- arima.sim(n=500, list(order=c(0,1,0)))
tsdisplay(y)

y <- seq(0, 10, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

y <- seq(0, 2, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

# Уровень воды в озере Гурон
# видим, что процесс занимает промежуточное положение, несмотря на то, что 
# ACF убывает, но недостаточно медленно для идеального стационарного процесса
y <- LakeHuron
tsdisplay(y)

# 
mod_1 <- Arima(y, order=c(2,0,0)) # 
mod_2 <- Arima(y, order=c(1,0,1)) # arma(1,1)
summary(mod_1)
summary(mod_2)

AIC(mod_1)
AIC(mod_2)

mod_3 <- Arima(y, order=c(2,0,1))
summary(mod_3)
AIC(mod_3)

prognoz <- forecast(mod_2, h=5)
prognoz
plot(prognoz)

# нестационарный
mod_4 <- Arima(y, order=c(1,1,0))
summary(mod_4)
AIC(mod_4)

prognoz <- forecast(mod_4, h=5)
prognoz
plot(prognoz)

# Автомодель
mod_a <- auto.arima(y)
summary(mod_a)
AIC(mod_a)

prognoz <- forecast(mod_a, h=5)
prognoz
plot(prognoz)

# Пример - анализ стоимости акций Гугл 
Sys.selocale("LC_TIME", "C")
getSymbols(Symbols = "GOOG", from="2014-01-01", to="2014-12-01")

head(GOOG)
y <- GOOG$GOOG.Close

tsdisplay(y)
dy <- diff(y)
dy
tsdisplay(dy) # видим, что все корреляции близки к нулю - процесс белый шум

mod_1 <- Arima(y, order=c(0,1,0))
summary(mod_1)
prognoz <- forecast(mod_1, h=10)
prognoz
plot(prognoz)

mod_a <- auto.arima(y)
summary(mod_a)

# анализ процеса - численности населения России
y <- sophisthse("POPNUM_Y")
tsdisplay(y) # тоже похож на случайное блуждание
mod_1 <- Arima(y, order=c(1,1,0), include.drift=TRUE)
summary(mod_1)
prognoz_1 <- forecast(mod_1, h=5)
plot(prognoz_1)

# анализ индекса потребительских цен
y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))

time(y)
ym <- y[97:nrow(y)]
tsdisplay(as.ts(ym))

# сезонная модель
mod_1 <- Arima(ym, order=c(1,0,0), seasonal=c(1,0,0))
summary(mod_1)
AIC(mod_1)
prognoz_1 <- forecast(mod_1, h=12)
plot(prognoz_1)

mod_a <- auto.arima(ym)
AIC(mod_a)
prognoz_a <- forecast(mod_a, h=12)
plot(prognoz_a)

