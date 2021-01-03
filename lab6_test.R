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

library("Ecdat")
data("Griliches")
d <- Griliches
head(d)

# 11
model11 <- lm(data=d, lw80~age80+iq+school80+expr80)
summary(model)
vcov(model11)

# 12
cm <- vcov(model11)
cmHC <- vcovHC(model11, type="HC3")
cm[3,4] - cmHC[3,4]

# 13
vcovHC(model11, type="HC0")[2,2]
vcovHC(model11, type="HC1")[2,2]
vcovHC(model11, type="HC2")[2,2]
vcovHC(model11, type="HC3")[2,2]

# 14
# Breusch-Pagan/White test
bptest(model11, data=d, varformula = ~ + I(expr80))

# 15
# Goldfeld-Quandt test
gqtest(model11, order.by = ~ age80, data=d, fraction = 0.2)

# 16
data(Solow)
help(Solow)
h <- Solow
head(h)
model16 <- lm(data=h, q ~ k + A)
summary(model16)

vc16 <- vcov(model16)
vc16HAC <- vcovHAC(model16)
vc16HAC[3,3] - vc16[3,3]

# 17
# Durbin-Watson test
model17 <- lm(data=h, q ~ A)
summary(model17)
dwt(model17)

# 18
#  Breusch-Godfrey test
bgtest(model17, order = 3)

# 19
Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")

plot(MSFT$MSFT.Close, main = "")

# 20
Sys.setlocale("LC_TIME","C")

getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")
m <- MSFT$MSFT.Close
## Compile a dataframe
y <- as.numeric( m$MSFT.Close )
x1 <- as.numeric( stats::lag(m, -1) )
x2 <- as.numeric( stats::lag(m, -2) )
md <- data.frame(y, x1, x2)

## Compose a model
model20 <- lm(data=md, y ~ x1 + x2)
summary(model20)
