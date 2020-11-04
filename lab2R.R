library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# генерируем случайные величины
# Z_1, ...... Z_100 ~ N(5, 9)

z <- rnorm(100, mean=5, sd=3)
qplot(z)

# построим функцию плотности
x <- seq(-10, 15, by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y,geom="line")

# посчитаем вероятность того, что z < 3
# P(Z<3) = F(3)
pnorm(3, mean=5, sd=3)

# P(Z in [4:9])
# P(Z<9 - P(Z<4))
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# найдём квантиль распределения
# P(Z<a) = 0.7, a?
qnorm(0.7, mean=5, sd=3)

# другие распределения
# rchisq, dchisq, pchisq, qchisq - функции для распределения хи квадрат
# rt, dt, pt, qt - функции для t-распределения
# r - генерирование распределения
# d - плотость
# p - вероятность
# q - квантиль

# множественная регрессия, проверка гипотез
h <- swiss
glimpse(h)
help(swiss)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
coeftest(model) # выводим коэффициенты
confint(model) # выводим доверительный интрервал
sjp.lm(model)

# проверка линейных гипотез b_Cath = b_Agri
# способ с построением вспомогательной регрессии
model_aux <- lm(data=h, 
                Fertility~Catholic + I(Catholic+Agriculture) + Examination)
summary(model_aux)
linearHypothesis(model, "Catholic-Agriculture=0")

# стандартизированные коэффициенты
h_st <- mutate_each(h, "scale")
glimpse(h_st)

model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
sjp.lm(model_st)

# искусственный эксперимент
D <- matrix(nrow=100, rnorm(100*41,mean=0,sd=1))
df <- data.frame(D)

model_pusto <- lm(data=df, X1~.) #  X1 объясяется всеми остальными переменными
summary(model_pusto)

# сравним несколько моделей
model2 <- lm(data=h, Fertility~Catholic+Agriculture)
summary(model2)
compar_12 <- mtable(model, model2)
compar_12

# сохраним результаты работы
stuff <- list(data=h, model=model2)
saveRDS(file="mydata.RDS", stuff)
# mylist <- readRDS("mydata.RDS")