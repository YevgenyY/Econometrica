library("sandwich")
library("dplyr")
library("lmtest")
library("broom")
library("ggplot2")
library("car")

h <- read.table("flats_moscow.txt", header=TRUE)

# наблюдаем, что при малой площади (totsp) разброс цены тоже небольшой, 
# при большой площади - разброс цены большой
# гетероскедастичность
qplot(data=h, x=totsp, y=price)

model <- lm(data=h, price~totsp)
summary(model)

coeftest(model)

# смотрим на доверительные интервалы коэффициентов, 
# помним, что они считаются по формулам для данных с гетероскедастичностью
# и здесь не годятся
confint(model)

# смотрим на оценку ковариационной матрицы для моделей
vcov(model)

# провери наличие гетероскедастичности
h <- augment(model, h)
glimpse(h)
# видим, что в дополненных данных размер остатка увеличивается при росте totsp
qplot(data=h, totsp, abs(.resid))

# смотрим на разницу в оценке коэффициентах, вычисленные по формулам 
# без гетероскедастичности и с учётом гетероскедастичности
vcov(model)
vcovHC(model, type="HC2")

coeftest(model)
coeftest(model, vcov. = vcovHC(model))

# построим доверительные интервалы устойчивые к гетероскедастичности
conftable <- coeftest(model, vcov. = vcovHC(model))
ci <- data.frame(estimate=conftable[,1],se_hc=conftable[,2])

ci <- mutate(ci, left_ci=estimate-1.96*se_hc, 
                 right_ci=estimate + 1.96*se_hc)
ci

# сравним с оценкой без учёта HC
confint(model)

# формальные тесты на гетероскедастичность

# Тест Уайта
# H0 = имеет место гетероскедастичность
bptest(model)
bptest(model, data=h, varformula = ~ totsp + I(totsp^2))
# another syntax
bptest(model, data=h, varformula = ~ poly(totsp, 2))

# тест голдфельба-квандта
gqtest(model, order.by = ~ totsp, data=h, fraction = 0.2)