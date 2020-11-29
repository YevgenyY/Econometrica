library("HSAUR")
library("dplyr")
library("psych")
library("lmtest")
library("glmnet")
library("ggplot2")
library("car")

h <- cars
qplot(data=h, speed, dist)
model <- lm(data=h, dist~speed)
summary(model)

h <- mutate(h, speed2 = speed ^2, speed3 = speed^3)
model_mk <- lm(data=h, dist ~ speed + speed2 + speed3)
summary(model_mk)

# смотрим коэффициент вздутия регрессии
vif(model_mk)

# смотрим корреляцию
x0 <- model.matrix(data=h, dist~0 + speed + speed2 + speed3)
head(x0)
cor(x0) # близко к +1 -есть положительная корреляция

# доверитальный интервал для модели с мультиколлинеарностью увеличился не очень сильно
nd <- data.frame(speed=10, speed2=100, speed3=1000)
predict(model, newdata = nd, interval = "prediction")
predict(model_mk, newdata = nd, interval = "prediction")

# но если посмотреть на доверительные иинтервалы для отдельных коэффициентов
# то разница будет существенной
confint(model)
confint(model_mk)

### LASSO & Ridge ###
y <- h$dist
x0 <- model.matrix(data=h, dist~0 + speed + speed2 + speed3)

# LASSO
lambdas <- seq(50, 0.1, length=30)
m_lasso <- glmnet(x0, y, alpha=1, lambda=lambdas)

# смотрим зависимость коэффициентов от логарифмов лямбды
# первый коэф ~2 остальные 0, при росте лямбд все коэф. = 0
plot(m_lasso, xvar="lambda", label=TRUE) 

# смотрим дисперсию разброса объясняемой переменной
# видим, что в правой части графика, если мы немного жертвуем объяснённой дисперсии
# будет существенное уменьшение коэффициентов
plot(m_lasso, xvar="dev", label=TRUE)

# смотрим по горизонтали (величина штрафа - сумма модулей beta_hat), 
# по вертикали он же по коэффициентам
# видим, что первый коэффициент практически полностью определяет сумму штрафа
plot(m_lasso, xvar="norm", label=TRUE)

# смотрим коэфф
coef(m_lasso, s=c(0.1,1))

# Ridge
lambdas <- seq(50, 0.1, length=30)
m_rr <- glmnet(x0, y, alpha=0, lambda=lambdas)
plot(m_rr, xvar="lambda", label=TRUE) 
plot(m_rr, xvar="dev", label=TRUE)
plot(m_rr, xvar="norm", label=TRUE)

# Как выбрать оптимальные лямбды?
# метод кросс-валидации
cv <- cv.glmnet(x0, y, alpha=1)

# на графике выделяем два значения
# лямбда, при котором сумма квадратов ошибок минимальна
# вторая точка - лямбда, где немного пожертвовав суммой квадратов ошибок,
# можем сильно увеличить штрафы
plot(cv)
cv$lambda.min # здесь минимальна сумма квадратов ошибок
cv$lambda.1se # здесь коэфф близки к нулю

coef(cv,s="lambda.min")
coef(cv,s="lambda.1se")

### PCA ###
# смотрим набор данных по семиборью
h <- heptathlon
help(heptathlon)
glimpse(h)

h <- select(h, -score)
describe(h)

# у переменных разные единицы измерения и разный разброс
# нужно нормализовать и найти переменные, наиболее точно описывают 
# разницу между спортсменами

# смотрим корреляцию
cor(h)

h.pca <- prcomp(h, scale=TRUE)
pca1 <- h.pca$x[,1]      # выделяем новые x
v1 <- h.pca$rotation[,1] # выделяем коэффициенты при переменных в методе PCA
v1
head(pca1)

# Видим, что первая главная компонента ловит первые 63.7% дисперсии исходного набора
# первые две компоненты ловят 80%, а первые четыре компоненты, позволяют 
# описать 94% дисперсии исходного набора
summary(h.pca)

# смотрим корреляции
cor(heptathlon$score, pca1) # корреляция 0.99

# смотрим сколько дисперсии обЪясняет каждая компонента
plot(h.pca)

# смотрим результаты в осях первой и второй главных компонент
biplot(h.pca, xlim=c(-1,1))



