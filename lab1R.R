library("psych")
library("dplyr")
library("ggplot2")
library("GGally")

d <- cars
glimpse(d)
describe(d)
ncol(d)
nrows(d)
str(d)

# переводим мили в час в км в час, футы в метры
d2 <- mutate(d, speed=1.67*speed, dist=0.3*dist, ratio=dist/speed)
glimpse(d2)

# график для себя
qplot(data=d2, dist)

# график для публикации
qplot(data=d2, dist, xlab="Длина тормозного пути (м)",
      ylab="Кол-во машин",main="Данные 1920х годов")

qplot(data=d2, speed, dist)

model <- lm(data=d2, dist~speed)
model

# извлекаем из модели вектор коэффициентов
beta_hat <- coef(model)
eps_hat <- residuals(model)
eps_hat
y <- d2$dist
y_hat <- fitted(model)
y_hat

# Вычисляем остатки
RSS <- deviance(model)
RSS
TSS <- sum( (y-mean(y))^2 )
TSS
ESS <- TSS-RSS
ESS

# вычисляем R2 двумя способами
R2 <- ESS/TSS
R2
# второй
R2 <- cor(y, y_hat)^2
R2

# извлекаем матрицу X из модели
X <- model.matrix(model)
X

# прогнозируем тормозной пусть от скорости для 40 и 60 км/ч
nd <- data.frame(speed=c(40,60))
nd
predict(model, nd)

# добавляе к графику сглаживающую кривую
qplot(data=d2, speed, dist) + stat_smooth(method="lm")




