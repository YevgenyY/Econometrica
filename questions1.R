# q1
values <- c(0,2,4)
p <- c(0.6, 0.2, 0.2)

sum(values * p)

# q2
Ex <- 1
Ey <- 3
var_x <- 4
var_y <- 1
cov_xy <- 1
## E(2*X + Y)
E2x <- 2*Ex
E2x + Ey

# q3, var(3x+y)
Ex <- 1
Ey <- 3
var_x <- 2
var_y <- 4
cov_xy <- 2
# var(3*x+y)
# var(c*x) = c^2*var(x)
# var(ax+by+c) = a^2*var(x) + b^2*var(y) + 2abcov(x,y)
9*var_x + 4 + 2*3*cov_xy

# q10
TSS <- 100
RSS <- 20
ESS <- TSS-RSS
R2 <- ESS/TSS
R2

# q11
R2 <- 0.7
TSS <- 130
ESS <- R2 * TSS
RSS <- TSS - ESS
RSS

# q13
x <- c(1, 2, 3, 4, 5)
y <- c(2, 2, 1.7, 2.3, 2)
TSS <- sum( (y-mean(y))^2 )
y_hat <- mean(y)
ESS <- sum( (y-y_hat)^2 )
R2 <- ESS/TSS
R2

# q15
data(sleep)
mean(sleep$extra)

# q16
min(sleep$extra)+max(sleep$extra)

# q17
d <- sleep[5:14,]
d
round(var(d$extra),1)

# q18
data(mtcars)
model <- lm(data=mtcars, mpg~disp+hp+wt+am)
# Вычисляем остатки
RSS <- deviance(model)
RSS
y <- mtcars$mpg
TSS <- sum( (y-mean(y))^2 )
TSS
ESS <- TSS-RSS
ESS
R2 <- ESS/TSS
round(R2,2)

# q19
model <- lm(data=mtcars, mpg~disp+hp+wt+am)
coef <- model$coefficients
round(coef[4],2)

# q20
model1 <- lm(data=mtcars, mpg~disp+hp+wt+am)
model2 <- lm(data=mtcars, mpg~cyl+hp+wt+am)
model3 <- lm(data=mtcars, mpg~disp+cyl+wt+am)
model4 <- lm(data=mtcars, mpg~disp+hp+cyl+am)
R21 <- summary(model1)$r.squared
R22 <- summary(model2)$r.squared
R23 <- summary(model3)$r.squared
R24 <- summary(model4)$r.squared

vector_R2 <- c(R21, R22, R23, R24)
vector_R2
max(vector_R2)






