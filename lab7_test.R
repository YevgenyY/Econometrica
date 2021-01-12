# 1 LR-stat
2*(-300 + 310)
qchisq(p=0.99, df=1)

# 2 probit model
# y_hat_start =2 - 0.3*x_i + eps_i # это оценка скрытой переменной модели, которая максимизируется
# eps_i ~ N(0, 1)
# 2 Calculate Probability
# вычисляем значение функции в точке b_1 + b_2*x_i

p <- pnorm(2 - 0.3*6, 0, 1)
round(p, 2)

# 3
# I = -l''(theta_hat)
E <- 7
var_theta_hat <- 1/25
z_crit <- qnorm(0.975, mean=0, sd=1) # 1.96 and we use 0.975 as N(0,1) has two tails
left_ci <- E - z_crit * sqrt(var_theta_hat)
right_ci <- E + z_crit * sqrt(var_theta_hat)
left_ci
right_ci

# 4
# y_hat = -1 + 0.25*x_i + 0.05*z_i
# find x_i for max( dP(y_i = 1)/dz) if z_i = 10
# используем правило вычисление производной сложной функции
# dP(y_i=1) = dF(-1+0.25x_i+0.05z_i)/dz = f(-1+0.25x_i+0.05z_i)*0.05
# Найти x_i при котором предельный эффект (производная) максимален
u <- seq(from = -10, to=10, by=0.1)
y <- exp(-u)/(1+exp(-u))^2
qplot(u, y, geom="line")

# u should be zero then
# -1 + 0.25*x + 0.05*10 = 0 => x=2


# 5
# ln( f(x) )

# 6
# L(a) = nln(a) + (a-1)sum( ln(x) ) - an(ln(2)) 
# mul(x_i) = 7.78 * 10^(-20)
# calculate derivative by a and find extremum
p <- 7.78 * ( 10^-20)
n <- 100
a_hat <- n / ( n*log(2) - log(p) )
a_hat

# 11
require(graphics)
## Full mosaic
mosaicplot(HairEyeColor)
