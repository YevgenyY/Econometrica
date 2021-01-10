# 1 LR-stat
2*(-300 + 310)
qchisq(p=0.99, df=1)

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