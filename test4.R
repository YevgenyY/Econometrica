# 1 - pension

# 8 - 
1/(1-0.75)

# 10 - уменшьшить число регрессоров

# 15
h <- airquality
glimpse(h)
qplot(data=h, Ozone, Temp)

# 16
model <- lm(data=h, Ozone ~ Solar.R + Wind + Temp)
summary(model)
vif(model)

# 17 LASSO
ho <- na.omit(h)
y <- ho$Ozone
x0 <- model.matrix(data=ho, Ozone~0 + Solar.R + Wind + Temp)

lambdas <- seq(50, 0.1, length=30)

m_lasso <- glmnet(x0, y, alpha=1, lambda=lambdas)
summary(m_lasso)
coef(m_lasso, s=1)

# 18 Ridge
ho <- na.omit(h)
y <- ho$Ozone
x0 <- model.matrix(data=ho, Ozone~0 + Solar.R + Wind + Temp)

lambdas <- seq(50, 0.1, length=30)

m_ridge <- glmnet(x0, y, alpha=0, lambda=lambdas)
summary(m_ridge)
coef(m_ridge, s=2)

# 19
plot(m_lasso, xvar="norm", label=TRUE)
plot(m_ridge, xvar="norm", label=TRUE)

# 20
h.pca <- prcomp(x0, scale=TRUE)
p1 <- h.pca$x[,5]      # выделяем новые x
p2 <- h.pca$x[,6]

qplot(h.pca$x[,2], h.pca$x[,3])

