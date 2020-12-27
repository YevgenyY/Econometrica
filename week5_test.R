#1 - ряды

#2 - адекватная модель

#3 - 35 000

#4 - R^2=0.4, RSS=120, find ESS
# R^2 = ESS/TSS
# TSS = RSS + ESS
Rsq <- 0.4
RSS <- 120
TSS <- RSS/(1-Rsq)
ESS <- TSS - RSS
ESS

#5
df <- ChickWeight
glimpse(df)
db10 <- df[df$Time==10,]
head(db10)
round(mean(db10$weight),2)

#6
df21 <- df[df$Time==21,]
head(df21)
aggregate(df21$weight, by=list(diet=df21$Diet), FUN=mean)

#7
model <- lm(data=df, weight ~ Time + Diet)
summary(model)

#8
# edu = -145 + 0.08*income - 0.20*urban

#9
# Y_h = 20 + 5x + 6z
# H: b2 + b3 = 10
# se(b2) = 0.7, se(b3) = 0.25
# test - stat = ((b2_hat+b3_hat) - (b2+b3)) / sqrt( var(b2+b3) )
n = 5000
b2 <- 5
b3 <- 6
seb2 <- 0.7; var_b2 <- seb2^2
seb3 <- 0.5; var_b3 <- seb3^2
covb2b3 <- 0.25

var_b2b3 <- var_b2 + var_b3 + 2*var_b2*var_b3*covb2b3
res <- (b2+b3-10) / sqrt(var_b2b3)
res


#13
library('ggplot2')
diamonds
df <- diamonds

model <- lm(data=df, price ~ carat + table +x +y +z +depth)
summary(model)

#14
model <- lm(data=df, price ~ carat + table +x +y +depth)
summary(model)
confint(model)

#15
# RSS_ur > RSS_r
# TSS_ur = TSS_r = SUM(y - mean(y))

#16
# wage0 = 500 + 300mschooling + 100educ
# wage1 = b0 + b1*mschoolong + b2*educ + b3 * fschooling + b4 * fincome

ESS_r = 150
ESS_ur = 190
TSS = 230
# F = ( (RSS_R - RSS_UR)/r ) / (RSS_UR / (n - k_ur) )

RSS_r = TSS - ESS_r
RSS_ur = TSS - ESS_ur

n <- 75
r <- 2 # b3 & b4
k_ur <- 5 # b0, b1, b2, b3, b4
F <- ( (RSS_r - RSS_ur)/r ) / (RSS_ur/(n - k_ur) )

#19
library("Ecdat")
data("BudgetFood")
df <- BudgetFood

model <- lm(data=df, wfood ~ totexp + size)
nd <- data.frame(totexp=700000, size=4)

predict(model, nd, interval="prediction", level=0.9)

#20
library(lmtest)
model <- lm(data=df, wfood ~ totexp + size)
resettest(model)

#21
library(memisc)
h <- na.omit(BudgetFood)
model_0 <- lm(data=h, wfood ~ totexp + size)
model_1 <-lm(data=h, wfood ~ totexp*sex + size*sex)
#model_1 <-lm(data=h, wfood ~ totexp + size + sex)
summary(model_1)
anova(model_0, model_1)



#22
# 4.1.1

#23

#24
ESS <- 105
TSS <- 200

R2_1 <- 0.8
R2_2 <- 0.65
R2_3 <- 0.7

vif <- function(r2) {
  res <- 1/(1-r2)
  
  return(res)
}
vif(R2_1)
vif(R2_2)
vif(R2_3)

#25
# 4.1.3

#26
#    disp       hp       wt 
# 7.324517 2.736633 4.844618 
df <- mtcars
model <- lm(data=df, mpg ~ disp + hp + wt)

model1 <- lm(data=df, disp ~ hp + wt )
model2 <- lm(data=df, hp ~ disp + wt)
model3 <- lm(data=df, wt ~ disp + hp)
vif( summary(model1)$r.squared )
vif( summary(model2)$r.squared )
vif( summary(model3)$r.squared )

#27
df <- mtcars[,c(3,4,6)]
head(df)
df.pca <- prcomp(df, scale=TRUE)
pca1 <- df.pca$x[,1]
pca2 <- df.pca$x[,2]
pca3 <- df.pca$x[,3]
v1 <- df.pca$rotation[,1]
len <- sqrt(sum(pca1^2))
round(len, 2)

#28
summary(df.pca)
df$mpg <- mtcars$mpg
df$pca1 <- pca1
df$pca2 <- pca2
df$pca3 <- pca3

#plot(df.pca)
#biplot(df.pca)

model0 <- lm(data=df, mpg ~ pca1 + pca2)
model1 <- lm(data=df, mpg ~ pca1 + pca2 + pca3)

r2_0 <- summary(model0)$r.squared
r2_1 <- summary(model1)$r.squared

r2_1 - r2_0


