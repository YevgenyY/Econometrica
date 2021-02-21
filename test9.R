library("dplyr")
library("diamonds")
library("ggplot2")
library("caret")
library("AER")
library("sandwich")
library("ivpack")
library("memisc")

# 17
h <- diamonds
head(h)
set.seed(12345)
train_ind <- createDataPartition(h$price, p=0.8, list=FALSE) 
h_train <- h[train_ind,] 
h_test <- h[-train_ind,]
nrow(h_train)
nrow(h_test)
nrow(h)

model_1 <- lm(data=h_train, log(price)~log(carat)+log(depth)+clarity)
summary(model_1)
y <- h_test$price
y
y_hat_1 <- exp(predict(model_1, h_test))
y_hat_1
sum( (y - y_hat_1)^2 ) / 10^9


# 18
data(CollegeDistance)
h <- CollegeDistance
head(h)

model_iv <- ivreg(data=h, 
    wage ~ education + gender + region + unemp | distance + gender + region + unemp)
coeftest(model_iv, vcov=vcovHC) 
mtable(model_iv)
summary(model_iv)
