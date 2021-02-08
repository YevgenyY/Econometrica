# 9
y100 <- 11
y99 <- 12

y101 <- 10 + 0.6*y100 + 0.3*y99
y101

# 12

set.seed(2)
y <- arima.sim(n=100, list(ar=0.99))

tsdisplay(y)
x1 <- rev(seq(1:100))
x2 <- x1^2
x3 <- x1^3
df <- data.frame(y,x1,x2,x3)
head(df)
model1 <- lm(data=df, y~x1+x2+x3)
summary(model1)

# 13
set.seed(5)
y <- arima.sim(n=100, list(ar=0.5))
tsdisplay(y)

# 14
y <-  hhi_q_i[1:89,1]
mod_1 <- Arima(y, order=c(3,0,0))
summary(mod_1); AIC(mod_1)

mod_2 <- Arima(y, order=c(1,0,0))
summary(mod_2); AIC(mod_2)

mod_3 <- Arima(y, order=c(2,0,0))
summary(mod_3); AIC(mod_3)

mod_4 <- Arima(y, order=c(1,1,0))
summary(mod_4); AIC(mod_4)

# 15
y <-  hhi_q_i[1:29,1]
mod_a <- auto.arima(y)
AIC(mod_a)
summary(mod_a)

# 16
y <-  hhi_q_i[1:89,1]
mod_16 <- Arima(y, order=c(2,1,0))
summary(mod_16)
prognoz_16 <- forecast(mod_16, h=3)
plot(prognoz_16)

# 17
library("hydroGOF")
y_true <-  hhi_q_i[1:89,1]
y <-  hhi_q_i[1:86,1]
mod1 <- Arima(y, order=c(2,1,2))
mod2 <- Arima(y, order=c(1,1,3))
mod3 <- Arima(y, order=c(1,1,2))
mod4 <- Arima(y, order=c(0,1,0))

p1 <- forecast(mod1, 3)
p2 <- forecast(mod2, 3)
p3 <- forecast(mod3, 3)
p4 <- forecast(mod4, 3)
p1

y3 <- as.numeric(y_true[89])
y2 <- as.numeric(y_true[88])
y1 <- as.numeric(y_true[87])
y_hat1 <- as.numeric(p1$mean)
y_hat2 <- as.numeric(p2$mean)
y_hat3 <- as.numeric(p3$mean)
y_hat4 <- as.numeric(p4$mean)

mse_calc <- function(y_hat, y1, y2, y3) {
  r1 <- (y_hat[1] - y1)^2
  r2 <- (y_hat[2] - y2)^2
  r3 <- (y_hat[3] - y3)^2
  
  mse <- (r1 + r2 + r3)/3
  return(mse)
    
}
mse_calc(y_hat1, y1, y2, y3)
mse_calc(y_hat2, y1, y2, y3)
mse_calc(y_hat3, y1, y2, y3)
mse_calc(y_hat4, y1, y2, y3)

# 18
mod_s <- Arima(y_true, order=c(1,1,1), seasonal=c(1,0,0))
summary(mod_s)
mod_s <- Arima(y, order=c(1,0,0), seasonal=c(1,0,0))
p_s <- forecast(mod_s, 3)
p_s
y_hat5 <- as.numeric(p_s$mean)
mse_calc(y_hat5, y1, y2, y3)

# 19
dummy <- rep(0, 89)
dummy[62:69] <- 1
y <-  hhi_q_i[1:89,1]
mod_19 <- Arima(y_true, order=c(1,1,1), xreg = dummy)
summary(mod_19)

# 20
set.seed(30)

y1 <- arima.sim(n=100, list(ar=0.7))

plot(y1,type="l",axes=T, ylab = "variable Y")

rect(20,-1000,25,1000,col="#FFCCEE",border="#FFCCEE")

rect(70,-1000,80,1000,col="#FFCCEE",border="#FFCCEE")

par(new=TRUE)

plot(y1,type="l",ylab="")
