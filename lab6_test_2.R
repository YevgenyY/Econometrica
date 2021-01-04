library("devtools")

library("lubridate") # работа с датами

library("sandwich") #vcovHC, vcovHAC
library("lmtest") # tests
library("car") # more tests
library("bstats") # more more tests
library("zoo") # временные ряды
library("xts") # ещё ряды
library("dplyr") # манипуляции с данными
library("broom") # просто манипуляции
library("ggplot2") # графики

library("rlms")
h <- rlms_read("data/r22i_os_32.sav")

sal <- h$rj13.2
age <- 2013 - h$rh6
sex <- h$rh5
edu <- h$r_diplom
ltn <- h$status
stf <- h$rj1.1.1

df <- data.frame(sal, age, sex, edu, ltn, stf)
df <- subset(df, ltn==1 | ltn==2 | is.na(ltn))
df <- subset(df, stf==1 | stf==2 | is.na(stf))
df <- subset(df, (edu>=1 & edu <=6) | is.na(edu))

df$ltn[df$ltn==1] <- 0 # it is a village
df$ltn[df$ltn==2] <- 1 # it is a city
df$status <- as.factor(df$ltn)

df$stf[df$stf == 2] <- 0 # 0 - rather satisfied, 1 - fully satisfied
df$happiness <- as.factor(df$stf)

df$sex[df$sex==2] <- 0
df$sex <- as.factor(df$sex)

df$edu[df$edu <= 3] <- 0 # незаконченное среднее
df$edu[df$edu == 4] <- 1 # законченное среднее
df$edu[df$edu == 5] <- 2 # законченное среднее специальное
df$edu[df$edu == 6] <- 3 # законченное высшее
df$edu <- as.factor(df$edu)
df <- select(df, -c(ltn, stf))

h <- df[complete.cases(df),]
h <- select(h, -c(ltn, stf))

# We are working with h now
# 1
max(df$age)

# 2
s <- df$sal
sum(is.na(s))

# 3
s <- h$sal
qplot(s, geom="histogram")
summary(s)

# 4
qplot(data=h, sal/1000, geom="histogram", fill=sex) + facet_grid(~sex) + 
  xlim(c(1,100)) 
  
qplot(data=h, age, geom="histogram", fill=sex) + facet_grid(~sex) + 
  xlim(c(1,100)) 

# 5
# set all dummy variables
h$hs_edu[h$edu==1] <- 1
h$hs_edu[h$edu!=1] <- 0

h$spec_edu[h$edu==2] <- 1
h$spec_edu[h$edu!=2] <- 0

h$high_edu[h$edu==3] <- 1
h$high_edu[h$edu!=3] <- 0

h$city[h$status==1] <- 1
h$city[h$status!=1] <- 0

h$satisfied[h$happiness==1] <- 1
h$satisfied[h$happiness!=1] <- 0

model5 <- lm(data=h, sal ~ age + sex + hs_edu + 
               spec_edu + high_edu + city + satisfied)
summary(model5)

coeftest(model5, vcov. = vcovHC(model5))
