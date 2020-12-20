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

