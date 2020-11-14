library("psych")
library("dplyr")
library("memisc")
library("ggplot2")
library("lmtest")
library("foreign")
library("vcd") # визуализация множества переменных на одном графике
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

h <- diamonds
glimpse(h)

qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex()

f <- read.csv("../data/flats_moscow.txt", sep="\t", header=TRUE, dec=".")
glimpse(f)
qplot(data=f, totsp, price)
qplot(data=f, log(totsp), log(price))
mosaic(data=f, ~walk+brick+floor, shade=TRUE)

f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data=f, log(price))
qplot(data=f, log(price), fill=brick)
qplot(data=f, log(price), fill=brick, position = "dodge")
qplot(data=f, log(price), fill=brick, geom="density")
qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)
g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

# МНК
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))

summary(model_0)
mtable(model_2)
sjp.lm(model_2)


