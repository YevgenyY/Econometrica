library("devtools")
library("dplyr")
library("erer")
library("vcd")
library("ggplot2")
library("reshape")
library("AUC")

options(stringsAsFactors = FALSE)

t <- read.csv("data/titanic3.csv")
glimpse(t)

t <- mutate(t, sex=as.factor(sex), pclass=as.factor(pclass), 
            survived=as.factor(survived))
glimpse(t)
summary(t)

mosaic(data=t, ~ sex + pclass + survived, shade=TRUE)
qplot(data=t, x=survived, y=age, geom="violin")
qplot(data=t, x=survived, y=age, geom="boxplot")

# наложим функции плотности
qplot(data=t, x=age, y=..count.., fill=survived, geom="density", position="stack")
qplot(data=t, x=age, y=..count.., fill=survived, geom="density", position="fill")

# Оцениывние моделей
m_logit <- glm(data=t, survived ~ sex +age + pclass +fare,
               family=binomial(link="logit"), x=TRUE)

m_probit <- glm(data=t, survived ~ sex +age + pclass +fare,
               family=binomial(link="probit"), x=TRUE)

summary(m_logit)
summary(m_probit)
