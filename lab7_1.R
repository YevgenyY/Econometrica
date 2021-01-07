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

vcov(m_logit)

# Прогнозирование
newdata <- data.frame(age=seq(from=5,to=100), length=100, sex="male", pclass=as.factor(2), fare=100)

pr_logit <- predict(m_logit, newdata, se=TRUE)
newdata_pr <- cbind(newdata, pr_logit)
head(newdata_pr)

newdata_pr <- mutate(newdata_pr, prob=plogis(fit), 
                     left_ci=plogis(fit-1.96*se.fit),
                     right_ci=plogis(fit+1.96*se.fit))

# Строим график вероятности от возраста с доверительными интервалами
qplot(data=newdata_pr, x=age, y=prob,geom="line") + 
  geom_ribbon(aes(ymin=left_ci, ymax=right_ci), alpha=0.2)


# Вторая модель
t2 <- select(t, sex, age, pclass, survived, fare) %>% na.omit
head(t2)

m_logit2 <- glm(data=t2, survived ~ sex +age, family=binomial(lin="logit"),x=TRUE)

# Сравним две моделиб H0: они одинаковые
# p-value ~0 - H0 - отвергаем
lrtest(m_logit, m_logit2)
