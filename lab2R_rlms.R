library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("devtools")
library("rlms")
library("hexbin")
library("Sys")

# devtools::install_github("bdemeshev/rlms")

up <- Sys.getenv("USERPROFILE")
cwd <- paste(up, "\\Documents\\MyR\\CourseraEconometrica", sep='')
setwd(cwd)

#datafile <- paste(up, "\\Documents\\MyR\\data\\r28i_os_32\\r28i_os_32.sav", sep='')
datafile <- paste(up, "\\Documents\\MyR\\data\\r21iall_32.sav", sep='')

h <- rlms_read(datafile)

# выбираем переменные
h2 <- select(h, qm1, qm2, qh6, qh5)
describe(h2)

h3 <- rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
h3 <- mutate(h3, vozrast =2012 - b_year)
describe(h3)

# выбираем наблюдения
summary(h3$sex)
h4 <- filter(h3, sex==1)

# график
qplot(data=h4, rost, ves)
qplot(data=h4, ves)
