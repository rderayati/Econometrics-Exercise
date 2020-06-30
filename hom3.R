rm(list = ls())

library(AER)
library(wooldridge)
library(gmm)
library(stargazer)
library(psych)
library(readr)
hprice <- read_csv("C:/Users/utente/Desktop/hprice.csv")
View(hprice)

hprice81 <- hprice[which(hprice$year=='1981'),]
biv_mod81<-lm(log(price) ~ ldist, data=hprice81)
summary(biv_mod81)

y81=dummy.code(hprice$year,1981,na.rm=TRUE)

pooled_mod<-lm(log(price) ~ y81+ldist+(y81*ldist),data=hprice)
summary(pooled_mod)

pooled_mod1<-lm(log(price) ~ y81+ldist+(y81*ldist)+linst+linstsq
 +log(area)+log(land)+age+I(age^2)+rooms+baths,data=hprice)
summary(pooled_mod1)

