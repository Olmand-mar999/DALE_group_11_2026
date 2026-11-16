library(ordinal)
library(tidyverse)

wine <- wine
fm1 <-  clm(rating ~ temp + contact, data=wine)
summary(fm1)

anova(fm1, type="III")

fm2 <- clm(rating ~ temp, data=wine)
fm3 <- clm(rating ~ contact, data=wine)
anova(fm2, fm1)
summary(fm2)
summary(fm3)

drop1(fm1, test="Chi")

fm.nom <- clm(rating ~ temp, nominal = ~ contact, data=wine)
summary(fm.nom)
