
library(ISLR2)
ISLR2
Default
glmtest <- glm(default~student+balance+income, family="binomial", data=Default)
summary(Default$default)
a.glm <- predict(glmtest, type = ("response"))
summary(glmtest)
a.glm


glmtest_ny <- glm(default~student+balance, family="binomial", data=Default)
summary(glmtest_ny)

