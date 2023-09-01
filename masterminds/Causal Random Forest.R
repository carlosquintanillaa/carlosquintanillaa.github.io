library(grf)
library(tidyverse)

datos = read_csv('/Users/carlosq/Desktop/het1.csv')

X = as.matrix(datos[,1:5])
W = datos$t
Y = datos$y

tau.forest <- causal_forest(X,Y,W)

tau.hat.oob <- predict(tau.forest)
hist(tau.hat.oob$predictions)

testdata = read_csv('/Users/carlosq/Desktop/het2.csv')
Xtest = as.matrix(testdata)

tau.hat <- predict(tau.forest, Xtest)

plot(Xtest[, 2], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "p")
reg1 = lm(tau.hat$predictions~Xtest[,2])
abline(reg1)

# Estimate the conditional average treatment effect on the full sample (CATE).
average_treatment_effect(tau.forest, target.sample = "control")


train <- sample(1:nrow(X), nrow(X) / 2)
cf.priority <- causal_forest(X[train, ], Y[train], W[train])
priority.cate <- predict(cf.priority, X[-train, ])$predictions
cf.eval <- causal_forest(X[-train, ], Y[-train], W[-train])
rate <- rank_average_treatment_effect(cf.eval, priority.cate)
rate
plot(rate, las=1)
