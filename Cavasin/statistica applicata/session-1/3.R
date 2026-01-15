library(ISLR)
library(lattice)
attach(Default)

source("utils.R")

xyplot(income ~ balance, groups = default, data = Default)

par(mfrow=c(1,2))

boxplot(default~balance, data=Default)
