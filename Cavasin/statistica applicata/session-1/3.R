library(ISLR)
library(lattice)
library(DAAG)
source("utils.R")

attach(Default)

xyplot(income ~ balance, groups = default, data = Default)

par(mfrow = c(1, 3))

boxplot(balance ~ default, data = Default, col = c("deepskyblue", "magenta"))
boxplot(income ~ default, data = Default, col = c("deepskyblue", "magenta"))
boxplot(student ~ default, data = Default, col = c("deepskyblue", "magenta"))

par(mfrow = c(1, 1))

credit.glm1 <- glm(default ~ balance, family = binomial, data = Default)
summary(credit.glm1)

predict(credit.glm1, data.frame(balance = c(1000, 2000)), type = c("response"))

Default$studentD <- 0
Default$studentD[Default$student == "Yes"] <- 1

credit.glm2 <- glm(default ~ student, family = binomial, data = Default)
summary(credit.glm2)
logistic.plot(credit.glm2)

credit.glm3 <- glm(default ~ balance + student, family = binomial, data = Default)
summary(credit.glm3)
logistic.plot(credit.glm3)

table(as.numeric(fitted(credit.glm3) > 0.5), default)
CVbinary(credit.glm3)
