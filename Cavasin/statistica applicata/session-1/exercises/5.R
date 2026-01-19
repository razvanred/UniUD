library("DAAG")
source("utils.R")

cars.lm1 <- lm(dist ~ speed + I(speed^2), data = cars)
par(mfrow = c(3, 2))
plot(cars.lm1, which = 1:6)
summary(cars.lm1)

# dat <- data.frame(speed = seq(3, 25, length = 100))
# fv <- predict(cars.lm1, newdata = dat, se = TRUE)
# fv <- predict(cars.lm1, se = TRUE)

# plot(dist ~ speed, data = cars, pch = 16)
# lines(cars$speed, fv$fit, col = "red")

vif(cars.lm1)

cars.lm2 <- lm(dist ~ speed + I(speed^2), weights = 1 / speed, data = cars)
plot(cars.lm2, which = 1:6)
summary(cars.lm2)
