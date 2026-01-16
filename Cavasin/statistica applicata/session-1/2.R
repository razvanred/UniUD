source("utils.R")
par(mfrow = c(1, 1))

program <- matrix(c(
    1, 2.66, 20, 0, 0, 2, 2.89, 22, 0, 0, 3, 3.28, 24, 0, 0, 4, 2.92, 12, 0,
    0, 5, 4.00, 21, 0, 1, 6, 2.86, 17, 0, 0, 7, 2.76, 17, 0, 0, 8, 2.87, 21,
    0, 0, 9, 3.03, 25, 0, 0, 10, 3.92, 29, 0, 1, 11, 2.63, 20, 0, 0, 12, 3.32,
    23, 0, 0, 13, 3.57, 23, 0, 0, 14, 3.26, 25, 0, 1, 15, 3.53, 26, 0, 0, 16,
    2.74, 19, 0, 0, 17, 2.75, 25, 0, 0, 18, 2.83, 19, 0, 0, 19, 3.12, 23, 1,
    0, 20, 3.16, 25, 1, 1, 21, 2.06, 22, 1, 0, 22, 3.62, 28, 1, 1, 23, 2.89,
    14, 1, 0, 24, 3.51, 26, 1, 0, 25, 3.54, 24, 1, 1, 26, 2.83, 27, 1, 1, 27,
    3.39, 17, 1, 1, 28, 2.67, 24, 1, 0, 29, 3.65, 21, 1, 1, 30, 4.00, 23, 1,
    1, 31, 3.10, 21, 1, 0, 32, 2.39, 19, 1, 1
), nrow = 32, byrow = T)
colnames(program) <- c("OBS", "GPA", "TUCE", "PSI", "GRADE")
program <- as.data.frame(program)

mod1.lm <- lm(GRADE ~ GPA + TUCE + PSI, data = program)
logistic.plot(mod1.lm)

par(mfrow = c(3, 2))

mod2.glm <- glm(GRADE ~ PSI, family = binomial, data = program)
summary(mod2.glm)
logistic.plot(mod2.glm, par = FALSE)

mod3.glm <- glm(GRADE ~ GPA + TUCE + PSI, family = binomial, data = program)
logistic.plot(mod3.glm, par = FALSE)
summary(mod3.glm)

mod4.glm <- glm(GRADE ~ GPA + PSI, family = binomial, data = program)
logistic.plot(mod4.glm, par = FALSE)
summary(mod4.glm)

# predict gives raw log(odds) values, type must be specified
predicted <- predict(mod4.glm, type = c("response"))

table(as.numeric(predicted >= 0.5), program$GRADE)
