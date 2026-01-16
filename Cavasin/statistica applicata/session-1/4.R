source("utils.R")

odds.ratio <- function(x) {
    x[1, 1] * x[2, 2] / (x[1, 2] * x[2, 1])
}

apply(UCBAdmissions, 3, odds.ratio)

odds.ratio(margin.table(UCBAdmissions, c(1, 2)))

UCB <- setNames(as.data.frame.table(UCBAdmissions["Admitted", , ]), c("gender", "dept", "admit"))
UCB$reject <- as.data.frame.table(UCBAdmissions["Rejected", , ])$Freq
UCB$gender <- relevel(UCB$gender, ref = "Male")
UCB$total <- UCB$admit + UCB$reject
UCB$p <- UCB$admit / UCB$total

UCB.glm1 <- glm(cbind(admit, reject) ~ dept + gender,
    family = binomial,
    data = UCB
)
summary(UCB.glm1)
anova(UCB.glm1)

UCB.glm2 <- glm(cbind(admit, reject) ~ gender + dept,
    family = binomial,
    data = UCB
)
anova(UCB.glm2)

UCB.glm3 <- glm(cbind(admit, reject) ~ dept * gender,
    family = binomial,
    data = UCB
)
summary(UCB.glm3)
anova(UCB.glm3)
