onions <- read.table("onions.dat", col.names = c("yield", "dens", "location"))

par(mfrow = c(2, 2))

hist(onions$yield, freq = FALSE, main = "", ylab = "", ylim = c(0, 0.016))
curve(dnorm(x, mean(onions$yield), sd(onions$yield)), add = TRUE, lwd = 2, col = "red")

qqnorm(onions$yield, main = "", xlab = "", ylab = "")
qqline(onions$yield, lwd = 2, col = "red")


hist(onions$dens, freq = FALSE, main = "", ylab = "", ylim = c(0, 0.010))
curve(dnorm(x, mean(onions$dens), sd(onions$dens)), add = TRUE, lwd = 2, col = "red")

qqnorm(onions$dens, main = "", xlab = "", ylab = "")
qqline(onions$dens, lwd = 2, col = "red")

par(mfrow = c(1, 1))

(41 + 30 + 43 + 34 + (1)) / 322 * 100

ppoints
