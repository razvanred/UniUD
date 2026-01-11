ambient <- c(254, 252, 239, 240, 250, 256, 267, 249, 259, 269)
heated <- c(233, 252, 237, 246, 255, 244, 248, 242, 217, 257, 254)

mean(ambient) - mean(heated)

(var(ambient) * (length(ambient) - 1) + var(heated) * (length(heated) - 1)) / (length(ambient) + length(heated) - 2)

s2p <- weighted.mean(c(var(ambient), var(heated)), c(length(ambient) - 1, length(heated) - 1))
s2p

y1 <- rnorm(15, mean = 30)
t.test(y1, mu = 30)
dt()

pnorm()

pchisq()