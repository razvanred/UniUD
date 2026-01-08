t.onesample <- function(
  x,
  # y = NULL,
  # alternative = c("two.sided", "less", "greater"),
  mu = NULL,
  # paired = FALSE,
  # var.equal = FALSE,
  conf.level = 0.95, ...
) {
    res <- t.test(x, mu = (if (is.null(mu)) 0 else mu), conf.level = conf.level)
    df <- res$parameter
    int <- res$conf.int
    stat <- res$statistic

    dnor <- function(normal) {
        normal * res$stderr + res$estimate
    }

    nor <- function(x) {
        (x - res$estimate) / res$stderr
    }


    xs <- if (is.null(mu)) {
        list(min = min(x), max = max(x))
    } else {
        list(min = min(x, dnor(stat), mu), max = max(x, dnor(stat), mu))
    }
    xx <- seq(xs$min, xs$max, length.out = 1000)

    plot(xx, dt(nor(xx), df),
        type = "l", lwd = 2,
        xlab = " ", ylab = " "
    )

    cord.x <- c(xs$min, seq(xs$min, int[1], length.out = 500), int[1])
    cord.y <- c(0, dt(nor(seq(xs$min, int[1], length.out = 500)), df), 0)
    polygon(cord.x, cord.y, col = "skyblue")
    cord.x <- c(xs$max, seq(xs$max, int[2], length.out = 500), int[2])
    cord.y <- c(0, dt(nor(seq(xs$max, int[2], length.out = 500)), df), 0)
    polygon(cord.x, cord.y, col = "skyblue")
    abline(0, 0, lwd = 2)
    points(x, rep(0, length.out = length(x)), pch = "|", cex = 0.8)
    points(res$estimate, 0, pch = "|", col = "tomato1")


    if (!is.null(mu)) {
        abline(v = mu, col = "tomato1")
        points(dnor(stat), 0, pch = 16, cex = 1.2, col = "red")

        xx <- seq(max(stat, -stat), nor(xs$max), length.out = 500)
        lines(dnor(xx), dt(xx, df), lwd = 2, col = "red")
        xx <- seq(min(stat, -stat), nor(xs$min), length.out = 500)
        lines(dnor(xx), dt(xx, df), lwd = 2, col = "red")
    }
}

y1 <- rnorm(40, mean = 30)
t.onesample(y1, mu = 30.5)
