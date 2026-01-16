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
    curve(dt(nor(x), df),
        n = 200, xlim = c(xs$min, xs$max),
        lwd = 2, xlab = " ", ylab = " "
    )

    cord.x <- c(xs$min, seq(xs$min, int[1], length.out = 100), int[1])
    cord.y <- c(0, dt(nor(seq(xs$min, int[1], length.out = 100)), df), 0)
    polygon(cord.x, cord.y, col = "skyblue")
    cord.x <- c(xs$max, seq(xs$max, int[2], length.out = 100), int[2])
    cord.y <- c(0, dt(nor(seq(xs$max, int[2], length.out = 100)), df), 0)
    polygon(cord.x, cord.y, col = "skyblue")
    abline(0, 0, lwd = 2)
    points(x, rep(0, length.out = length(x)), pch = "|", cex = 0.8)
    points(res$estimate, 0, pch = "|", col = "tomato1")


    if (!is.null(mu)) {
        abline(v = mu, col = "tomato1")
        points(dnor(stat), 0, pch = 16, cex = 1.2, col = "red")

        curve(dt(nor(x), df),
            xlim = c(dnor(max(stat, -stat)), xs$max),
            add = TRUE, lwd = 2, col = "red"
        )

        curve(dt(nor(x), df),
            xlim = c(dnor(min(stat, -stat)), xs$min),
            add = TRUE, lwd = 2, col = "red"
        )
    }
}

lm.intervals <- function(mod, inverse_expr = x, xlab = NULL, ylab = NULL) {
    closure <- substitute(inverse_expr)
    f <- function(x) {
        eval(closure, envir = list(x = x), enclos = parent.frame())
    }

    xy <- model.frame(mod)
    names <- list(y = colnames(xy)[1], x = colnames(xy)[2])
    colnames(xy) <- c("y", "x")

    plot(f(y) ~ x,
        data = xy, pch = 16,
        xlab = if (is.null(xlab)) names$x else xlab,
        ylab = if (is.null(ylab)) names$y else ylab
    )
    curve(f(predict(mod, newdata = setNames(list(x), names$x))), col = "blue", add = TRUE)

    # t <- predict(mod, interval = "prediction")
    # print(t)

    curve(f(predict(mod, newdata = setNames(list(x), names$x), interval = "confidence")[, "lwr"]),
        add = TRUE, lty = 2, lwd = 2, col = "red"
    )
    curve(f(predict(mod, newdata = setNames(list(x), names$x), interval = "confidence")[, "upr"]),
        add = TRUE, lty = 2, lwd = 2, col = "red"
    )
    curve(f(predict(mod, newdata = setNames(list(x), names$x), interval = "prediction")[, "lwr"]),
        add = TRUE, lty = 2, lwd = 2
    )
    curve(f(predict(mod, newdata = setNames(list(x), names$x), interval = "prediction")[, "upr"]),
        add = TRUE, lty = 2, lwd = 2
    )
}

logistic.plot <- function(mod, par = TRUE) {
    if (par) {
        oldpar <- par(mfrow = c(1, 2))
    }
    xy <- model.frame(mod)
    plot.default(data.frame(xy[1], fitted(mod)),
        pch = 1, ylim = c(-0.1, 1.1), xaxt = "n",
        ylab = "Fitted values"
    )
    axis(1, at = xy[, 1])
    abline(0, 0, col = "red", lwd = 2)
    abline(1, 0, col = "red", lwd = 2)

    boxplot(fitted(mod) ~ xy[, 1],
        ylim = c(-0.1, 1.1),
        xlab = colnames(xy)[1], ylab = "Fitted values"
    )
    abline(0, 0, col = "red", lwd = 2)
    abline(1, 0, col = "red", lwd = 2)

    if (par) {
        par(oldpar)
    }
}

# par(mfrow = c(2, 1))
# y1 <- rnorm(40, mean = 30)
# t.onesample(y1, mu = 30.1)

# lm.intervals(lm(log(dist / (1 - dist)) ~ speed, data = cars), exp(x) / (1 + exp(x)), ylab = "dist")

# test raggruppati per funzione
