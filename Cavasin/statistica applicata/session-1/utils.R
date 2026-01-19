library("car")
library("lattice")

clear.plots <- function() while (dev.cur() > 1) dev.off()

grid.plot <- function(rows, cols) par(mfrow = c(rows, cols), mar = c(3, 3, 1, 1), mgp = c(1.6, 0.5, 0))

equispaced <- function(from = 1, to, n = to) {
    length <- abs(to - from)
    radius <- length / (2 * n)
    centers <- seq(from = from + radius, to = to - radius, length.out = n)
}

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
    polygon(cord.x, cord.y, col = "#5B97D3")
    cord.x <- c(xs$max, seq(xs$max, int[2], length.out = 100), int[2])
    cord.y <- c(0, dt(nor(seq(xs$max, int[2], length.out = 100)), df), 0)
    polygon(cord.x, cord.y, col = "#5B97D3")
    abline(0, 0, lwd = 2)
    points(x, rep(0, length.out = length(x)), pch = "|", cex = 0.8)
    points(res$estimate, 0, pch = "|", col = "#D95959")


    if (!is.null(mu)) {
        abline(v = mu, col = "#D95959")
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
        old.par <- par(mfrow = c(1, 2))
        on.exit(par(old.par))
    }
    xy <- model.frame(mod)
    plot.default(data.frame(xy[1], fitted(mod)),
        pch = 1, ylim = c(-0.1, 1.1), xaxt = "n",
        ylab = "Fitted values",
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
}

plot.pairs <- function(data) {
    # correlazione sull'upper, pearson(sx) e spearman(dx)
    panel.cor <- function(x,
                          y,
                          digits = 2,
                          prefix = "",
                          cex.cor,
                          ...) {
        par(usr = c(0, 1, 0, 1))
        cor_pearson <- cor(x, y, method = "pearson")
        cor_spearman <- cor(x, y, method = "spearman")

        txt_pearson <- format(cor_pearson, digits = digits)
        txt_spearman <- format(cor_spearman, digits = digits)

        if (missing(cex.cor)) {
            cex.cor <- 0.8 / strwidth(txt)
        }
        rect(0, 0, 0.5, abs(cor_pearson), border = rgb(0, 0, 0, alpha = 0.5), col = if (cor_pearson > 0) "#8BBF65" else "#F2BC57")
        rect(0.5, 0, 1, abs(cor_spearman), border = rgb(0, 0, 0, alpha = 0.5), col = if (cor_spearman > 0) "#8BBF65" else "#F2BC57")
        text(0.04, 0.96, adj = c(0, 1), txt_pearson, cex = 1.2)
        text(0.96, 0.04, adj = c(1, 0), txt_spearman, cex = 1.2)
    }

    panel.dist <- function(x, y, ...) {
        box.plot <- function(x, y, horizontal = FALSE) {
            levels <- length(levels(data[[par("mfg")[if (horizontal) 1 else 2]]]))
            plot.length <- diff(par("usr")[if (horizontal) 3:4 else 1:2])
            boxplot(y ~ x,
                add = TRUE,
                horizontal = horizontal,
                axes = FALSE,
                at = equispaced(to = levels),
                boxwex = (plot.length / levels) * 0.8
            )
        }
        input <- list(row = data[[par("mfg")[1]]], col = data[[par("mfg")[2]]])
        if (is.factor(input$col) && is.factor(input$row)) {
            levels <- list(
                width = length(levels(data[[par("mfg")[1]]])),
                height = length(levels(data[[par("mfg")[2]]]))
            )
            image(equispaced(to = levels$height),
                equispaced(to = levels$width),
                table(x, y),
                col = gray.colors(12),
                add = TRUE
            )
        } else if (is.factor(input$col)) {
            box.plot(x, y)
        } else if (is.factor(input$row)) {
            box.plot(y, x, horizontal = TRUE)
        } else {
            panel.smooth(x, y, ...)
        }
    }

    # istogramma sulla diagonale
    panel.hist <- function(x, ...) {
        old.usr <- par(usr = c(par("usr")[1:2], 0, 1.5))
        on.exit(par(usr = old.usr))
        col <- data[[par("mfg")[1]]]
        h <- hist(x, breaks = if (is.factor(col)) length(levels(col)) else "Sturges", plot = FALSE)
        breaks <- h$breaks
        y <- h$counts
        rect(breaks[-length(breaks)], 0, breaks[-1], y / max(y), col = "#5B97D3")
    }

    # label magenta se è factor
    panel.text <- function(x, y, labels, cex, font, ...) {
        col <- ifelse(is.factor(getElement(data, labels[1])), "#D95959", "black")
        text(0.5, 0.95, labels[1], col = col, adj = c(0.5, 1), font = 2, cex = 1.15)
    }

    # effettivo plotting
    pairs(
        data,
        panel = panel.smooth,
        diag.panel = panel.hist,
        upper.panel = panel.cor,
        lower.panel = panel.dist,
        text.panel = panel.text,
        bg = "blue",
        font.labels = 2,
        pch = 1,
        cex = 1,
        cex.labels = 1,
        oma = c(1, 1, 1, 1),
        mgp = c(2, 0.2, 0),
        gap = 0.2,
        tcl = -0.25
    )
}

plot.vif <- function(mod) {
    vif <- vif(mod)
    barplot(vif,
        main = "VIF",
        col = ifelse(vif > 5, "#D95959", "#5B97D3"),
    )
    abline(h = 5, col = "red", lwd = 2)
}

drop1.aicbic <- function(mod, test = c("F", "none", "Chisq")) {
    test <- match.arg(test)
    df <- drop1(mod, test = test)
    bics <- drop1(mod, k = log(nrow(mod$model)))$AIC
    df$BIC <- bics
    # Reorder columns to move the last one to the first position
    # [ncol(df)] is the index of the last column, [1:(ncol(df)-1)] are the rest
    df[, c(1:match("AIC", colnames(df)), ncol(df), 5:(ncol(df) - 1))]
}

aicbic <- function(...) {
    names <- as.character(substitute(list(...))[-1])

    df <- AIC(...)
    df$BIC <- BIC(...)$BIC
    rownames(df) <- names
    df
}

classifier.summary <- function(actual, predicted, negative = 0) {
    cm <- confusionMatrix(actual, predicted, negative)
    de <- diagnosticErrors(cm)
    list(cm = cm, de = de)
}


# par(mfrow = c(2, 1))
# y1 <- rnorm(40, mean = 30)
# t.onesample(y1, mu = 30.1)

# lm.intervals(lm(log(dist / (1 - dist)) ~ speed, data = cars), exp(x) / (1 + exp(x)), ylab = "dist")

# test raggruppati per funzione

# cars$vs <- factor(cars$vs)
# cars$am <- factor(cars$am)
# cars$cyl <- factor(cars$cyl)
# cars$gear <- factor(cars$gear)

# plot.pairs(cars)
