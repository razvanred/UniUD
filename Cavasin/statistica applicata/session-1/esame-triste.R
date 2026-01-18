# Si consideri il dataset automobili.txt, che contiene informazioni su 30 modelli di auto per le quali si
# rilevano le seguenti variabili: consumo in miglia/gallone (mpg), numero di cilindri (cyl), cilindrata (disp),
# potenza (hp), rapporto al ponte posteriore (drat), peso in 1000 libbre (wt), tempo per 1/4 di miglio (qsec),
# motore (vs, con 0 = V-shaped, 1 = straight), cambio (am, con 0 = automatic, 1 = manual), numero di marce (gear),
# numero carburatoroi (carb). Si definisca un opportuno modello statistico per studiare la relazione tra il
# consumo mpg (variabile risposta) e le potenziali variabili esplicative presenti nel dataset.
# Note
# - Per caricare il dataset si utilizzi il comando read.table(file = "automobili.txt", header = TRUE, row.names=1),
# dopo aver salvato il file .txt nella directory di lavoro corrente

require("lattice")
require("car")
require("moments")
require("MASS")

clearPlots <- function() while (dev.cur() > 1) dev.off()

# funzione per "pairs" più intelleggibili
plot.pairs <- function(Dataset) {
    # correlazione sull'upper, pearson(sx) e spearman(dx)
    panel.cor <- function(x,
                          y,
                          digits = 2,
                          prefix = "",
                          cex.cor,
                          ...) {
        par(usr = c(0, 1, 0, 1))
        cor_pearson <- abs(cor(x, y, method = "pearson"))
        cor_spearman <- abs(cor(x, y, method = "spearman"))

        txt_pearson <- format(cor_pearson, digits = digits)
        txt_spearman <- format(cor_spearman, digits = digits)

        txt <- paste0(prefix, "\n", "p: ", txt_pearson, "\n", "s: ", txt_spearman)
        if (missing(cex.cor)) {
            cex.cor <- 0.8 / strwidth(txt)
        }
        text(0.5, 0.5, txt, cex = 1.2)

        rect(0, 0, 0.25, cor_pearson, col = "#545452")
        rect(0.75, 0, 1, cor_spearman, col = "#545452")
    }

    # istogramma sulla diagonale
    panel.hist <- function(x, ...) {
        usr <- par("usr")
        on.exit(par(usr = usr))
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts
        y <- y / max(y)

        rect(breaks[-nB], 0, breaks[-1], y, col = "#b89f88")
    }

    # label magenta se è factor
    panel.text <- function(x, y, labels, cex, font, ...) {
        col <- ifelse(is.factor(getElement(Dataset, labels[1])), "#776a3c", "black")
        text(0.5, 0.95, labels[1], col = col, adj = c(0.5, 1), font = 2, cex = 1.15)
    }

    # effettivo plotting
    pairs(
        Dataset,
        panel = panel.smooth,
        diag.panel = panel.hist,
        upper.panel = panel.cor,
        text.panel = panel.text,
        bg = "blue",
        font.labels = 2,
        pch = 1,
        cex = 1,
        cex.labels = 1,
    )
}

# fuzione per stampare più dati rispetto al summary
summary.extended <- function(Dataset) {
    functs <- c("var", "sd", "IQR", "skewness", "kurtosis")

    Summary <- summary(Dataset)
    Summary <- rbind(Summary, rep(NA, length(names(Dataset))))

    format <- sprintf("%%-%gs:%%.2f", max(mapply(nchar, functs)))
    for (funct in functs) {
        Summary <- rbind(Summary, mapply(function(col) {
            tryCatch(
                sprintf(format, funct, match.fun(funct)(col)),
                error = function(err) NA,
                silent = TRUE
            )
        }, Dataset))
    }

    as.table(Summary)
}

# plotta KDE (istogramma + distribuzione normale di riferimento)
plot.KDE <- function(Dataset, name = deparse(substitute(Dataset))) {
    ds_dens <- density(Dataset)

    dnorm_x <- seq.default(min(Dataset), max(Dataset), length.out = 100)
    dnorm_y <- dnorm(dnorm_x, mean(Dataset), sd(Dataset))

    xlim <- range(dnorm_x)
    ylim <- c(0, 1.04 * max(max(ds_dens$y), max(dnorm_y)))



    hist(
        Dataset,
        main = c("Normality of ", name),
        freq = FALSE,
        xlim = xlim,
        ylim = ylim
    )
    lines(ds_dens, lwd = 2)
    lines(dnorm_x, dnorm_y, col = "red", lwd = 2)
}

# calcola il vif ed evidenzia la significatività dei valori
checkVIF <- function(model) {
    V <- vif(model)

    colin <- sapply(floor(V / 5), function(n) paste(rep("*", n), collapse = ""))
    noquote(cbind(V, colin))
}


# plotta sia un qqplot della normale, sia KDE, sullo stesso grafico
check.normality <- function(Dataset) {
    # visually
    par(mfrow = c(1, 2))
    qqPlot(Dataset, envelope = list(col = "red"), col.lines = "red")
    plot.KDE(Dataset, name = deparse(substitute(Dataset)))
    par(mfrow = c(1, 1))

    # numerically
    Sh <- shapiro.test(Dataset)
    Sh$data.name <- deparse(substitute(Dataset))
    print(Sh)
    cat("comes from normal: ", Sh$p.value >= 0.05, "\n")
    Sh$p.value >= 0.05
}


clearPlots()

# mpg cyl disp hp drat wt qsec vs am gear carb
cars <- read.csv(
    "automobili.txt",
    sep = " ",
    dec = ".",
    quote = "\"",
    header = TRUE,
)
cars$vs <- factor(cars$vs)
cars$am <- factor(cars$am)

# mpg    consumo in miglia/gallone
# cyl    numero di cilindri
# disp   cilindrata
# hp     potenza
# drat   rapporto al ponte posteriore
# wt     peso in 1000 libbre
# qsec   tempo per 1/4 di migli
# vs     motore, con 0 = V-shaped, 1 = straight
# am     cambio, con 0 = automatic, 1 = manual
# gear   numero di marce
# carb   numero carburatori

print(summary.extended(cars))

model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, cars)
par(mfrow = c(2, 2))
plot(model)
print(summary(model))

# cyl disp hp drat wt vs
model <- lm(mpg ~ cyl + disp + hp + drat + wt + vs, cars)
par(mfrow = c(2, 2))
plot(model)
print(summary(model))

par(mfrow = c(1, 1))
plot.pairs(cars)


par(mfrow = c(1, 2))
check.normality(cars$drat)
check.normality(cars$qsec)

checkVIF(model)
