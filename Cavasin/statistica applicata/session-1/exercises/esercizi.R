# rm(list = ls())
require.packages <- function(Packages) {
    install.packages(setdiff(Packages, installed.packages()))
    for (p in Packages) {
        require(p, character.only = TRUE)
    }
}

clear.plots <- function() while (dev.cur() > 1) dev.off()

################################################################################

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

# prima plotta il modello, poi stampa delle statistiche
checkLM <- function(model) {
    # visually
    par(mfrow = c(2, 2))
    plot(model)
    par(mfrow = c(1, 1))

    # numerically
    s <- summary(model)
    print(s)
    p.value <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)

    an <- anova(model)
    print(an)

    cat("\n")
    cat("any significant var : ", p.value < 0.05, "\n")
    cat("residuals are normal: ", shapiro.test(model$residuals)$p.value >= 0.05, "\n")
    cat("adj R-sq            : ", s$adj.r.squared, "\n")

    res <- list(summary = s, anova = an)
    invisible(res)
}

# calcola il vif ed evidenzia la significatività dei valori
checkVIF <- function(model) {
    V <- vif(model)

    colin <- sapply(floor(V / 5), function(n) paste(rep("*", n), collapse = ""))
    noquote(cbind(V, colin))
}

# plotta i parziali
plot.partials <- function(model, mfrow = c(1, 1)) {
    par(mfrow = mfrow)
    termplot(
        model,
        main = deparse(substitute(model)),
        partial.resid = TRUE,
        se = TRUE,
        smooth = panel.smooth,
        pch = 20,
        col.smth = "blue",
        col.res = "gray30",
    )
    par(mfrow = c(1, 1))
}

# plotta intervalli, lm simple
plot.intervalsSimple <- function(model, data, xName, n = 10) {
    x <- data[, xName]

    plot(data, type = "n")
    for (s in simulate(model, n)) {
        abline(lm(s ~ x), lty = 2, col = "gray80")
    }
    points(data)

    int <- list()
    int$x <- seq(min(x), max(x), length.out = 100)

    int.x.df <- as.data.frame(int$x)
    colnames(int.x.df) <- xName

    int$conf <- predict(model, newdata = int.x.df, interval = "confidence")
    int$pred <- predict(model, newdata = int.x.df, interval = "prediction")

    lines(int$x, int$conf[, "fit"], col = "red", lty = 2, lwd = 2)
    lines(int$x, int$conf[, "lwr"], col = "blue", lty = 2, lwd = 2)
    lines(int$x, int$conf[, "upr"], col = "blue", lty = 2, lwd = 2)
    lines(int$x, int$pred[, "lwr"], col = "darkblue", lty = 2, lwd = 2)
    lines(int$x, int$pred[, "upr"], col = "darkblue", lty = 2, lwd = 2)

    invisible(int)
}

# aiuta al plotting di intervalli di conf e pred
plot.intervals <- function(formula, model, data, newdata, newdata.x, main = deparse(substitute(newdata)), xlab = deparse(formula[3]), ylab = deparse(formula[2])) {
    plot(
        formula,
        data = data,
        main = main,
        xlab = xlab,
        ylab = ylab
    )

    int <- list()
    int$x <- newdata.x
    int$conf <- predict(model, newdata = newdata, interval = "confidence")
    int$pred <- predict(model, newdata = newdata, interval = "prediction")

    lines(int$x, int$conf[, "fit"], col = "red", lwd = 2)
    lines(int$x, int$conf[, "lwr"], col = "blue", lty = 2, lwd = 2)
    lines(int$x, int$conf[, "upr"], col = "blue", lty = 2, lwd = 2)
    lines(int$x, int$pred[, "lwr"], col = "darkblue", lty = 2, lwd = 2)
    lines(int$x, int$pred[, "upr"], col = "darkblue", lty = 2, lwd = 2)

    invisible(int)
}

################################################################################

# Si definisca un opportuno modello statistico per studiare la relazione
# tra LDL (variabile risposta) e le possibili variabili esplicative Sesso, Eta, Trigliceridi.

# packages sicuri
require.packages(c("moments", "car", "MASS", "lattice"))

# to check
# requirePackages(c("caret","nortest","fBasics","ISLR","ROCR", "vcd", "vioplot", "DAAG", "qeML","partykit","crossval","RWeka","xtable","party"))

ColesteroloFull <- read.csv(
    "colesterolo4.txt",
    sep = " ",
    dec = ".",
    quote = "\"",
    header = TRUE,
)
ColesteroloFull$Sesso <- factor(ColesteroloFull$Sesso)

Colesterolo <- ColesteroloFull
# Colesterolo <- ColesteroloFull[, c("Sesso","Eta", "Trigliceridi", "LDL")]

Colesterolo.newx <- data.frame(
    Sesso = c("M", "FALSE"),
    Eta = c(78, 68),
    Colesterolo = c(270, 280),
    HDL = c(78, 59),
    Trigliceridi = c(180, 150),
    Indice = c(3.4, 2.8)
)

################################################################################
# overall checks
################################################################################

summary.extended(Colesterolo)
plot.pairs(Colesterolo)
splom(Colesterolo, groups = Colesterolo$Sesso, auto.key = TRUE)
# si osserva che:
# - alcune variabili sono molto correlate,

################################################################################
# single var checks
################################################################################

check.normality(Colesterolo$Eta)
check.normality(Colesterolo$HDL)
check.normality(Colesterolo$Trigliceridi)
check.normality(Colesterolo$Indice)
# non sembrano provenienti da una normale

check.normality(Colesterolo$LDL)
# sembra essere proveniente da una distribuzione normale (RV)

check.normality(Colesterolo$Colesterolo)
# sembra essere *quasi* proveniente da una normale, eccetto per il sample 495

check.normality(Colesterolo$Colesterolo[-495])
# proveniente da normale


################################################################################
# lms checks
################################################################################


lin.base <- lm(LDL ~ ., data = Colesterolo)
checkLM(lin.base)
#* verificando le assunzioni sul modello:
#* - residui sembrano pressocché lineari, (sembra esistere una minima curva verso il basso, ma la ritengo accettabile)
#*     tuttavia il test di shapiro-wilk non conferma la mia ipotesi
#* - errori quasi normalmente distribuiti, eccetto per alcuni potenziali outlier (495, 22)
#* - omoschedasticità pressocché confermata
#* - un effettivo outlier (495) uno molto vicino (22)

check.normality(lin.base$residuals)
# verifico nuovamente che no, non sono normali, sembra appunto per 22 e 495


lin.x495 <- lm(LDL ~ ., data = Colesterolo[c(-495), ])
checkLM(lin.x495)
#* Avendo osservato un effettivo outlier, ripeto rimuovendo 495.
#* verificando le assunzioni sul nuovo modello:
#* - ...
#* - i residui sono normali, ancora forse 22 è particolare
#* - ...
#* - un potenziale outlier: 22

check.normality(lin.x495$residuals)
# sembra meglio, però lo stesso provo a togliere il 22


lin.x495.x22 <- lm(LDL ~ ., data = Colesterolo[c(-495, -22), ])
checkLM(lin.x495.x22)
#* provo rimuovendo anche 22
#* verificando le assunzioni sul nuovo modello:
#* - ...
#* - ...
#* - ...
#* - no outlier


################################################################################
# multicollinearità
################################################################################


checkVIF(lin.base)
# sembra che Colesterolo, HDL, Indice siano collineari

# provo a toglierne uno alla volta
Colesterolo.x495.x22 <- Colesterolo[c(-495, -22), ]

lin.no_col <- lm(LDL ~ . - Colesterolo, data = Colesterolo.x495.x22)
lin.no_hdl <- lm(LDL ~ . - HDL, data = Colesterolo.x495.x22)
lin.no_ind <- lm(LDL ~ . - Indice, data = Colesterolo.x495.x22)

checkVIF(lin.x495.x22)
checkVIF(lin.no_col)
checkVIF(lin.no_hdl)
checkVIF(lin.no_ind)
# sembra che togliere uno qualsiasi di quelle tre variabili
# sia sufficiente per abbassare notevolmente la colinearità

AIC(lin.x495.x22)
AIC(lin.no_col)
AIC(lin.no_hdl)
AIC(lin.no_ind)
# inoltre il modello senza Indice risulta, secondo AIC
# lievemente migliore rispetto al modello con ogni variabile


################################################################################
# variabili richieste
################################################################################


# siccome è richiesto considerare la relazione LDL ~ Sesso + Eta + Trigliceridi,
# d'ora in poi considero solo quelle variabili

lin.slim <- lm(LDL ~ Eta + Trigliceridi + Sesso, data = Colesterolo.x495.x22)
checkLM(lin.slim)

anova(lin.x495.x22)
anova(lin.slim)
# il nuovo modello "slim" presenta un adj R-squared basso,
# ossia poca della varianza dei dati è spiegata dal modello.

# questo è probabilmente dato dalla mancanza della variabile Colesterolo,
# che secondo un'analisi ANOVA spiega la maggior parte della varianza

lin.slim.all_Sesso <- lm(LDL ~ (Eta + Trigliceridi):Sesso, data = Colesterolo.x495.x22)
lin.slim.eta_Sesso <- lm(LDL ~ Eta:Sesso + Trigliceridi, data = Colesterolo.x495.x22)
lin.slim.tri_Sesso <- lm(LDL ~ Eta + Trigliceridi:Sesso, data = Colesterolo.x495.x22)

checkLM(lin.slim.all_Sesso)
checkLM(lin.slim.eta_Sesso)
checkLM(lin.slim.tri_Sesso)
# sembra che categorizzare rispetto al Età:Sesso sia una scelta che porta lievi migliorie,


plot.partials(lin.x495.x22, mfrow = c(2, 3))
plot.partials(lin.slim, mfrow = c(2, 3))
# ancora una volta si può notare il quantitativo di informazione persa non considerando il Colesterolo

################################################################################
# intervalli di LDL rispetto Eta e sesso
################################################################################

xM <- data.frame(
    Eta          = seq(min(Colesterolo$Eta), max(Colesterolo$Eta), length.out = 100),
    Trigliceridi = rep(mean(Colesterolo$Trigliceridi), 100),
    Sesso        = rep("M", 100)
)
xF <- xM
xF$Sesso <- rep("FALSE", 100)

par(mfrow = c(1, 2))
plot.intervals(formula = LDL ~ Eta, model = lin.slim, data = Colesterolo, newdata = xM, newdata.x = xM$Eta)
plot.intervals(formula = LDL ~ Eta, model = lin.slim, data = Colesterolo, newdata = xF, newdata.x = xM$Eta)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot.intervals(formula = LDL ~ Eta, model = lin.slim.eta_Sesso, data = Colesterolo, newdata = xM, newdata.x = xM$Eta)
plot.intervals(formula = LDL ~ Eta, model = lin.slim.eta_Sesso, data = Colesterolo, newdata = xF, newdata.x = xM$Eta)
par(mfrow = c(1, 1))

################################################################################
# da qui sotto non ho ancora lavorato bene


data3 <- data.frame(Z = c(1, 2, 3, 4), P = c(2, 3, 6, 7))
mod2 <- lm(P ~ Z, data3)
plot.intervalsSimple(mod2, data3, "Z")




# non sembra che categorizzare con Sesso faccia la differenza

LDL_su_Eta <- lm(LDL ~ Eta, data = Colesterolo)
checkLM(LDL_su_Eta)

LDL_su_Sesso <- lm(LDL ~ Eta * Sesso, data = Colesterolo)
checkLM(LDL_su_Sesso)



lin.x495.x22_ridotto <- lm(LDL ~ Colesterolo + HDL + Trigliceridi, data = Colesterolo[c(-495, -22), ])
checkLM(lin.x495.x22_ridotto)



# verifico con scala log:
check.normality(log(Colesterolo$HDL))
check.normality(log(Colesterolo$Indice))
# provenienti da normale

check.normality(log(Colesterolo$Trigliceridi))
# non proveniente da normale



lin.base_log <- lm(LDL ~ Sesso + Eta + Colesterolo + Trigliceridi + log(HDL) + log(Indice), data = Colesterolo)
checkLM(lin.base_log)

lin.x495_log <- lm(LDL ~ Sesso + Eta + Colesterolo + Trigliceridi + log(HDL) + log(Indice), data = Colesterolo[c(-495), ])
checkLM(lin.x495_log)

lin.x495.x22_log <- lm(LDL ~ Sesso + Eta + Colesterolo + Trigliceridi + log(HDL) + log(Indice), data = Colesterolo[c(-495, -22), ])
checkLM(lin.x495.x22_log)

AIC(lin.base, lin.base_log)
AIC(lin.x495, lin.x495_log)
AIC(lin.x495.x22, lin.x495.x22_log)
# risulta sempre migliore quello senza scalature log

# provando a scalare anche RV
lin.x495.x22_log_log <- lm(log(LDL) ~ Sesso + Eta + Colesterolo + log(HDL) + log(Indice), data = Colesterolo[c(-495, -22), ])
checkLM(lin.x495.x22_log_log)
# nessuna miglioria





# i vari punti sono tutti schiacciati all'inizio, con qualche valore fuori scala. provo col log
loglin <- lm(LDL ~ log(Trigliceridi) + Sesso + Eta, data = Colesterolo)
checkLM(loglin)
summary(loglin, correlation = TRUE)

lasso <- qeLASSO(Colesterolo, "LDL")
lasso$coefs

predict(lasso, Colesterolo.newx)
predict(lin.base, Colesterolo.newx)
predict(lin.slim, Colesterolo.newx)





# newdata <- data.frame(X = 1.1)
# y <- predict(lmm, newdata = newdata)
# points(newdata, y,col="blue")

################################################################################

pca <- qePCA(Colesterolo, "LDL", "qeLin", pcaProp = 0.5)
pca
summary(pca$pcaout)
plot(pca$pcaout)
pca$pcaout

prcomp(as.matrix(Colesterolo[, -which(Colesterolo)]), scale. = TRUE)
x <- Colesterolo[, -which(names(Colesterolo) == "Sesso")]

prcomp(x, scale. = TRUE, rank. = 2)

# altro
data(mlb1)

data(car)
dt.QE <-
    dt.QE <- qeRpart(Colesterolo, "LDL")
plotcp(dt.QE)
plot(dt.QE)

dt.QE2 <- qeRpart(mlb1, "Position")
par(mfrow = c(1, 2))
plotcp(dt.QE)
plot(dt.QE)
par(mfrow = c(1, 1))
