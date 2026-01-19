# Statistica

> Per Wikipedia e Vidoni SSE, sum of squared errors = RSS, residual sum of squares

## Standard Error

Mean Square Error $MSE=E[(\hat\theta-\theta)^2]=V(\hat\theta)+|E[\hat\theta]-\theta|^2=V(\hat\theta)+Bias^2$\
Standard Error $SE=\sqrt{MSE}$. Se $\hat\theta$ è **unbiased**, $SE=\sqrt{V(\hat\theta)}$

* mean
  * $SEM=\sqrt{\frac{\sigma^2}{n}}$
  * $\widehat{SEM}=\sqrt{\frac{S^2}{n}}$
  * linear regression fitted mean ($\hat\mu_j$)
    * $\widehat{SEFM}=\hat\sigma\sqrt{\frac1n+\frac{(x_j-\overline x)^2}{SST_X}}$

      dipende da $x_j$
* proportion
  * $SEP=\sqrt{\frac{p(1-p)}{n}}$
* difference
  * varianza diversa
    * difference/mean
      * $SED=\sqrt{SEM^2_X+SEM^2_Y}$
  * pooled variance $\widehat{SED^\sigma}=\sqrt{S^2_p\left(\frac1{n_X}+\frac1{n_y}\right)}$
    * difference/mean
      * $S^2_p=\frac{S^2_1(n_1-1)+S^2_2(n_2-1)}{n_1+n_2-2}$
    * proportions
      * $S^2_p=\hat p(1-\hat p)$
* predictors
  * linear predictor
    * $\widehat{SEL}=\sqrt{\hat\sigma^2+\widehat{SEFM}^2}$

## Statistics

* $T$-statistic
  * mean, studentized sample mean

    $\frac{\overline Y-\mu}{\widehat{SEM}}\sim t_{n-1}$
  * linear confidence interval

    $\frac{\overline Y_i-\hat y_i}{\widehat{SEFM}}\sim t_{n-2}$
  * difference of mean, same variance

    $\frac{\overline X-\overline Y}{\widehat{SED^\sigma}}\sim t_{n_X+n_Y-2}$
  * difference of mean, different variance

    $\frac{\overline X-\overline Y}{SED}$
  * linear predictor

    $\frac{Y_i-\hat y_i}{\widehat{SEL}}\sim t_{n-2}$
* $Z$-statistic
  * mean, standardized sample mean

    $\frac{\overline Y-\mu}{SEM}\sim N(0,1)$
  * proportions

    $\frac{p_X-p_{H_0}}{SEP}\sim N(0,1)$
  * difference of proportions

    $\frac{p_X-p_Y}{\widehat{SED^\sigma}}\sim N(0,1)$
* $F$-statistic
  * sample variance ratio, normal distribution

    $\frac{S^2_X/\sigma^2_X}{S^2_Y/\sigma^2_Y}\sim F(n_X-1,n_Y-1)$
  * generalized linear test

    $\frac{\text{explained variance}}{\text{unexplained variance}}$
    * ANOVA

      $\frac{BSS/a-1}{WSS/an-a}\sim F(a-1,an-a)$

    * comparison con un modello restricted $R$

      $\frac{(SSE^R-SSE)/(k-k_R)}{SSE/(n-k)}\sim F(k-k_R,n-k)$

      $\frac{(SST-SSE)-(SST-SSE^R)}{SSE}=\frac{SSE^R-SSE}{SSE}$
    * linear regression. Su $\hat Y^R=\overline y$

      $\frac{SSE^R-SSE}{SSE}=\frac{SST-SSE}{SSE}=\frac{SSR}{SSE}$

* $\chi^2$-statistic
  * large enough contingency table

    $\sum\limits^r_{i=1}\sum\limits^c_{j=1}\frac{(n_{ij}-\hat p^0_{ij} n)}{\hat p^0_{ij} n}\sim \chi^2((r-1)(c-1))$

    $\hat p^0_{ij}=\frac{n_{i+}}{n}\frac{n_{+j}}{n}$
  * sample variance from normal distributions

    $\frac{(n-1)S^2}{\sigma^2}\sim \chi^2(n-1)$
  * ANOVA
    * BSS, between-group sum of squares, sotto $H_0$

      $n\sum\limits^{a}_{i=0}(\overline y_i-\overline y)^2\sim\chi^2(a-1)$
    * WSS, within-group sum of squares

      $\sum\limits^{a}_{i=0}\sum\limits^{n}_{j=0}(\overline y_{ij}-\overline y_i)^2\sim\chi^2(an-a)$
  * linear regression
    * sum of squared residuals, sotto $H_0$

      $\frac{SSR}{\sigma^2}\sim\chi^2(k)$

      $SSR=\sum\limits^n_{i=0}(\hat y_i-\overline y)^2$
    * sum of squared errors

      $\frac{SSE}{\sigma^2}\sim\chi^2(n-k-1)$

      $SSE=\sum\limits^n_{i=0}\hat\epsilon_i^2$
    * sum of squares total, sotto $H_0$

      $\frac{SST}{\sigma^2}\sim\chi^2(n-1)$

      $SST=\sum\limits^n_{i=0}(y_i-\overline y)^2=SSR+SSE$

## Test

* mean
  * `t.test(data)` intervallo di confidenza della media, opzionalmente secondo `conf.level` e `alternative`
    * $T$-statistic/mean, studentized sample mean
    * `t.test(data, mu = mu0)` verifica se la media è compatibile con `mu0`
  * normal
    * `prop.test(success, total, p = mu0, correct = FALSE)` su large enough Bernoulli, confronta `success/total` con `p` e calcola p-value, opzionalmente `conf.level`

      esegue un $\chi^2$-test su $z^2$, con $SE$ appropriato
      * $Z$-statistic/proportions
  * comparison con $H_0: \theta_X-\theta_Y=0$
    * `t.test(data1, data2)` confronta medie con $\sigma^2$ simile
      * $T$-statistic/difference of mean, same variance
    * `t.test(data1, data2, var.equal = FALSE)` Welch test, confronta medie con $\sigma^2$ diverso
      * $T$-statistic/difference of mean, different variance
    * `t.test(data1, data2, paired = TRUE)` verifica se le differenze pairwise $Y'=X-Y$ sono compatibili con $\mu'=0$
      * $T$-statistic/mean, studentized sample mean
    * `prop.test(c(successX, successY), c(totalX, totalY), correct = FALSE)` su large enough Bernoulli, confronta `successX/totalX` con `successY/totalY` e calcola p-value, opzionalmente `conf.level`.

      esegue un $\chi^2$-test su $z^2$, con $S^2_p$ per le proporzioni
      * $Z$-statistic/difference of proportions
* variance test
  * `var.test(data1, data2)` F-test, confronta le varianze, opzionalmente `conf.level` e `alternative`
    * $F$-statistic/sample variance ratio, normal distribution
* normality test
  * test grafici
    * qq-plot
    * histogram vs normal distribution
  * test numerici
    * `dagoTest(data)` calcola più p-value
      * omnibus: cumulativo
      * kurtosis: peso delle code
      * skewness: simmetria delle code
    * `shapiroTest(data)` Shapiro-Wilk test
    * `ks.test(data)` Kolmogorov-Smirnov test
* test di correlazione
  * correlazioni lineari
    * `cor.test(data1, data2, method = "pearson")` Pearson's correlation coefficient

      `cor(data1, data2, method = "pearson")` calcola $\rho(X,Y)=\frac{COV(X,Y)}{\sigma_X\sigma_Y}$, $-1\leq\rho\leq1$
  * correlazioni monotone
    * `cor.test(data1, data2, method = "spearman")` calcola Spearman's rank correlation coefficient
      * `cor(data1, data2, method = "spearman")` calcola $\tau_s=\rho(R(X),R(Y))$
    * `cor.test(data1, data2, method = "kendall")` calcola Kendall's rank correlation coefficient
      * `cor(data1, data2, method = "kendall")` calcola $\tau_k=\frac{n_c-n_d}{n}$

        $n_c$ = numero di coppie concordi\
        $n_d$ = numero di coppie discordi
  * `chisq.test(contTable)` tabella di contingenza di variabili categoriche

    Pearson's $\chi^2$-test, p-value alto se indipendenti
    * $\chi^2$-statistic/large enough contingency table
  * `vif(model)` calcola variance inflation factor, $\gtrapprox5$ multicollinearità, $\gt10$ severa

    $\text{VIF}_j=\frac{1}{1-R^2_j}$

    Dove $R^2_j$ è $R^2$ per il modello avente $X_j$ come response variable, e il resto delle variabili come regressors
* model checking
  * `predict(model, newdata = data, interval = "confidence")` confidence interval for $\mu_i$ where $i=1..|$`newdata`$|$

    $[\hat\mu_i\pm t_{n-2;1-\alpha/2}\widehat{SEFM}]$
    * $T$-statistic/linear confidence interval
  * `predict(model, newdata = data, interval = "prediction")` prediction interval for $Y_i$ where $i=1..|$`newdata`$|$

    $[\hat Y_i\pm t_{n-2;1-\alpha/2}\widehat{SEL}]$
    * $T$-statistic/linear predictor
  * `logLik(model)` calcola $\log(\hat{\mathcal L})$, alto è meglio

    $\mathcal L=P_\theta (X=x)$
  * `boxcox(model, lambda = c(l1, l2, ...))` grafico box-cox interpolato sui valori in `lambda`. Se `plotit=F, interp=F` restituisce solo un vettore di likelihood.

    $y(\lambda)=\begin{cases}\frac{y^\lambda-1}{\lambda}&\text{if }\lambda\neq0\\\log y&\text{if }\lambda=0\end{cases}$
  * `AIC(model1, model2, ...)` calcola AIC, basso è meglio

      $AIC=-2\ln(\hat{\mathcal L})+2k$
  * `BIC(model1, model2, ...)` calcola BIC, penalizza numerosità del sample, basso è meglio

    $BIC=-2\ln(\hat{\mathcal L})+\ln(n)k$
    * `AIC(model, k = log(length(data)))`
  * `drop1(model, test = "F")` restricts a turno un parametro a 0 e calcola statistiche con `test`
  * `cv -= log(d(one_out, mu, sd))` cross validation leave-one-out, basso è meglio

    $CV=-\sum\limits^n_{i=1}\ln(f_{\hat\theta[\setminus i]}(y_i))$
  * `anova(model)` calcola la tabella ANOVA per un modello generico
    * calcola F-statistic per ANOVA, calcola p-value
    * $F$-statistic/generalized linear test/ANOVA
  * `anova(model.r, model)` generalized linear test tra un modello ristretto e uno completo. $H_0$ è `model.r`, perché la statistica si basa sulla distanza da $SSE^R$ a $SSE$
    * calcola F-statistic per comparison, calcola p-value
    * `RSS` sono rispettivamente $SSE^R$ e $SSE$. $SSE^R\geq SSE$
    * $F$-statistic/generalized linear test/comparison
* `summary(model)` calcola informazioni su un modello
  * t-test su ciascun coefficiente su $H_0=\beta_i=0$
  * $R^2=\frac{SSR}{SST}$ percentuale di varianza spiegata dal modello, adjusted $R^2=1-\frac{V(\hat\epsilon)}{V(y)}$
  * $F$-statistic/generalized linear test/linear regression

    Rigettare F-test $H_0:\beta_1=\beta_i=0$ (F-value estremo, p-value basso) con gli individuali t$_i$-test accettanti di $H_0:\beta_i=0$, suggerisce multicollinearità, una relazione di dipendenza senza stimatori significativi.

## Fitting

* `lm(response ~ predictor, data = data)` linear regression
  * calcola coefficienti della regressione lineare, calcola p-value
    * $F$-statistic/generalized linear test/linear regression
* `aov(response ~ predictor, data = data)` calcola modello ANOVA
  * calcola F-statistic per ANOVA, calcola p-value
    * $F$-statistic/generalized linear test/ANOVA
* `glm(response ~ predictor, data = data, family = distribution(link = link))` generalized linear regression
  * `distribution(link = default)`
    * `binomial(link = "logit")`
    * `gaussian(link = "identity")`
    * `Gamma(link = "inverse")`
    * `inverse.gaussian(link = "1/mu^2")`
    * `poisson(link = "log")`
    * `quasi(link = "identity", variance = "constant")`
    * `quasibinomial(link = "logit")`
    * `quasipoisson(link = "log")`
  * `link`
    * `"logit"`
    * `"probit"`
    * `"identity"`
    * `"log"`
    * `"sqrt"`
    * `"1/mu^2"`
    * `"inverse"`

## Linear-Logistic Comparison

| Linear regression                    | Logistic regression                                             |
| ------------------------------------ | --------------------------------------------------------------- |
| estimates, std. errors, t-values     | estimates, std. errors, z-values                                |
| sum of squares                       | deviance                                                        |
| residual standard error              | -                                                               |
| minimize the residual sum of squares | maximize the $\log(\hat{\mathcal L})$ (minimizing the deviance) |
| select models with smaller AIC       | select models with smaller AIC                                  |
| compare nested models via F-tests    | compare nested models via $\chi^2$ tests                        |
| full set of diagnostic plots         | some diagnostic plots                                           |
| partial residual plots               | plots of explanatory variable contributions                     |
| $R^2$ and adjusted $R^2$             | predictive accuracy                                             |
