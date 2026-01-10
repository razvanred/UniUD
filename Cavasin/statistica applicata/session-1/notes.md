[![](img/sd.svg)](https://en.wikipedia.org/wiki/Standard_error#Derivation)
![](img/notes.png)
![](img/notes_2.png)

![](img/notes_3.png)

---
---

![](img/notes_1.png)

---
---

## Standard Error

Mean Square Error $MSE=E[(\hat\theta-\theta)^2]=V(\hat\theta)+|E[\hat\theta]-\theta|^2=V(\hat\theta)+Bias^2$\
Standard Error $SE=\sqrt{MSE}$. Con $\hat\theta$ è **unbiased** $SE=\sqrt{V(\hat\theta)}$ 

* mean
  * $SEM=\sqrt{\frac{\sigma^2}{n}}$
  * $\widehat{SEM}=\sqrt{\frac{S^2}{n}}$
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

## Statistics

* $T$-statistic
  * mean, studentized sample mean
    * $\frac{\overline Y-\mu}{\widehat{SEM}}\sim t_{n-1}$
  * difference of mean, same variance
    * $\frac{\overline X-\overline Y}{\widehat{SED^\sigma}}\sim t_{n_X+n_Y-2}$
  * difference of mean, different variance
    * $\frac{\overline X-\overline Y}{SED}\sim t_?$
  * mean difference, paired data. Su $V=X-Y$ e $\mu=0$
    * $\frac{\overline{V}}{\widehat{SEM}}\sim t_{n-1}$ 
* $Z$-statistic
  * mean, standardized sample mean
    * $\frac{\overline Y-\mu}{SEM}\sim N(0,1)$
  * proportions
    * $\frac{p_X-p_{H_0}}{SEP}\sim N(0,1)$
  * difference of proportions
    * $\frac{p_X-p_Y}{\widehat{SED^\sigma}}\sim N(0,1)$
* $F$-statistic
  * sample variance ratio, normal distribution
    * $\frac{S^2_X/\sigma^2_X}{S^2_Y/\sigma^2_Y}\sim F(n_X-1,n_Y-1)$
* $\chi^2$-statistic
  * large enough contingency table
    * $\sum\limits^i_{i=1..r}\sum\limits^j_{j=1..c}\frac{(n_{ij}-\hat p^0_{ij} n)}{\hat p^0_{ij} n}\sim \chi^2((r-1)(c-1))$

      con $\hat p^0_{ij}=\frac{n_{i+}}{n}\frac{n_{+j}}{n}$
  * sample variance from normal distributions
    * $\frac{(n-1)S^2}{\sigma^2}\sim \chi^2(n-1)$

## Test

* mean
  * not normal
    * `t.test(data)` intervallo di confidenza della media, opzionalmente secondo `conf.level` e `alternative`
      * $T$-statistic/mean, studentized sample mean
    * `t.test(data, mu = mu0)` verifica se la media è compatibile con mu, e calcola p-value
      * $T$-statistic/mean, studentized sample mean
  * normal
    * `prop.test(success, total, p = mu0, correct = FALSE)` su large enough Bernoulli, confronta `success/total` con `p` e calcola p-value, opzionalmente `conf.level`.

      esegue un $\chi^2$-test su $z^2$, con $SE$ appropriato
      * $Z$-statistic/proportions
  * comparison con $H_0: \theta_X-\theta_Y=0$
    * `t.test(data1, data2)` confronta medie con $\sigma^2$ simile
      * $T$-statistic/difference of mean, same variance
    * `t.test(data1, data2, var.equal = FALSE)` Welch test, confronta medie con $\sigma^2$ diverso
      * $T$-statistic/difference of mean, different variance
    * `t.test(data1, data2, paired = TRUE)` confronta medie con pairwise
      * $T$-statistic/mean difference, paired data
    * `prop.test(c(successX, successY), c(totalX, totalY), correct = FALSE)` su large enough Bernoulli, confronta `successX/totalX` con `successY/totalY` e calcola p-value, opzionalmente `conf.level`.

      esegue un $\chi^2$-test su $z^2$, con $S^2_p$ appropriato
      * $Z$-statistic/difference of proportions
* variance test
  * `var.test(data1, data2)` F test, confronta le varianze, opzionalmente `conf.level` e `alternative"`
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
  * tabella di contingenza di variabili categoriche
    * `chisq.test(contTable)` controlla la $\chi^2$-statistic.

      Pearson's $\chi^2$-test, p-value alto se indipendenti
      * $\chi^2$-statistic/large enough contingency table