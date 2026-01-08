[![](img/sd.svg)](https://en.wikipedia.org/wiki/Standard_error#Derivation)
![](img/notes.png)
![](img/notes_2.png)

![](img/notes_3.png)

---
---

![](img/notes_1.png)

---
---

## Statistics

* mean
  * z statistic, standardized sample mean
    * $Z=\frac{\overline Y-\mu}{\sigma/\sqrt n}\sim N(0,1)$
  * t statistic, studentized sample mean
    * $Z=\frac{\overline Y-\mu}{S/\sqrt n}\sim t_{n-1}$
* variance
  * observation from normal distribution
    * $\frac{(n-1)S^2}{\sigma^2}\sim \chi^2(n-1)$
    * sample variance ratio
      * $SVR=\frac{S^2_X/\sigma^2_X}{S^2_Y/\sigma^2_Y}\sim F(n_X-1,n_Y-1)$

## Standard Error

Mean Square Error $MSE=E[(\hat\theta-\theta)^2]=V(\hat\theta)+|E[\hat\theta]-\theta|^2=V(\hat\theta)+Bias^2$\
Standard Error $SE=\sqrt{MSE}$. Con $\hat\theta$ è **unbiased** $SE=\sqrt{V(\hat\theta)}$ 

* mean
  * $\widehat{SEM}=\sqrt{\frac{\sigma^2}{n}}$
  * $SEM=\sqrt{\frac{S^2}{n}}$
* proportion
  * $SE=\sqrt{\frac{p(1-p)}{n}}$
* difference
  * varianza diversa
    * difference/mean
      * $SED=\sqrt{SEM^2_X+SEM^2_Y}$
  * pooled variance $\widehat{SED}=\sqrt{S^2_p\left(\frac1{n_X}+\frac1{n_y}\right)}$
    * difference/mean
      * $S^2_p=\frac{S^2_1(n_1-1)+S^2_2(n_2-1)}{n_1+n_2-2}$
    * proportions
      * $S^2_p=\hat p(1-\hat p)$

## Test

| test | comando | scopo |
| - | - | - |
| intervallo di confidenza | `t.test(data)` | trovare l'intervallo di confidenza della media di una variabile |
| t test | `t.test(data, mu = h0)` | testare se  |


* mean
  * not normal
    * `t.test(data)` intervallo di confidenza della media, opzionalmente secondo `conf.level = 0.95` e `alternative = "two.sided"`
    * `t.test(data, mu=h0)` verifica se la media è compatibile con mu, e calcola p-value
  * comparison
    * `t.test(data1, data1)` confronta medie con $\sigma^2$ simile
      * $T=\frac{\overline X-\overline Y}{\widehat{SED}}$
    * `t.test(data1, data1, var.equal = FALSE)` Welch test, confronta medie con $\sigma^2$ diverso
      * $T=\frac{\overline X-\overline Y}{SED}$
    * `t.test(data1, data1, paired = TRUE)` confronta medie con pairwise
  * normal
    * large enough Bernoulli
      * z-test with z under null- hypothesis
      * `prop.test(success, total, p = mu0, correct = FALSE)` confronta `success/total` con `p` e calcola p-value, opzionalmente `conf.level = 0.95`
        * esegue un $\chi^2$-test su $z^2$
* variance test
  * `var.test(data1, data2)` F test, confronta le varianze, opzionalmente `conf.level = 0.95` e `alternative = "two.sided"`
    * calcola p-value of $SVR$
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