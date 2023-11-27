Final project update
================
SHARON
2023-08-09

``` r
A <- read.csv("C:/Users/PC/Downloads/copd raw dataset.csv")
B <- A[1:100, c(3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19)]
cor_matrix<-cor(B)
cor_matrix
```

    ##                       AGE  PackHistory    MWT1Best         FEV1     FEV1PRED
    ## AGE           1.000000000 -0.007895796 -0.23009296 -0.096014350  0.069880677
    ## PackHistory  -0.007895796  1.000000000 -0.25218319 -0.127804787 -0.130344817
    ## MWT1Best     -0.230092963 -0.252183187  1.00000000  0.469214160  0.390377728
    ## FEV1         -0.096014350 -0.127804787  0.46921416  1.000000000  0.776673903
    ## FEV1PRED      0.069880677 -0.130344817  0.39037773  0.776673903  1.000000000
    ## FVC          -0.136785408 -0.087568084  0.44570272  0.819332462  0.522127509
    ## FVCPRED       0.008493073 -0.004082314  0.25944386  0.516605690  0.625886338
    ## CAT           0.084514535 -0.143081796 -0.15448693 -0.065444077  0.007360731
    ## HAD          -0.223602294  0.031037202 -0.28742556 -0.151993169 -0.110615534
    ## SGRQ         -0.134720605  0.035692277 -0.53364267 -0.308306783 -0.333491357
    ## AGEquartiles  0.897199318  0.002200882 -0.21632479 -0.080799772  0.007169417
    ## copd         -0.022188155  0.068877577 -0.41761890 -0.771017088 -0.866165527
    ## smoking       0.122593889  0.018607334  0.02541031  0.008647888  0.032097072
    ##                      FVC      FVCPRED          CAT         HAD        SGRQ
    ## AGE          -0.13678541  0.008493073  0.084514535 -0.22360229 -0.13472061
    ## PackHistory  -0.08756808 -0.004082314 -0.143081796  0.03103720  0.03569228
    ## MWT1Best      0.44570272  0.259443865 -0.154486927 -0.28742556 -0.53364267
    ## FEV1          0.81933246  0.516605690 -0.065444077 -0.15199317 -0.30830678
    ## FEV1PRED      0.52212751  0.625886338  0.007360731 -0.11061553 -0.33349136
    ## FVC           1.00000000  0.624761306 -0.160322748 -0.13524279 -0.22685420
    ## FVCPRED       0.62476131  1.000000000 -0.139068330 -0.15330264 -0.28998692
    ## CAT          -0.16032275 -0.139068330  1.000000000  0.16176467  0.28782893
    ## HAD          -0.13524279 -0.153302641  0.161764675  1.00000000  0.39416884
    ## SGRQ         -0.22685420 -0.289986922  0.287828928  0.39416884  1.00000000
    ## AGEquartiles -0.10778270 -0.107834755  0.130016993 -0.20509986 -0.07144873
    ## copd         -0.55175070 -0.643755370  0.020008915  0.16018269  0.32738354
    ## smoking       0.01180189  0.129488070 -0.026876647 -0.06819844 -0.20212695
    ##              AGEquartiles        copd      smoking
    ## AGE           0.897199318 -0.02218815  0.122593889
    ## PackHistory   0.002200882  0.06887758  0.018607334
    ## MWT1Best     -0.216324795 -0.41761890  0.025410308
    ## FEV1         -0.080799772 -0.77101709  0.008647888
    ## FEV1PRED      0.007169417 -0.86616553  0.032097072
    ## FVC          -0.107782702 -0.55175070  0.011801892
    ## FVCPRED      -0.107834755 -0.64375537  0.129488070
    ## CAT           0.130016993  0.02000892 -0.026876647
    ## HAD          -0.205099860  0.16018269 -0.068198445
    ## SGRQ         -0.071448734  0.32738354 -0.202126950
    ## AGEquartiles  1.000000000  0.02884067  0.058560801
    ## copd          0.028840672  1.00000000  0.098833242
    ## smoking       0.058560801  0.09883324  1.000000000

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(RColorBrewer)
corrplot(cor_matrix, method = "color")
```

![](Final_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
B$HAD[B$HAD >=22] <- NA
C<- B[!is.na(B$HAD), ] ##subsetting to remove missing observation in HAD 
C<- C[, c(3, 9, 10, 12, 13)]
MWT1Best<- C$MWT1Best
SGRQ <- C$SGRQ
HAD<- C$HAD
smoking<- as.factor(C$smoking)
copd<- as.factor(C$copd)
head(C)
```

    ##   MWT1Best HAD  SGRQ copd smoking
    ## 1      120   8 69.55    3       2
    ## 2      176  21 44.24    2       2
    ## 3      201  18 44.09    2       2
    ## 5      210  18 75.56    3       2
    ## 6      216  21 73.82    2       1
    ## 8      237   2 45.41    3       2

``` r
quant<- C[, c(1, 2, 3)] ##subsetting quantitative variables for plotting
plot(quant)
```

![](Final_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
cor(C) ##correlation matrix
```

    ##              MWT1Best        HAD       SGRQ        copd      smoking
    ## MWT1Best  1.000000000 -0.3401884 -0.4774372 -0.34494090  0.008468273
    ## HAD      -0.340188355  1.0000000  0.5500243  0.27007977 -0.109407705
    ## SGRQ     -0.477437239  0.5500243  1.0000000  0.28658463 -0.162614257
    ## copd     -0.344940899  0.2700798  0.2865846  1.00000000  0.092227939
    ## smoking   0.008468273 -0.1094077 -0.1626143  0.09222794  1.000000000

``` r
nrow(C)
```

    ## [1] 89

``` r
##exploratory data analysis

SGRQ<- C$SGRQ
hist(SGRQ, # histogram
     col="steelblue", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "SGRQ",
     breaks = 20,
     main = "Histogram of quality of life scores")
lines(density(SGRQ), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
```

![](Final_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
table(C$copd)
```

    ## 
    ##  1  2  3  4 
    ## 20 38 26  5

``` r
table(C$smoking)
```

    ## 
    ##  1  2 
    ## 14 75

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.2

``` r
describe(C$HAD)
```

    ##    vars  n mean   sd median trimmed  mad min max range skew kurtosis  se
    ## X1    1 89 9.13 5.71      9    8.95 5.93   0  21    21 0.28    -0.84 0.6

``` r
describe(C$MWT1Best)
```

    ##    vars  n   mean     sd median trimmed   mad min max range  skew kurtosis
    ## X1    1 89 405.18 104.51    431  409.15 85.99 120 699   579 -0.39     0.02
    ##       se
    ## X1 11.08

``` r
describe(C$SGRQ)
```

    ##    vars  n mean    sd median trimmed   mad  min   max range skew kurtosis   se
    ## X1    1 89 38.7 16.75  36.74   38.31 15.51 8.12 75.56 67.44 0.24    -0.62 1.78

``` r
##variable selection
library(bestglm)
```

    ## Loading required package: leaps

``` r
##creating new dataframe
D<- data.frame(MWT1Best=C$MWT1Best, HAD=C$HAD, copd=C$copd, smoking= C$smoking, SGRQ=C$SGRQ)
bestglm(D, IC = "AIC", method = "forward")
```

    ## AIC
    ## BICq equivalent for q in (0.0134523756915128, 0.785369240197715)
    ## Best Model:
    ##                Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 48.26129634 7.09913299  6.798196 1.321964e-09
    ## MWT1Best    -0.05261334 0.01425963 -3.689672 3.929274e-04
    ## HAD          1.28674591 0.26121600  4.925984 4.024188e-06

``` r
bestglm(D, IC = "AIC", method = "backward")
```

    ## AIC
    ## BICq equivalent for q in (0.0134523756915128, 0.785369240197715)
    ## Best Model:
    ##                Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 48.26129634 7.09913299  6.798196 1.321964e-09
    ## MWT1Best    -0.05261334 0.01425963 -3.689672 3.929274e-04
    ## HAD          1.28674591 0.26121600  4.925984 4.024188e-06

``` r
bestglm(D, IC = "AIC", method = "exhaustive")
```

    ## AIC
    ## BICq equivalent for q in (0.0134523756915128, 0.785369240197715)
    ## Best Model:
    ##                Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 48.26129634 7.09913299  6.798196 1.321964e-09
    ## MWT1Best    -0.05261334 0.01425963 -3.689672 3.929274e-04
    ## HAD          1.28674591 0.26121600  4.925984 4.024188e-06

``` r
models <- regsubsets(C$SGRQ~., data = C, nvmax = 4)
summary(models)
```

    ## Subset selection object
    ## Call: regsubsets.formula(C$SGRQ ~ ., data = C, nvmax = 4)
    ## 4 Variables  (and intercept)
    ##          Forced in Forced out
    ## MWT1Best     FALSE      FALSE
    ## HAD          FALSE      FALSE
    ## copd         FALSE      FALSE
    ## smoking      FALSE      FALSE
    ## 1 subsets of each size up to 4
    ## Selection Algorithm: exhaustive
    ##          MWT1Best HAD copd smoking
    ## 1  ( 1 ) " "      "*" " "  " "    
    ## 2  ( 1 ) "*"      "*" " "  " "    
    ## 3  ( 1 ) "*"      "*" " "  "*"    
    ## 4  ( 1 ) "*"      "*" "*"  "*"

``` r
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic))
```

    ##   Adj.R2 CP BIC
    ## 1      3  2   2

``` r
lm<- lm(C$SGRQ~C$MWT1Best+C$HAD)
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = C$SGRQ ~ C$MWT1Best + C$HAD)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.1743  -9.6250   0.0713   9.9015  26.0976 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 48.26130    7.09913   6.798 1.32e-09 ***
    ## C$MWT1Best  -0.05261    0.01426  -3.690 0.000393 ***
    ## C$HAD        1.28675    0.26122   4.926 4.02e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.15 on 86 degrees of freedom
    ## Multiple R-squared:  0.3978, Adjusted R-squared:  0.3838 
    ## F-statistic: 28.41 on 2 and 86 DF,  p-value: 3.368e-10

``` r
library(MASS)
rs1<- studres(lm)
plot(rs1~fitted(lm))
abline(h=0)
```

![](Final_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
bptest(lm)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  lm
    ## BP = 4.267, df = 2, p-value = 0.1184

``` r
ks.test(rs1,"pnorm")
```

    ## Warning in ks.test.default(rs1, "pnorm"): ties should not be present for the
    ## Kolmogorov-Smirnov test

    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  rs1
    ## D = 0.063759, p-value = 0.8623
    ## alternative hypothesis: two-sided

``` r
summary(lm)
```

    ## 
    ## Call:
    ## lm(formula = C$SGRQ ~ C$MWT1Best + C$HAD)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -30.1743  -9.6250   0.0713   9.9015  26.0976 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 48.26130    7.09913   6.798 1.32e-09 ***
    ## C$MWT1Best  -0.05261    0.01426  -3.690 0.000393 ***
    ## C$HAD        1.28675    0.26122   4.926 4.02e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.15 on 86 degrees of freedom
    ## Multiple R-squared:  0.3978, Adjusted R-squared:  0.3838 
    ## F-statistic: 28.41 on 2 and 86 DF,  p-value: 3.368e-10

``` r
set.seed(123)

ntest<- 50
rpmse_m<-numeric(ntest)
cvg_m <- numeric(ntest)
for (i in 1:ntest) {
  
  index <- sample(1:89, 20, replace = FALSE)
  testy <- SGRQ[index]; trainy <- SGRQ[-index]
  testHAD <- HAD[index]; trainHAD <- HAD[-index]
  testMWT1B <- MWT1Best[index]; trainMWT1B <- MWT1Best[-index]
  
  lm1<- lm(trainy~ trainHAD+trainMWT1B)
  
  pred1<- predict(lm1, data.frame(trainHAD=testHAD, trainMWT1B= testMWT1B), interval = "prediction")
 
  
  
  rpmse_m[i]<- sqrt(mean((pred1[,1]-testy)^2))

  
  cvg_m[i] <- mean(pred1[,2] < testy & pred1[,3] > testy)

  
}

mean(rpmse_m)
```

    ## [1] 13.13649

``` r
mean(cvg_m)
```

    ## [1] 0.972

``` r
pred1
```

    ##         fit       lwr      upr
    ## 1  37.18066 11.138426 63.22290
    ## 2  42.93066 16.338641 69.52268
    ## 3  32.22081  6.092456 58.34916
    ## 4  49.82822 23.678716 75.97773
    ## 5  23.95180 -2.581804 50.48541
    ## 6  46.96259 20.911510 73.01368
    ## 7  44.89066 18.753145 71.02817
    ## 8  32.12761  5.998923 58.25630
    ## 9  38.20549 12.135530 64.27544
    ## 10 34.05047  8.010833 60.09011
    ## 11 38.46634 12.409055 64.52362
    ## 12 37.59431 11.598934 63.58968
    ## 13 23.95180 -2.581804 50.48541
    ## 14 31.76985  5.486813 58.05289
    ## 15 34.46411  8.435215 60.49301
    ## 16 44.76403 18.593236 70.93483
    ## 17 42.50199 16.174859 68.82911
    ## 18 32.70148  6.576157 58.82681
    ## 19 41.63384 15.293804 67.97388
    ## 20 31.76985  5.486813 58.05289

``` r
confint(lm)
```

    ##                   2.5 %      97.5 %
    ## (Intercept) 34.14868703 62.37390566
    ## C$MWT1Best  -0.08096054 -0.02426615
    ## C$HAD        0.76746569  1.80602613
