# EM-algorithm




### 1. Смоделировать $n$-мерную выборку ($n \ge 3$), состоящую из $N$ наблюдений из $\mathcal{N} (\mu. \Sigma)$. Параметры выбрать самостоятельно.

Define function for generating a random positive-definite matrix with user-specified positive eigenvalues. If eigenvalues are not specified, they are generated from a uniform distribution.

```r
PDmatrix <- function (n, ev = runif(n, 0, 10)) {
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}
```


Initialize variables

```r
N <- 10^3
n <- 3
mu <- runif(n, 0, 100)
sigma <- PDmatrix(n)

print(mu)
```

```
## [1] 81.454669 28.943491  5.926579
```

```r
print(sigma)
```

```
##             [,1]     [,2]        [,3]
## [1,]  9.64345671 0.154110 -0.02427311
## [2,]  0.15410996 3.036415  1.47574543
## [3,] -0.02427311 1.475745  8.94083165
```


Generate multidimensional sample with init values

```r
library(MASS)
sample <- mvrnorm(N, mu, sigma)

head(sample)
```

```
##          [,1]     [,2]      [,3]
## [1,] 78.58838 28.20656  8.639852
## [2,] 79.18383 27.58062  3.789758
## [3,] 80.47637 29.00900  6.927687
## [4,] 81.91013 29.95199  8.759349
## [5,] 82.62816 30.38148  4.907921
## [6,] 85.19651 30.53927 11.316145
```

### 2. Внести случайным образом пропуски. Желательно, чтобы можно было через некоторый параметр управлять, насколько много значений пропущено.

```r
percent <- 0.2
nas <- round(percent * N)

sample <- apply(sample, 2, function (x) {
  x[sample(1:N)[1:nas]] <- NA
  x
})

head(sample)
```

```
##          [,1]     [,2]     [,3]
## [1,] 78.58838 28.20656 8.639852
## [2,] 79.18383 27.58062       NA
## [3,]       NA 29.00900 6.927687
## [4,] 81.91013       NA 8.759349
## [5,] 82.62816 30.38148 4.907921
## [6,]       NA       NA       NA
```

### 3. Реализовать EM-алгоритм для оценивания параметров нормального распределения
