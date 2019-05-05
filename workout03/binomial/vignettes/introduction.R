---
title: "Introduction"
author: "Daiqiao Lin"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
---

```{r, echo = FALSE, message = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


library(binomial)
```

# The Binomial Package
`"binomial"` is a self-created R package that includes functions for calculating probabilities of a Binomial random variable, and also some related calculations shown below.





##statistics of binomial variable
 `bin_mean()`: show the mean of binomial random variable
 `bin_variance()`: show the variance of binomial random variable
 `bin_mode()`: show the mode of binomial random variable
 `bin_skewness()`: show the skewness of binomial random variable
 `bin_kurtosis()`: show the kurtosis of binomial random variable


```{r}
bin_mean(10, 0.4)
bin_variance(10, 0.4)
bin_mode(10, 0.4)
bin_skewness(10, 0.4)
bin_kurtosis(10, 0.4)
```
