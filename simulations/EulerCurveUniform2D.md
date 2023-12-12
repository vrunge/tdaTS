---
title: "Study of Euler Curves for 2D Uniform data"
author: "Vincent Runge"
date: "12/12/2023"
output:
  html_document:
    keep_md: true
    css: styles.css
    toc: true
    toc_float: true
    highlight: tango
    number_sections: true
---





```r
library(TDA)
library(tdaTS) #our package
library(tibble)
library(interp) #delaunay triangulation
```

```
## Warning: package 'interp' was built under R version 4.2.3
```


# Delaunay triangulations



```r
n <- 1000
x <- runif(n)
y <- runif(n)
data <- data2D_pointTwoCirclesMerged(1000,overlap = 0.2)
res <- tri.mesh(x = data)
plot(res)
```

![](EulerCurveUniform2D_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
