
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scdv

An R package for SCDV (Sparse Composite Document Vectors) algorithm

[![Travis-CI Build
Status](https://api.travis-ci.com/teramonagi/scdv.svg?branch=master)](https://travis-ci.com/teramonagi/scdv)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/gghighlight)](https://cran.r-project.org/package=scdv)

## Installation

``` r
# install.packages("gghighlight")

# The development version from GitHub:
# install.packages("devtools")
devtools::install_github("teramonagi/scdv")
```

## Example

``` r
library(scdv)
# Get example document from Project Gutenberg (http://www.gutenberg.org/wiki/Main_Page)
urls <- c(
  "http://www.gutenberg.org/files/98/98-0.txt",
  "http://www.gutenberg.org/files/1342/1342-0.txt"
)
x <- purrr::map(urls, ~ httr::content(httr::GET(.x)))
doc <- purrr::map(x, ~ tokenizers::tokenize_words(.x, stopwords = stopwords::stopwords("en"))[[1]])
# Set the number of cluster (k), and the word2vec dimension (dimension)
k <- 10
dimension <- 100
# Calculate Sparse Composite Document Vector
dv <- scdv::scdv(doc, k, dimension)
```
