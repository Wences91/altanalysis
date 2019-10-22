
[![Build
Status](https://travis-ci.org/Wences91/altanalysis.svg?branch=master)](https://travis-ci.org/Wences91/altanalysis)

# Altmetric analysis

Stats and networks for altmetric mentions in R

## Installation

``` r
# Install from GitHub
# install.packages('devtools')
devtools::install_github('Wences91/altanalysis')
```

## Example

### Load data

``` r
mentions <- read.csv('mentions-sample.csv',
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
```

### Create co-mentions

``` r
library(altanalysis)
co_mentions <- co_authors(mentions)
## Not duplicated actor names
```

### Create co-mentions network

``` r
edges <- co_mentions$mentions
nodes <- co_mentions$actors

par(mar = c(0, 0, 0, 0))
network(nodes, edges, co_min = 1, min = 3)
## Q: 0.55
```

<img src="README_figs/README-unnamed-chunk-5-1.png" width="672" />
