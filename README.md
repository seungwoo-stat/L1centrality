
<!-- README.md is generated from README.Rmd. Please edit that file -->

# L1centrality

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/L1centrality)](https://CRAN.R-project.org/package=L1centrality)
<!-- badges: end -->

Graph/Network Analysis Based on L1 Centrality

## What is *L*<sub>1</sub> centrality?

*L*<sub>1</sub> centrality is a new centrality measure that assesses the
prominence of vertices in an undirected and connected graph (Kang and
Oh, 2023). It properly handles graphs that have weights assigned to both
vertices and edges. Based on this centrality measure, several graphical
and multiscale analysis tools are developed.

## How can I get L1centrality?

Version 0.0.2 of this package is available on
[CRAN](https://cran.r-project.org/package=L1centrality):

``` r
install.packages("L1centrality")
library(L1centrality)
```

You can install the development version (0.0.2.9000) of `L1centrality`
via:

``` r
devtools::install_github("seungwoo-stat/L1centrality")
library(L1centrality)
```

## How do I use it?

Using the Marvel Cinematic Universe movie network and *L*<sub>1</sub>
centrality function `L1cent()` provided via this package, we can
identify movies with high *L*<sub>1</sub> centrality, i.e., movies that
are central in the given network.

``` r
library(L1centrality)
L1cent(MCUmovie) |> 
  sort(decreasing = TRUE) |> 
  head()
#>  Avengers: Infinity War       Avengers: Endgame            The Avengers 
#>               1.0000000               0.6939885               0.5269421 
#>              Iron Man 2 Avengers: Age of Ultron  Spider-Man: Homecoming 
#>               0.4113450               0.3865311               0.3691427
```

There are other useful functions built upon the *L*<sub>1</sub>
centrality measure: `L1centMDS()` (target plot), `L1centNB()`
(*L*<sub>1</sub> centrality-based neighborhood), `L1centLOC()` (local
*L*<sub>1</sub> centrality), `L1centEDGE()` (multiscale edge
representation), and `Lorenz_plot()` (group heterogeneity plot and
index).

Moreover, two network data are provided: `MCUmovie` (Marvel Cinematic
Universe movie network) and `rokassembly21` (Republic of Korea’s 21st
National Assembly bill cosponsorship network). See the document for
further details.

## Where can I learn more?

Visit [this repo](https://github.com/seungwoo-stat/L1centrality-paper)
for code to reproduce the figures and analysis from the paper Kang and
Oh (2023).

## References

Seungwoo Kang and Hee-Seok Oh. On a Notion of Graph Centrality Based on
*L*<sub>1</sub> Data Depth. Technical Report. 2023.
