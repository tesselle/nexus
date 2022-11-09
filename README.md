
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nexus <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/nexus/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/nexus/actions)
[![codecov](https://codecov.io/gh/tesselle/nexus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tesselle/nexus)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/nexus/badge/main)](https://www.codefactor.io/repository/github/tesselle/nexus/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/nexus)](https://cran.r-project.org/package=nexus)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/nexus"
alt="r-universe" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

Exploration and analysis of compositional data in the framework of
Aitchison (1986). This package provides tools for chemical
fingerprinting and source tracking of ancient materials.

**Initial development is in progress.**

## Installation

You can install the released version of **nexus** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nexus")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/nexus")
```

## Usage

``` r
## Load the package
library(nexus)
```

**nexus** provides a set of S4 classes that represent different special
types of matrix.

-   `CompositionMatrix` represents relative frequency data (composition
    data),
-   `LogRatio` represents log-ratio matrix.

*It assumes that you keep your data tidy*: each variable must be saved
in its own column and each observation (sample) must be saved in its own
row.

These new classes are of simple use as they inherit from base `matrix`:

``` r
## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)
```

The `CompositionMatrix` and `LogRatio` classes have special slots:

-   `samples` for replicated measurements/observation,
-   `groups` to group data by site/area.

When coercing a `data.frame` to a `CompositionMatrix` object, an attempt
is made to automatically assign values to these slots by mapping column
names. This behavior can be disabled by setting
`options(arkhe.autodetect = FALSE)`.

``` r
X <- matrix(data = sample(0:10, 75, TRUE), nrow = 15, ncol = 5)
X <- as.data.frame(X)
X$samples <- sample(c("a", "b", "c", "d", "e"), 15, TRUE)
X$groups <- sample(c("A", "B", "C"), 15, TRUE)

## Coerce to a count matrix
Y <- as_composition(X)

## Get groups
get_samples(Y)
#>  [1] "d" "c" "c" "d" "e" "c" "b" "a" "c" "a" "b" "e" "a" "d" "d"

## Get groups
get_groups(Y)
#>  [1] "A" "C" "C" "B" "C" "A" "B" "A" "A" "A" "C" "A" "B" "B" "C"
```

## Contributing

Please note that the **nexus** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-aitchison1986" class="csl-entry">

Aitchison, J. 1986. *The Statistical Analysis of Compositional Data*.
Monographs on Statistics and Applied Probability. Londres, UK ; New
York, USA: Chapman and Hall.
<http://dx.doi.org/10.1007/978-94-009-4109-0>.

</div>

</div>
