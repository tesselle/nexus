
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nexus <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/nexus/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/nexus/actions)
[![codecov](https://codecov.io/gh/tesselle/nexus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tesselle/nexus)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/nexus/badge/main)](https://www.codefactor.io/repository/github/tesselle/nexus/overview/main)
[![Dependencies](https://tinyverse.netlify.app/badge/nexus)](https://cran.r-project.org/package=nexus)

<a href="https://tesselle.r-universe.dev/nexus"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/nexus"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=nexus"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/nexus"
alt="CRAN Version" /></a>
<a href="https://cran.r-project.org/web/checks/check_results_nexus.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/nexus.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=nexus"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/nexus" alt="CRAN Downloads" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10225630.svg)](https://doi.org/10.5281/zenodo.10225630)
<!-- badges: end -->

## Overview

Exploration and analysis of compositional data in the framework of
Aitchison (1986). **nexus** provides tools for chemical fingerprinting
and source tracking of ancient materials. This package provides methods
for compositional data analysis:

- Logratio transformations: `transform_lr()`, `transform_clr()`,
  `transform_alr()`, `transform_ilr()`, `transform_plr()`.
- Compositional statistics.
- Zero and missing value replacement.
- Outlier detection: `outliers()`.

This package also includes methods for provenance studies:

- Multivariate analysis: `pca()`.
- Mixed-mode analysis using geochemical and petrographic data (Baxter et
  al. 2008): `mix()`.

[**isopleuros**](https://packages.tesselle.org/isopleuros/) is a
companion package to **nexus** that allows to create ternary plots.

------------------------------------------------------------------------

To cite nexus in publications use:

Frerebeau N, Philippe A (2024). *nexus: Sourcing Archaeological
Materials by Chemical Composition*. Université Bordeaux Montaigne,
Pessac, France. <doi:10.5281/zenodo.10225630>
<https://doi.org/10.5281/zenodo.10225630>, R package version 0.3.0,
<https://packages.tesselle.org/nexus/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

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
#> Loading required package: dimensio
```

**nexus** provides a set of S4 classes that represent different special
types of matrix. The most basic class represents a compositional data
matrix, i.e. quantitative (nonnegative) descriptions of the parts of
some whole, carrying relative, rather than absolute, information
(Aitchison 1986).

*It assumes that you keep your data tidy*: each variable must be saved
in its own column and each observation (sample) must be saved in its own
row.

These new classes are of simple use as they inherit from base `matrix`
(see `vignette("nexus")`):

``` r
## Mineral compositions of rock specimens
## Data from Aitchison 1986
data("hongite")
head(hongite)
#>       A    B    C    D    E
#> H1 48.8 31.7  3.8  6.4  9.3
#> H2 48.2 23.8  9.0  9.2  9.8
#> H3 37.0  9.1 34.2  9.5 10.2
#> H4 50.9 23.8  7.2 10.1  8.0
#> H5 44.2 38.3  2.9  7.7  6.9
#> H6 52.3 26.2  4.2 12.5  4.8

## Coerce to compositional data
coda <- as_composition(hongite)
head(coda)
#> <CompositionMatrix: 6 x 5>
#>        A     B     C     D     E
#> H1 0.488 0.317 0.038 0.064 0.093
#> H2 0.482 0.238 0.090 0.092 0.098
#> H3 0.370 0.091 0.342 0.095 0.102
#> H4 0.509 0.238 0.072 0.101 0.080
#> H5 0.442 0.383 0.029 0.077 0.069
#> H6 0.523 0.262 0.042 0.125 0.048

## Compositional barplots
barplot(coda, order = 1)
```

![](man/figures/README-groups-1.png)<!-- -->

## Contributing

Please note that the **nexus** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-aitchison1986" class="csl-entry">

Aitchison, J. 1986. *The Statistical Analysis of Compositional Data*.
Monographs on Statistics and Applied Probability. Londres, UK ; New
York, USA: Chapman and Hall.

</div>

<div id="ref-baxter2008" class="csl-entry">

Baxter, M. J., C. C. Beardah, I. Papageorgiou, M. A. Cau, P. M. Day, and
V. Kilikoglou. 2008. “On Statistical Approaches to the Study of Ceramic
Artefacts Using Geochemical and Petrographic Data.” *Archaeometry* 50
(1): 142–57. <https://doi.org/10.1111/j.1475-4754.2007.00359.x>.

</div>

</div>
