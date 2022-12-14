
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecsymr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/rgayler/vecsymr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rgayler/vecsymr?branch=main)
[![R-CMD-check](https://github.com/rgayler/vecsymr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rgayler/vecsymr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of {vecsymr} is to implement Vector Symbolic Architecture (VSA)
primitives to support experimentation. It is intended to be a simple VSA
implementation (the VSA equivalent of the geneticist’s fruit fly) to
provide a convenient base for experimentation. The design choices are
[my](https://www.rossgayler.com) personal preferences to support my
research. The initial emphasis is on functionality and flexibility with
no specific concern for performance.

I believe that phasor VSAs (where the vector elements are unit magnitude
complex numbers) are the best choice for basic VSAs. However, I have
initially imported functions for bipolar VSAs from
[VSA_altitude_hold](https://github.com/rgayler/VSA_altitude_hold) to
provide some code while I get the hang of writing an R package. Once the
package contains enough phasor VSA code I will probably remove the
bipolar VSA code. The phasor VSA code will probably include some extra
features to support non-negativity and experimentation with clean-up
memory.

## Installation

You can install the development version of {vecsymr} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rgayler/vecsymr")
```

There is currently no intention to put this package on CRAN. If it turns
out to be sufficiently useful and general I may try get it accepted as
an [rOpenSci](https://ropensci.org/packages/) package.

The current implementation is experimental. I expect the functional
content to evolve as I work out what I want this package to do. I also
expect the API to evolve as I work out how to make it easier to work
with. If you want to do any reproducible work with the package you will
need to use something like [{renv}](https://rstudio.github.io/renv/) to
freeze the version in use.

## Ignore below here

Everything after this point is just boilerplate to be edited.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(vecsymr)
## basic example code
```

## Remember

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

If you create figures in the README don’t forget to commit and push the
resulting figure files, so they display on GitHub.
