Bayesian inference for state-space models with R
================

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/sbfnk/rbi)
[![R-CMD-check](https://github.com/sbfnk/rbi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbfnk/rbi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/sbfnk/rbi/branch/main/graph/badge.svg?token=vK4TWfgYo0)](https://codecov.io/github/sbfnk/rbi)
![GitHub
contributors](https://img.shields.io/github/contributors/sbfnk/rbi)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

[rbi](https://github.com/sbfnk/rbi) is an `R` interface to
[libbi](https://libbi.org), a library for Bayesian inference.

It mainly contains:

- various functions to retrieve and process the results from libbi
  (which are in NetCDF format)
- a `bi_model` class, to manipulate libbi models
- a `libbi` wrapper class, to perform Bayesian using libbi inference
  from within R,

# Installation

The easiest way to install the latest stable version of **rbi** is via
[CRAN](https://cran.r-project.org/package=rbi):

``` r
install.packages("rbi")
```

Alternatively, the current development version can be installed using
the `remotes` package

``` r
# install.packages("remotes")
library("remotes")
install_github("sbfnk/rbi")
```

The **rbi** package has only been tested on GNU/Linux and OS X, but it
should mostly work everywhere `R` works.

If you want to use **rbi** as a wrapper to **LibBi** then you need a
working version of [LibBi](https://github.com/lawmurray/LibBi). To
install **LibBi** on a Mac, the easiest way is to install
[Homebrew](https://brew.sh), followed by (using a command shell,
i.e. Terminal or similar):

``` sh
brew install libbi
```

On linux, follow the
[instructions](https://github.com/lawmurray/LibBi/blob/master/INSTALL_LINUX.md)
provided with LibBi.

The path to `libbi` script can be passed as an argument to **rbi**,
otherwise the package tries to find it automatically using the `which`
linux/unix command.

If you just want to process the output from **LibBi**, then you do not
need to have **LibBi** installed.

# Getting started

A good starting point is to look at the included demos:

``` r
demo(PZ_generate_dataset) ## generating a data set from a model
demo(PZ_PMMH) ## particle Markov-chain Metropolis-Hastings
demo(PZ_SMC2) ## SMC^2
demo(PZ_filtering) ## filtering
```

For further information, have a look at the introductory vignette from
the link from the [rbi CRAN
package](https://CRAN.R-project.org/package=rbi).

# Using coda

**LibBi** contains the `get_traces` method which provides an interface
to [coda](https://cran.r-project.org/package=coda).

# Other packages

For higher-level methods to interact with
[LibBi](https://github.com/lawmurray/LibBi), have a look at
[rbi.helpers](https://github.com/sbfnk/RBi.helpers).
