Bayesian inference for state-space models with R
=============

[![Build Status](https://travis-ci.org/sbfnk/RBi.svg?branch=master)](https://travis-ci.org/sbfnk/RBi) [![codecov](https://codecov.io/github/sbfnk/RBi/branch/master/graphs/badge.svg)](https://codecov.io/github/sbfnk/RBi) 

[RBi](https://github.com/libbi/rbi) is an `R` interface to [libbi](http://libbi.org), a library for Bayesian inference.

It mainly contains:
- various functions to retrieve and process the results from libbi (which are in NetCDF format)
- a `bi_model` class, to manipulate libbi models
- a `libbi` wrapper class, to perform Bayesian using libbi inference from within R,

Installation
==============

**RBi** requires **R** (>= 2.12.1) as well as the packages:
- `reshape2`
- `ncdf4`
- `data.table`

The easiest way to install the latest stable version of **RBi** is via CRAN. The package is called `rbi` (all lower case):

```r
install.packages('rbi')
```

Alternatively, the current development version can be installed using the `devtools` package

```r
# install.packages("devtools")
library('devtools')
install_github("libbi/rbi")
```

The **RBi** package has only been tested on GNU/Linux and OS X, but it should mostly work everywhere `R` works.

If you want to use **RBi** as a wrapper to **LibBi** then you need a working version of [LibBi](https://github.com/libbi/LibBi). To install **LibBi** on a Mac or Unix, the easiest way is to install [Homebrew](http://brew.sh) (on OS X) or [Linuxbrew](http://linuxbrew.sh) (on linux), followed by (using a command shell, i.e. Terminal or similar):

```sh
brew install libbi
``` 

The path to `libbi` script can be passed as an argument to **RBi**, otherwise the package tries to find it automatically using the `which` linux/unix command.

If you just want to process the output from **LibBi**, then you do not need to have **LibBi** installed.

Getting started
==============

A good starting point is to look at the included demos:

```{r}
 demo(PZ_generate_dataset) ## generating a data set from a model
 demo(PZ_PMMH)             ## particle Markov-chain Metropolis-Hastings
 demo(PZ_SMC2)             ## SMC^2
 demo(PZ_filtering)        ## filtering
```

For further information, have a look at the introductory vignette from the link from the [rbi CRAN package](https://CRAN.R-project.org/package=rbi).

Using coda
==========

**LibBi** contains the `get_traces` method which provides an interface to [coda](https://cran.r-project.org/package=coda).

Other packages
==============

For higher-level methods to interact with [LibBi](https://github.com/libbi/LibBi), have a look at [RBi.helpers](https://github.com/sbfnk/rbi.helpers).
