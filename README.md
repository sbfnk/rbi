Bayesian inference for state-space models with R
=============

[rbi] (https://github.com/libbi/RBi) is an `R` interface to [libbi] (https://github.com/libbi/LibBi), a library for Bayesian inference.

It mainly contains:
- various functions to retrieve and process the results from libbi (which are in NetCDF format)
- a `bi_model` class, to manipulate libbi models
- a `libbi` wrapper class, to perform Bayesian using libbi inference from within R,

Installation
==============

`rbi` requires `R` (>= 2.12.1) as well as the packages:
- `reshape2`
- `ncdf4`
- `data.table`

The easiest way to install the latest stable version of `rbi` is via CRAN:

```r
install.packages('rbi')
```

Alternatively, the current development version can be installed using the `devtools` package

```r
# install.packages("devtools")
library('devtools')
install_github("libbi/rbi")
```

The `rbi` package has only been tested on GNU/Linux and OS X, but it should mostly work everywhere `R` works.

If you want to use `rbi` as a wrapper to [libbi](https://github.com/libbi/LibBi) then you need a working version of [libbi](https://github.com/libbi/LibBi). To install `libbi` on a Mac or Unix, the easiest way is via the homebrew-science tap: Install [Homebrew](http://brew.sh) (on OS X) or [Linuxbrew](http://linuxbrew.sh) (on linux), then issue the following commands (using a command shell, i.e. Terminal or similar):

```sh
brew tap homebrew/science
brew install libbi
``` 

The path to `libbi` script can be passed as an argument to `rbi`, otherwise the package tries to find it automatically using the `which` linux/unix command.

If you just want to process the output from [libbi](https://github.com/libbi/LibBi), then you do not need to have [libbi](https://github.com/libbi/LibBi) installed.

Getting started
==============

A good starting point is to look at the included demos:

```{r}
 demo(PZ_generate_dataset) ## generating a data set from a model
 demo(PZ_PMMH)             ## particle Markov-chain Metropolis-Hastings
 demo(PZ_SMC2)             ## SMC^2
 demo(PZ_filtering)        ## filtering
```

Using coda
==========

`rbi` contains the `get_traces` method which provides an interface to [coda](https://cran.r-project.org/package=coda).

Other packages
==============

For higher-level methods to interact with [libbi](https://github.com/libbi/LibBi), have a look at [rbi.helpers](https://github.com/sbfnk/rbi.helpers).
