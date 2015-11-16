RBi -- Bayesian inference for state-space models with R
=============

[RBi] (https://github.com/libbi/RBi) is an interface to [libbi] (https://github.com/libbi/LibBi), a library for Bayesian Inference.

It mainly contains:
- a wrapper to call libbi (called "libbi"), allowing to perform the full inference from within R,
- various functions to retrieve the results (which are in the NetCDF format) from libbi and to process them.

Installation
==============

`RBi` is an `R` package requiring `R` (>= 2.12.1) as well as the packages:
- `reshape2`
- `ncdf4`

The easiest way to install `RBi` is to use the `devtools` package:

```r
# install.packages("devtools")
library(devtools)
install_github("sbfnk/RBi")
```

It has only been tested on GNU/Linux and OS X, but it should mostly work everywhere `R` works.

If you want to use `RBi` as a wrapper to [libbi] (https://github.com/libbi/LibBi) then you need a working version of [libbi] (https://github.com/libbi/LibBi). The path to `libbi` script can be passed as an argument, otherwise the package tries to find it automatically using the `which` linux/unix command.

If you just want to process the output from [libbi] (https://github.com/libbi/LibBi), then you do not need to have [libbi] (https://github.com/libbi/LibBi) installed.

Getting started
==============

A good starting point is to use the included demos:

```{r}
 demo(PZ_generate_dataset) ## generating a data set from a model
 demo(PZ_PMMH)             ## particle Markov-chain Metropolis-Hastings
 demo(PZ_SMC2)             ## SMC^2
 demo(PZ_filtering)        ## filtering
```
