RBi -- INSTALL
==============

`RBi` is an `R` package requiring `R` (>= 2.12.1) as well as the packages:
- `ggplot2`
- `ncdf`
- `Rcpp` (>= 0.10.2)
- `stringr`

It has only been tested on GNU/Linux, but it should mostly work everywhere `R` works.

If you want to use `RBi` as a wrapper to [libbi] (https://github.com/libbi/LibBi) then you need a working version of [libbi] (https://github.com/libbi/LibBi). The path to `libbi` script can be passed as an argument, otherwise the package tries to find it automatically (see the function `bi_settings()`) using the `which` linux/unix command.

If you just want to process the output from [libbi] (https://github.com/libbi/LibBi), then you do not need to have [libbi] (https://github.com/libbi/LibBi) installed.
