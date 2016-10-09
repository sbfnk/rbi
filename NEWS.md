# rbi 0.5

* Change package name to lower case
* Tidy up for CRAN submission
* Fix finding libbi path
* Improve handling of config options
* Remove `stringr` dependency

# rbi 0.4

* Guess time dimension in netCDF files
* Bug fixes to better deal with multiple coordinate variables
* `burnin` option to `get_traces`
* Reduce memory use when thinning

# rbi 0.3

* Bug fixes in `bi_write`
* Improve dealing with different types of dimensions, including introduction of the `coord_dim` option in `bi_write`
* `bi_generate_dataset` now returns observations
* Remove `Rcpp` dependency

# rbi 0.2

* Remove C code and use `ncdf4` package for interaction with netCDF files
* Introduce the `bi_model` class to manipulate models
* Allow giving `init`, `obs` and `input` as R objects
* Rename `bi_wrapper` to `libbi`
* Improved `bi_read` function to directly read from `libbi` objects

# rbi 0.1

* initial version
