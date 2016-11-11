# rbi 0.4.1.9000

* "client" is now an option to libbi$run
* Fix bug in parsing options in libbi$initialize
* Fix bug in get_traces if there is only one sample
* bi_model$insert_lines can now work with blocks

# rbi 0.4.1

* Change package name to lower case
* Tidy up for CRAN submission
* Fix finding libbi path
* Fix handling of config options (#2, @tyler-abbot)
* Fix options in `bi_generate_dataset` (#3, #5, @tyler-abbot)
* Fix demos (#1, #6, @tyler-abbot)
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
