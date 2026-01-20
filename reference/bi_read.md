# Bi Read

This function reads all variable from a NetCDF file or the output of a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object. The
file can be specified as a string to the filepath, in which case a
NetCDF connection is opened, or directly as a NetCDF connection.

## Usage

``` r
bi_read(
  x,
  vars,
  dims,
  model,
  type,
  file,
  missval_threshold,
  coord_dims = list(),
  thin,
  verbose = FALSE,
  clear_cache = FALSE,
  init_to_param = FALSE,
  burn = 0
)
```

## Arguments

- x:

  either a path to a NetCDF file, or a NetCDF connection created using
  `nc_open`, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object from
  which to read the output

- vars:

  variables to read; if not given, all will be read

- dims:

  factors for dimensions

- model:

  model file or a `bi_model` object (if `x` is not a `libbi` object)

- type:

  vector of types of variable to read (out of "param", "state", "noise",
  "obs"). This needs 'x' to be a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object or
  `model` to be specified

- file:

  which file to read (if `x` is given as a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object): one
  of "output" (default), "init", "input", "obs"

- missval_threshold:

  upper threshold for the likelihood

- coord_dims:

  any `coord` dimensions, given as a named list of character vectors,
  where each element corresponds to the variable of the same name, and
  the character vector are the `coord` dimensions

- thin:

  thinning (keep only 1/thin of samples)

- verbose:

  if TRUE, will print variables as they are read

- clear_cache:

  if TRUE, will clear the cache and re-read the file even if cached data
  exists

- init_to_param:

  logical; if TRUE, convert states to initial values

- burn:

  number of initial samples to discard; default: 0

## Value

a list of data frames and/or numbers that have been read

## Examples

``` r
example_output_file <- system.file(package = "rbi", "example_output.nc")
d <- bi_read(example_output_file)
```
