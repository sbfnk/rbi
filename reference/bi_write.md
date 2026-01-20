# Create (e.g., init or observation) files for LibBi

This function creates (or appends to) a NetCDF file for LibBi from the
given list of vectors and/or data frames. Since any files can be passed
to [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) directly
via the `init`, `input` and `obs` options, this is mostly used
internally, this is mostly used internally.

## Usage

``` r
bi_write(
  filename,
  variables,
  append = FALSE,
  overwrite = FALSE,
  time_dim,
  coord_dims,
  dim_factors,
  value_column = "value",
  guess_time = FALSE,
  verbose
)
```

## Arguments

- filename:

  a path to a NetCDF file to write the variables into, which will be
  overwritten if it already exists. If necessary, ".nc" will be added to
  the file name

- variables:

  a `list` object, the names of which should be the variable names and
  values should be either single values or data frames

- append:

  if TRUE, will append variables if file exists; default: FALSE

- overwrite:

  if TRUE, will overwrite variables if file exists; default: FALSE

- time_dim:

  the name of the time dimension, if one exists; default: "time"

- coord_dims:

  the names of the coordinate dimension, if any; should be a named list
  of character vectors, they are matched to variables names

- dim_factors:

  factors that dimensions have; this corresponds to the `dims` element
  of a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- value_column:

  if any `variables` are data frames, which column contains the values
  (default: "value")

- guess_time:

  whether to guess time dimension; this would be a numerical column in
  the data frame given which is not the `value_column`; only one such
  column must exist

- verbose:

  if TRUE, will print variables as they are read

## Value

A list of the time and coord dims, and factors in extra dimensions, if
any

## Details

The list of variables must follow the following rules. Each element of
the list must itself be one of:

1\) a data frame with a `value_column` column (see option
'value_column') and any number of other columns indicating one or more
dimensions

2\) a numeric vector of length one, with no dimensions

The name of the list elements itself is used to create the corresponding
variable in the NetCDF file.

## Examples

``` r
filename <- tempfile(pattern = "dummy", fileext = ".nc")
a <- 3
b <- data.frame(
  dim_a = rep(1:3, time = 2), dim_b = rep(1:2, each = 3), value = 1:6
)
variables <- list(a = a, b = b)
bi_write(filename, variables)
#> $time_dim
#> NULL
#> 
#> $coord_dims
#> $coord_dims$b
#> [1] "dim_a" "dim_b"
#> 
#> 
#> $dims
#> list()
#> 
bi_file_summary(filename)
#> File /tmp/RtmpYq3WON/dummy3a385d3e85e6.nc (NC_FORMAT_CLASSIC):
#> 
#>      2 variables (excluding dimension variables):
#>         float a[]   
#>         float b[dim_a,dim_b]   
#> 
#>      2 dimensions:
#>         dim_a  Size:3 
#>             long_name: dim_a
#>         dim_b  Size:2 
#>             long_name: dim_b
```
