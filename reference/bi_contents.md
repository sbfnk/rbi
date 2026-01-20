# Bi contents

This function gets the name of all the variables in the passed file,
list or [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

## Usage

``` r
bi_contents(read, ...)
```

## Arguments

- read:

  either a path to a NetCDF file, or a NetCDF connection created using
  `nc_open`, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object from
  which to read the output

- ...:

  any parameters for
  [`bi_open`](https://sbfnk.github.io/rbi/reference/bi_open.md)
  (especially "file")

## Value

character vector of variable names

## Examples

``` r
example_output_file <- system.file(package = "rbi", "example_output.nc")
bi_contents(example_output_file)
#> [1] "time"          "alpha"         "P"             "Z"            
#> [5] "mu"            "sigma"         "clock"         "loglikelihood"
#> [9] "logprior"     
```
