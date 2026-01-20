# Bi Open

This function opens an NetCDF file The file can be specified as a string
to the filepath, in which case a NetCDF connection is opened, or
directly as a NetCDF connection.

## Usage

``` r
bi_open(x, file = "output")
```

## Arguments

- x:

  either a path to a NetCDF file, or a NetCDF connection created using
  `nc_open`, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object from
  which to read the output

- file:

  file to open (out of "input", "init", "obs", "output"), if `x` is
  given as a `libbi` object; by default, will read output file

## Value

an open NetCDF connection
