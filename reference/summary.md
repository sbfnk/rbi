# Print summary information about a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

This reads in the output file of the
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object (which
has been run before) and prints summary information of parameters

## Usage

``` r
# S3 method for class 'libbi'
summary(
  object,
  type = c("param", "state", "noise", "obs"),
  quantiles = c(0.25, 0.75),
  na.rm = FALSE,
  ...
)
```

## Arguments

- object:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- type:

  one of "param" (default), "state", "noise" or "obs", the variable type
  to summarise

- quantiles:

  quantiles to calculate (default: quartiles); minimum, median, mean and
  maximum are always calculated

- na.rm:

  logical; if true, any `na` and `NaN`'s are removed before calculations
  are performed

- ...:

  ignored

## Value

nothing (invisible NULL)
