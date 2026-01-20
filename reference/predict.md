# Using the LibBi wrapper to predict

The method `predict` is an alias for `sample(target="prediction")`.
Usually, an `init` object or file should be given containing posterior
samples.

For the help page of the base R `optimise` function, see
[`optimise`](https://rdrr.io/r/stats/optimize.html).

## Usage

``` r
# S3 method for class 'libbi'
predict(x, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- ...:

  any arguments to be passed to
  [`sample`](https://sbfnk.github.io/rbi/reference/sample.md)

## Value

an updated [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object
