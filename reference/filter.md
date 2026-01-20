# Using the LibBi wrapper to filter

The method `filter` launches `libbi` to filter state trajectories. See
the options to
[`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md) for how to
specify the various components of sampling with LibBi, and the LibBi
manual for all options that can be passed when the client is `filter`.

If `x` is given as a 'bi_model', a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object will be
created from the model For the help page of the base R `filter`
function, see [`filter`](https://rdrr.io/r/stats/filter.html).

## Usage

``` r
# S3 method for class 'libbi'
filter(x, ...)

# S3 method for class 'bi_model'
filter(x, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) or
  [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object, or the name of a file containing the model

- ...:

  options to be passed to
  [`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md)

## Value

an updated [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object
