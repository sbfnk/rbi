# Using the LibBi wrapper to optimise

The method `optimise` launches `libbi` to optimise the parameters with
respect to the likelihood or posterior distribution. See the options to
[`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md) for how to
specify the various components of sampling with LibBi, and the LibBi
manual for all options that can be passed when the client is `optimise`.

If `x` is given as a 'bi_model', a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object will be
created from the model For the help page of the base R `optimise`
function, see [`optimise`](https://rdrr.io/r/stats/optimize.html).

## Usage

``` r
# S3 method for class 'libbi'
optimise(x, ...)

# S3 method for class 'bi_model'
optimise(x, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) or
  `link{bi_model}` object, or the name of a file containing the model

- ...:

  options to be passed to
  [`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md)

## Value

an updated [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object
