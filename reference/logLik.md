# Using the LibBi wrapper to logLik

The method `logLik` extracts the log-likelihood of a `libbi` object.
This can be done, for example, after a call to
[`sample`](https://sbfnk.github.io/rbi/reference/sample.md) to inspect
the chain log-likelihoods.

For the help page of the base R `logLik` function, see
[`logLik`](https://rdrr.io/r/stats/logLik.html).

## Usage

``` r
# S3 method for class 'libbi'
logLik(object, ...)
```

## Arguments

- object:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- ...:

  options to be passed to
  [`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md)

## Value

a vector of log-likelihood
