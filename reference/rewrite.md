# Using the LibBi wrapper to rewrite

The method `rewrite` launches `LibBi` to rewrite a model to inspect its
internal representation in `LibBi`

If `x` is given as a 'bi_model', a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object will be
created from the model

## Usage

``` r
# S3 method for class 'libbi'
rewrite(x, ...)

# S3 method for class 'bi_model'
rewrite(x, ...)
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

a re-written
[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md) object
