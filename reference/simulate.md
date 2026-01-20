# Using the LibBi wrapper to simulate

The method `simulate` launches `LibBi` to simulate a model by passing
\`target="joint"\` to `LibBi`

If `x` is given as a 'bi_model', a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object will be
created from the model

## Usage

``` r
# S3 method for class 'libbi'
simulate(x, ...)

# S3 method for class 'bi_model'
simulate(x, ...)
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

an updated
[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md) object
