# Propose from the prior in a libbi model

Generates a version of the model where the proposal blocks are replaced
by the prior blocks. This is useful for exploration of the likelihood
surface.

## Usage

``` r
propose_prior(x)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
