# Get constants in a LibBi model

Get constants contained in a LibBi model and their values. This will
attempt to evaluate any calculation on the right hand side. Failing
that, it will be returned verbatim.

## Usage

``` r
get_const(model)
```

## Arguments

- model:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

## Value

a list of constants (as names) and their values
