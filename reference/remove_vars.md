# Remove variables

Removes variables from the left-hand side of a model

Used by [`fix`](https://sbfnk.github.io/rbi/reference/fix.md) and
[`to_input`](https://sbfnk.github.io/rbi/reference/to_input.md)

## Usage

``` r
remove_vars(x, vars)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- vars:

  vector of variables to remove

## Value

a bi model object of the new model

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
