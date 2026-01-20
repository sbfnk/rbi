# Convert variables into inputs

Used for predictions, if one doesn't want to re-simulate state/noise
trajectories

## Usage

``` r
to_input(x, vars)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- vars:

  vector of variables to convert to inputs

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
