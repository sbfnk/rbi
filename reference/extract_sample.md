# Extract a sample from a `LibBi` run.

This function takes the provided
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) results and
extracts a data frame.

## Usage

``` r
extract_sample(x, np, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object
  which has been run, or a list of data frames containing parameter
  traces (as returned by from `bi_read`)

- np:

  iteration to extract; if set to "last", the last sample will be
  extracted. If not given a random sample will be extracted

- ...:

  parameters to `bi_read` (e.g., dimensions)

## Value

a list of data frames or numeric vectors containing parameters and
trajectories
