# Get the parameter traces

This function takes the provided
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object which
has been run and returns a data frame with the parameter traces.

## Usage

``` r
get_traces(x, model, burnin, all = FALSE, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object
  which has been run, or a list of data frames containing parameter
  traces (as returned by `bi_read`); if it is not a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object,
  either 'all' must be TRUE or a model given

- model:

  a model to get the parameter names from; not needed if 'run' is given
  as a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object
  or 'all' is set to TRUE

- burnin:

  proportion of iterations to discard as burn-in (if between 0 and 1),
  or number of samples to discard (if \>1)

- all:

  whether all variables in the run file should be considered (otherwise,
  just parameters)

- ...:

  parameters to `bi_read` (e.g., dimensions)

## Value

a ata frame with parameter traces; this can be fed to `coda` routines
