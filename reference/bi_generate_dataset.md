# Bi Generate Dataset

This function is deprecated and has been renamed to
[`generate_dataset`](https://sbfnk.github.io/rbi/reference/generate_dataset.md)

## Usage

``` r
bi_generate_dataset(..., output_every = 1)
```

## Arguments

- ...:

  arguments to be passed to
  [`sample.libbi`](https://sbfnk.github.io/rbi/reference/sample.md),
  especially 'model', 'end_time' and 'seed'.

- output_every:

  real; if given, `noutputs` will be set so that there is output every
  `output_every` time steps; if set to 0, only generate an output at the
  final time

## Value

a `libbi` object, the generated data set
