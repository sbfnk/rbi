# Generate Dataset

This is a wrapper around `libbi sample --target joint --nsamples 1`, to
generate a synthetic dataset from a model. Parameters can be passed via
the 'init' option (see
[`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md), otherwise
they are generated from the prior specified in the model. The end time
should be specified using the "end_time" option. If this is not given,
only a parameter set is sampled. Use the 'noutputs' or 'output_every'
options to control the number of data points being generated. By
default, output_every is set to 1.

## Usage

``` r
generate_dataset(..., output_every = 1)
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
