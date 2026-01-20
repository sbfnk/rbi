# Using the LibBi wrapper to launch LibBi

The method `run` launches `LibBi` with a particular set of command line
arguments. Normally, this function would not be run by the user, but
instead one of the client functions
[`sample`](https://sbfnk.github.io/rbi/reference/sample.md),
[`filter`](https://sbfnk.github.io/rbi/reference/filter.md), or
[`optimise`](https://sbfnk.github.io/rbi/reference/optimise.md), or
[`rewrite`](https://sbfnk.github.io/rbi/reference/rewrite.md), which
pass any options on to `run`. Note that any options specified here are
stored in the [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object and do not have to be specified again if another command is run
on the object.

## Usage

``` r
# S3 method for class 'libbi'
run(
  x,
  client,
  proposal = c("model", "prior"),
  model,
  fix,
  config,
  log_file_name = character(0),
  init,
  input,
  obs,
  time_dim = character(0),
  coord_dims = list(),
  thin,
  output_every,
  chain = TRUE,
  seed = TRUE,
  debug = FALSE,
  ...
)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object; if
  this is not given, an empty
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object will
  be created

- client:

  client to pass to LibBi

- proposal:

  proposal distribution to use; either "model" (default: proposal
  distribution in the model) or "prior" (propose from the prior
  distribution)

- model:

  either a character vector giving the path to a model file (typically
  ending in ".bi"), or a `bi_model` object; by default, will use any
  model given in `x`

- fix:

  any variable to fix, as a named vector

- config:

  path to a configuration file, containing multiple arguments

- log_file_name:

  path to a file to text file to report the output of `LibBi`; if set to
  an empty vector (`character(0)`) or an empty string (""), which is the
  default, a temporary log file will be generated

- init:

  initialisation of the model, either supplied as a list of values
  and/or data frames, or a (netcdf) file name, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object which
  has been run (in which case the output of that run is used). If the
  object given as `x` has been run before, it will be used here with
  `init-np` set to the last iteration of the previous run, unless `init`
  is given explicitly.

- input:

  input of the model, either supplied as a list of values and/or data
  frames, or a (netcdf) file name, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object which
  has been run (in which case the output of that run is used as input)

- obs:

  observations of the model, either supplied as a list of values and/or
  data frames, or a (netcdf) file name, or a
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object which
  has been run (in which case the output of that run is used as
  observations)

- time_dim:

  The time dimension in any R objects that have been passed (`init`,
  `input`) and `obs`); if NULL (default), will be guessed from the given
  observation

- coord_dims:

  The coord dimension(s) in any `obs` R objects that have been passed;
  if NULL (default), will be guessed from the given observation file
  given

- thin:

  any thinning of MCMC chains (1 means all will be kept, 2 skips every
  other sample etc.); note that `LibBi` itself will write all data to
  the disk. Only when the results are read in with
  [`bi_read`](https://sbfnk.github.io/rbi/reference/bi_read.md) will
  thinning be applied.

- output_every:

  real; if given, `noutputs` will be set so that there is output every
  `output_every` time steps; if set to 0, only generate an output at the
  final time

- chain:

  logical; if set to TRUE and `x` has been run before, the previous
  output file will be used as `init` file, and `init-np` will be set to
  the last iteration of the previous run (unless target=="prediction").
  This is useful for running inference chains.

- seed:

  Either a number (the seed to supply to `LibBi`), or a logical
  variable: TRUE if a seed is to be generated for `RBi`, FALSE if
  `LibBi` is to generate its own seed

- debug:

  logical; if TRUE, print more verbose messages and write all variables
  to the output file, irrespective of their setting of 'has_output'

- ...:

  list of additional arguments to pass to the call to `LibBi`. Any
  arguments starting with \`enable\`/\`disable\` can be specified as
  boolean (e.g., \`assert=TRUE\` or \`cuda=TRUE\`). Any \`dry-\` options
  can be specified with a \`"dry"\` argument, e.g., \`dry="parse"\`. Any
  options that would be specified with \`with\`/\`without\` can be
  specified as character vector to an option named \`with\`/\`without\`,
  respectively, e.g. with="transform-obs-to-state".

## Value

an updated [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object, except if `client` is 'rewrite', in which case invisible NULL
will be returned but the rewritten model code printed

## See also

[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
