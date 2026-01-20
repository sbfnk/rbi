# Attach a new file or data set to a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

Adds an (output, obs, etc.) file to a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object. This
is useful to recreate a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object from
the model and output files of a previous run

The [`bi_write`](https://sbfnk.github.io/rbi/reference/bi_write.md)
options `append` and `overwrite` determine what exactly the file will
contain at the end of this. If they are both FALSE (the default), any
existing file will be ignored. If `append` is TRUE, the existing data in
the file will be preserved, and any data set passed as `data` and not
already in the file will be added. If `overwrite` is TRUE, existing data
in the file will be preserved except for variables that exist in the
passed `data`.

## Usage

``` r
# S3 method for class 'libbi'
attach_data(
  x,
  file,
  data,
  in_place = FALSE,
  append = FALSE,
  overwrite = FALSE,
  quiet = FALSE,
  time_dim = character(0),
  coord_dims = list(),
  ...
)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- file:

  the type of the file to attach, one of "output", "obs", "input" or
  "init"

- data:

  name of the file to attach, or a list of data frames that contain the
  outputs; it will be assumed that this is already thinned

- in_place:

  if TRUE, replace the file in place if it already exists in the libbi
  object; this can speed up the operation if append=TRUE as otherwise
  the file will have to be read and used again; it should be used with
  care, though, as it can render existing
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) objects
  invalid as the files they are pointing to are changed.

- append:

  if TRUE, will append variables if file exists; default: FALSE

- overwrite:

  if TRUE, will overwrite variables if file exists; default: FALSE

- quiet:

  if TRUE, will suppress the warning message normally given if
  replace=TRUE and the file exists already

- time_dim:

  the name of the time dimension, if one exists; default: "time"

- coord_dims:

  the names of the coordinate dimension, if any; should be a named list
  of character vectors, they are matched to variables names

- ...:

  any options to
  [`bi_write`](https://sbfnk.github.io/rbi/reference/bi_write.md) (e.g.,
  'time_dim')

## Value

an updated [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
object

## Examples

``` r
bi <- libbi(model = system.file(package = "rbi", "PZ.bi"))
example_output <- bi_read(system.file(package = "rbi", "example_output.nc"))
bi <- attach_data(bi, "output", example_output)
```
