# Write results of a `LibBi` run to an RDS file

This saves all options, files and outputs of a `LibBi` run to an RDS
file specified

## Usage

``` r
# S3 method for class 'libbi'
save_libbi(x, name, supplement, split = FALSE, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- name:

  name of the RDS file(s) to save to. If `split=TRUE`, this will be
  taken as a base for the names of the files to be created, e.g.
  'dir/name.rds' to create files of the form name\_....rds in directory
  'dir'.

- supplement:

  any supplementary data to save

- split:

  Logical, defaults to `FALSE`. Should the objects from the `LibBi` run
  be saved separately in a folder.

- ...:

  any options to [`saveRDS`](https://rdrr.io/r/base/readRDS.html)

## Value

the return value of `saveRDS`, i.e. NULL invisibly
