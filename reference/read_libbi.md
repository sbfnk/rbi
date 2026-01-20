# Read results of a `LibBi` run from an RDS file or from a folder. This completely reconstructs the saved `LibBi` object

This reads all options, files and outputs of a `LibBi` run from a
specified RDS file or folder (if `split = TRUE` has been used with
`save_libbi`).

## Usage

``` r
read_libbi(name, ...)
```

## Arguments

- name:

  name of the RDS file(s) to read

- ...:

  any extra options to pass to
  [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) when
  creating the new object

## Value

a new [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object
