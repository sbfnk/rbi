# Get the contents of a block in a LibBi model

Returns the contents of a block in a LibBi model as a character vector
of lines.

## Usage

``` r
# S3 method for class 'bi_model'
get_block(x, name, shell = FALSE, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- name:

  name of the block

- shell:

  if TRUE (default:FALSE), will return the shell (i.e., the definition
  of the block) as well as content; this is useful, e.g., to see options
  passed to a `transition` or `ode` block

- ...:

  ignored

## Value

a character vector of the lines in the block
