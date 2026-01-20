# Add a block to a LibBi model

Add a block to a LibBi model. If that block exists, it will be removed
first.

## Usage

``` r
# S3 method for class 'bi_model'
add_block(x, name, lines, options, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- name:

  name of the block

- lines:

  character vector, lines in the block

- options:

  any options to the block

- ...:

  ignored

## Value

a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md) object
containing the new block
