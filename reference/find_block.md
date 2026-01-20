# Find a block in a LibBi model

Finds a block and returns the range of line numbers encompassed by that
block.

## Usage

``` r
# S3 method for class 'bi_model'
find_block(x, name, inner = FALSE, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- name:

  of the block to find

- inner:

  only return the inner part of the block (not the block definition)

- ...:

  ignored

## Value

an integerr vector, the range of line numbers

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
