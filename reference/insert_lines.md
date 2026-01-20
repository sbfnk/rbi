# Insert lines in a LibBi model

Inserts one or more lines into a libbi model. If one of `before` or
`after` is given, the line(s) will be inserted before or after a given
line number or block name, respectively. If one of `at_beginning of` or
`at_end_of` is given, the lines will be inserted at the beginning/end of
the block, respectively.

## Usage

``` r
# S3 method for class 'bi_model'
insert_lines(x, lines, before, after, at_beginning_of, at_end_of, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- lines:

  vector or line(s)

- before:

  line number before which to insert line(s)

- after:

  line number after which to insert line(s)

- at_beginning_of:

  block at the beginning of which to insert lines(s)

- at_end_of:

  block at the end of which to insert lines(s)

- ...:

  ignored

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ <- insert_lines(PZ, lines = "noise beta", after = 8)
```
