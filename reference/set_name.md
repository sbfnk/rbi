# Set the name of a bi model

Changes the name of a bi model (first line of the .bi file) to the
specified name.

## Usage

``` r
# S3 method for class 'bi_model'
set_name(x, name, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- name:

  Name of the model

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
PZ <- set_name(PZ, "new_PZ")
```
