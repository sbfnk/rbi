# Get the name of a bi model

Extracts the name of a bi model (first line of the .bi file).

## Usage

``` r
# S3 method for class 'bi_model'
get_name(x, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- ...:

  ignored

## Value

a character string, the name of the model

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
get_name(PZ)
#> [1] "PZ"
```
