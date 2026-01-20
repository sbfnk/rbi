# Check if two models are unequal

Ignores differences in the model name.

## Usage

``` r
# S3 method for class 'bi_model'
`!=`(e1, e2, ...)
```

## Arguments

- e1:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

- e2:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

- ...:

  ignored

## Value

TRUE or FALSE, depending on whether the models are equal or not

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ != PZ # FALSE
#> [1] FALSE
```
