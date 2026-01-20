# Writes a bi model to a file.

Writes a bi model to a file given by `filename`. The extension '.bi'
will be added if necessary.

## Usage

``` r
# S3 method for class 'bi_model'
write_model(x, filename, update.name = TRUE, ...)

# S3 method for class 'libbi'
write_model(x, filename, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object, or a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md)
  object containing a model

- filename:

  name of the file to be written

- update.name:

  whether to update the model name with the file name

- ...:

  ignored

## Value

the return value of the
[`writeLines`](https://rdrr.io/r/base/writeLines.html) call.

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
new_file_name <- tempfile("PZ", fileext = ".bi")
write_model(PZ, new_file_name)
```
