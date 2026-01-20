# Bi Model

`bi_model` creates a model object for `Rbi` from a libbi file, URL or
character vector. Once the instance is created, the model can be fed to
a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object.

## Usage

``` r
bi_model(filename, lines, ...)
```

## Arguments

- filename:

  the file name of the model file

- lines:

  lines of the model (if no `filename` is given), a character vector

- ...:

  ignored

## Value

a `{bi_model}` object containing the newly created model

## See also

[`fix`](https://sbfnk.github.io/rbi/reference/fix.md),
[`insert_lines`](https://sbfnk.github.io/rbi/reference/insert_lines.md),
[`remove_lines`](https://sbfnk.github.io/rbi/reference/remove_lines.md),
[`replace_all`](https://sbfnk.github.io/rbi/reference/replace_all.md),
[`get_name`](https://sbfnk.github.io/rbi/reference/get_name.md),
[`set_name`](https://sbfnk.github.io/rbi/reference/set_name.md),
[`write_model`](https://sbfnk.github.io/rbi/reference/write_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
```
