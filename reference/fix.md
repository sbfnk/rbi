# Fix noise term, state or parameter of a libbi model

Replaces all variables with fixed values as given ; note that this will
not replace differential equations and lead to an error if applied to
states that are changed inside an "ode" block

For the help page of the base R `fix` function, see
[`fix`](https://rdrr.io/r/utils/fix.html).

## Usage

``` r
# S3 method for class 'bi_model'
fix(x, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- ...:

  values to be assigned to the (named) variables

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ <- fix(PZ, alpha = 0)
```
