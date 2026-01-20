# Enable outputting variables in a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

Any variable type given will have any 'has_output=0' option removed in
the given model.

## Usage

``` r
enable_outputs(x, type = "all")
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- type:

  either "all" (default), or a vector of variable types that are to have
  outputs enabled

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ[6] <- "param mu (has_output=0)"
PZ <- enable_outputs(PZ)
```
