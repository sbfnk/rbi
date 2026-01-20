# Remove line(s) and/or block(s) in a libbi model

Removes one or more lines in a libbi model.

## Usage

``` r
# S3 method for class 'bi_model'
remove_lines(
  x,
  what,
  only,
  type = c("all", "assignment", "sample"),
  preserve_shell = FALSE,
  ...
)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- what:

  either a vector of line number(s) to remove, or a vector of blocks to
  remove (e.g., "parameter")

- only:

  only remove lines assigning given names (as a vector of character
  strings)

- type:

  which types of lines to remove, either "all", "sample" (i.e., lines
  with a "~") or "assignment" (lines with a "\<-" or "=") (default:
  "all")

- preserve_shell:

  if TRUE (default: FALSE), preserve the definition of a block even if
  all lines are removed; this is useful to preserve options passed to a
  `transition` or `ode` block

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
PZ <- remove_lines(PZ, 2)
```
