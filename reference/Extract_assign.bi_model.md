# Subset and replace model lines

Extracts a subset of lines from the model and assigns new character
strings.

## Usage

``` r
# S3 method for class 'bi_model'
x[i, ...] <- value
```

## Arguments

- x:

  A bi_model

- i:

  A vector of line numbers

- ...:

  ignored

- value:

  A vector of the same length as `i`, containing the replacement strings

## Value

the updated `bi_model` object

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ[3:4] <- c("const e = 0.4", "const m_l = 0.05")
```
