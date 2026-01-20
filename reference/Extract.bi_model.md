# Subset model lines

Extracts a subset of lines from the model.

## Usage

``` r
# S3 method for class 'bi_model'
x[i, ...]
```

## Arguments

- x:

  A bi_model

- i:

  A vector of line numbers

- ...:

  ignored

## Value

a character string of the extracted model lines(s)

## Examples

``` r
model_file_name <- system.file(package = "rbi", "PZ.bi")
PZ <- bi_model(filename = model_file_name)
PZ[3:4]
#> [1] "const e = 0.3"   "const m_l = 0.1"
```
