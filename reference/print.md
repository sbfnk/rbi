# Print the lines of a LibBi model

Prints all lines in a LibBi model

This prints the model name, basic information such as number of
iterations and timesteps run, as well as a list of variables.

## Usage

``` r
# S3 method for class 'bi_model'
print(x, spaces = 2, screen = TRUE, ...)

# S3 method for class 'libbi'
print(x, verbose = FALSE, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- spaces:

  number of spaces for indentation

- screen:

  whether to print to screen (default: TRUE). In that case, line numbers
  will be added; otherwise, a character vector will be returned.

- ...:

  ignored

- verbose:

  logical; if TRUE, locations of files and working folder should be
  printed

## Value

if `screen` is `FALSE`, a character vector of model lines

nothing (invisible NULL)
