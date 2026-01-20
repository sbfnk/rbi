# Replace all instances of a string with another in a model

Takes every occurrence of one string and replaces it with another

## Usage

``` r
# S3 method for class 'bi_model'
replace_all(x, from, to, ...)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- from:

  string to be replaced (a regular expression)

- to:

  new string (which can refer to the regular expression given as `from`)

- ...:

  ignored

## Value

the updated `bi_model` object

## See also

[`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
