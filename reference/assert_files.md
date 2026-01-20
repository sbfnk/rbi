# Check that a LibBi wrapper has valid output

This checks that the
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object given
has been run (via
[`sample`](https://sbfnk.github.io/rbi/reference/sample.md),
[`filter`](https://sbfnk.github.io/rbi/reference/filter.md) or
[`optimize`](https://rdrr.io/r/stats/optimize.html))) and the output
file has not been modified since.

## Usage

``` r
# S3 method for class 'libbi'
assert_files(x, ...)
```

## Arguments

- x:

  a [`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

- ...:

  ignored

## Value

nothing (invisible NULL); an error will be thrown if there is a problem
