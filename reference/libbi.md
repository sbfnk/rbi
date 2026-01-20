# LibBi Wrapper

`libbi` allows to call `LibBi`. Upon creating a new libbi object, the
following arguments can be given. Once the instance is created, `LibBi`
can be run through the
[`sample`](https://sbfnk.github.io/rbi/reference/sample.md),
[`filter`](https://sbfnk.github.io/rbi/reference/filter.md), or
[`optimise`](https://sbfnk.github.io/rbi/reference/optimise.md), or
[`rewrite`](https://sbfnk.github.io/rbi/reference/rewrite.md) methods.
Note that `libbi` objects can be plotted using
[`plot`](https://rdrr.io/r/graphics/plot.default.html) if the
`rbi.helpers` package is loaded.

## Usage

``` r
libbi(model, path_to_libbi, dims, use_cache = TRUE, ...)
```

## Arguments

- model:

  either a character vector giving the path to a model file (typically
  ending in ".bi"), or a `bi_model` object

- path_to_libbi:

  path to `LibBi` binary; by default it tries to locate the `libbi`
  binary using the `which` Unix command, after having loaded "~/.bashrc"
  if present; if unsuccessful it tries "~/PathToBiBin/libbi"; if
  unsuccessful again it fails.

- dims:

  any named dimensions, as list of character vectors

- use_cache:

  logical; whether to use the cache (default: true)

- ...:

  options passed to
  [`run.libbi`](https://sbfnk.github.io/rbi/reference/run.md)

## Value

a new `libbi` object

## See also

[`sample`](https://sbfnk.github.io/rbi/reference/sample.md),
[`filter`](https://sbfnk.github.io/rbi/reference/filter.md),
[`optimise`](https://sbfnk.github.io/rbi/reference/optimise.md),
[`rewrite`](https://sbfnk.github.io/rbi/reference/rewrite.md)

## Examples

``` r
bi_object <- libbi(model = system.file(package = "rbi", "PZ.bi"))
```
