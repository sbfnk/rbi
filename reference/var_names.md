# Get variable names in a LibBi model

Get variable names of one or more type(s)

This returns all variable names of a certain type ("param", "state",
"obs", "noise", "const") contained in the model of a
[`libbi`](https://sbfnk.github.io/rbi/reference/libbi.md) object

## Usage

``` r
var_names(x, vars, type, dim = FALSE, opt = FALSE, aux = FALSE)
```

## Arguments

- x:

  a [`bi_model`](https://sbfnk.github.io/rbi/reference/bi_model.md)
  object

- vars:

  a character vector of variable names; if given, only these variables
  names will be considered

- type:

  a character vector of one or more types

- dim:

  logical; if set to TRUE, names will contain dimensions in brackets

- opt:

  logical; if set to TRUE, names will contain options (e.g., has_output)

- aux:

  logical; if set to TRUE, auxiliary names will be returned

## Value

a character vector ofvariable names
