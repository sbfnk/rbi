# Find the libbi executable

This tries to find the libbi executable; if this does not find libbi but
it is installed, the location can be either passed to this function, or
set globally via \`options(path_to_libbi="/insert/full/path/here").

## Usage

``` r
locate_libbi(path_to_libbi)
```

## Arguments

- path_to_libbi:

  path to libbi, as either the path where the libbi executable resides,
  or the full path to the executable

## Value

full path to the libbi executable as character string; if it is not
found, an error is thrown

## Author

Sebastian Funk
