# Check if a variable is sparse

Check if a variable is sparse

## Usage

``` r
check_sparse_var(x, coord_cols, value_column)
```

## Arguments

- x:

  data.table

- coord_cols:

  character vector of coordinate columns

- value_column:

  the name of the value column

## Value

TRUE if the variable is sparse, FALSE otherwise

## Details

Takes a data.table with given coordinate columns and a value column and
checks if all combinations of the coordinate columns are present for
each combination of the other columns.

## Author

Sebastian Funk
