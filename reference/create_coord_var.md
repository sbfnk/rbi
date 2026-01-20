# Create a coordinate variable

Create a coordinate variable

## Usage

``` r
create_coord_var(
  name,
  dims,
  dim_factors,
  coord_dim,
  index_table,
  ns_dim,
  time_dim,
  nr_column,
  value_column
)
```

## Arguments

- name:

  Name of the variable

- dims:

  Dimensions of all variables

- dim_factors:

  Factors of all dimensions

- coord_dim:

  Coordinate dimension

- index_table:

  data.table with index columns

- ns_dim:

  ns dimension

- time_dim:

  time dimension

- nr_column:

  nr column

- value_column:

  value column

## Value

a list with information on the coordinate variable

## Details

Creates a coordinate variable with associated dimensions

## Author

Sebastian Funk
