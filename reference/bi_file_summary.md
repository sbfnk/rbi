# NetCDF File Summary

This function prints a little summary of the content of a NetCDF file,
as well as its creation time. You can then retrieve variables of
interest using
[`bi_read`](https://sbfnk.github.io/rbi/reference/bi_read.md).

## Usage

``` r
bi_file_summary(...)
```

## Arguments

- ...:

  Any extra parameters to
  [`bi_open`](https://sbfnk.github.io/rbi/reference/bi_open.md),
  especially `x` and `file`

## Value

No return value

## Examples

``` r
example_output_file <- system.file(package = "rbi", "example_output.nc")
bi_file_summary(example_output_file)
#> File /home/runner/work/_temp/Library/rbi/example_output.nc (NC_FORMAT_NETCDF4):
#> 
#>      9 variables (excluding dimension variables):
#>         double time[nr]   (Contiguous storage)  
#>         double alpha[np,nr]   (Contiguous storage)  
#>         double P[np,nr]   (Contiguous storage)  
#>         double Z[np,nr]   (Contiguous storage)  
#>         double mu[np]   (Contiguous storage)  
#>         double sigma[np]   (Contiguous storage)  
#>         8 byte int clock[]   (Contiguous storage)  
#>         double loglikelihood[np]   (Contiguous storage)  
#>         double logprior[np]   (Contiguous storage)  
#> 
#>      2 dimensions:
#>         nr  Size:51 (no dimvar)
#>         np  Size:128 (no dimvar)
#> 
#>     3 global attributes:
#>         libbi_schema: MCMC
#>         libbi_schema_version: 1
#>         libbi_version: 1.3.0
```
