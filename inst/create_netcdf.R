library(bi)
filename <- tempfile(pattern="dummy", fileext=".nc")
a <- list(values = rnorm(3), dimension = "dim_a")
b <- list(values = 1:5, dimension = "dim_b")
c <- list(values = 5:9, dimension = "dim_b")
variables <- list(a=a, b=b, c=c)
netcdf_create_from_list(filename, variables)
bi_file_ncdump(filename)

