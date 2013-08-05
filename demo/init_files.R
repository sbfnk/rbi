### This demo shows how to create init files

rm(list = ls(all.names=TRUE))
unlink(".RData")
try(detach(package:bi, unload = TRUE), silent = TRUE)
library(bi, quietly = TRUE)

init_file_name <- tempfile(pattern="init", fileext=".nc")
param0 <- list(A = c(0.1, 0.2, 0.3), B = 0.2)
try(bi_init_file(filename=init_file_name, variables=param0), TRUE)  
# does not work because all vectors in param0 should be of equal length

param1 <- list(A = c(0.1, 0.2, 0.3), B = c(0.2, 0.2, 0.2))
bi_init_file(filename=init_file_name, variables=param1)
