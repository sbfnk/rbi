#' @rdname bi_read_file
#' @name bi_read_file
#' @title Bi Read File
#' @description
#' This function reads all variable from a NetCDF file.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#' 
#' @param file either a path to a NetCDF file, or a NetCDF connection created using open.ncdf(filename)
#' @param dims factors for dimensions
#' @param missval.threshold upper threshold for the likelihood
#' @param variables only extract given variables (for space saving)
#' @param quiet suppress progress bar
#' @param flat flatten results
#' @return list of results (or a data.frame, if \code{flat} has been set to TRUE)
#' @export
bi_read_file <- function(file, dims, missval.threshold, variables)
{
  res <- list()

  if (typeof(file) == "character"){
    nc <- open.ncdf(tools::file_path_as_absolute(file))
  } else {
    nc <- resultfile
  }
  
  for (var in nc$var) {
    if (missing(variables) || var$name %in% variables) {
      all_values <- read_var_input(nc, var$name)
      
      dim_names <- sapply(var$dim, function(x) {
        ifelse(x$len > 1, x$name, "") # ncvar_get ignores dimensions of length 1
      })
      dim_names <- dim_names[nchar(dim_names) > 0]

      if (any(duplicated(dim_names))) {
        for (dup_dim in dim_names[duplicated(dim_names)]) {
          dim_names[dim_names == dup_dim] <-
            paste(dup_dim, seq_along(dim_names[dim_names == dup_dim]), sep = ".")
        }
      }
      
      if (prod(dim(all_values)) > 1) {
        ## more than just one value
        mav <- data.frame(melt(all_values, varnames = c(dim_names)))
      } else {
        ## fixed value
        mav <- data.frame(value = all_values)
      }

      if (!missing(missval.threshold)) {
        missing.values <- which(mav$value > missval.threshold)
        if (length(missing.values) > 0) {
          mav[missing.values, ]$value <- NA
        }
      }

      for (current.dim in dim_names) {
        if (!missing(dims) && current.dim %in% names(dims)) {
          mav[[current.dim]] <-
            factor(mav[[current.dim]], labels = dims[[current.dim]])
        } else {
          mav[[current.dim]] <- mav[[current.dim]] - 1
        }
      }

      res[[var$name]] <- mav
    }
  }

  if (typeof(file) == "character") close.ncdf(nc)

  return(res)
}
