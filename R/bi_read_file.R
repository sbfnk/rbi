#' @rdname bi_read_file
#' @name bi_read_file
#' @title Bi Read File
#' @description
#' This function reads all variable from a NetCDF file.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#' 
#' @param file either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}
#' @param dims factors for dimensions
#' @param missval.threshold upper threshold for the likelihood
#' @param variables only extract given variables (for space saving)
#' @param time_dim name of time dimension (if any)
#' @param quiet suppress progress bar
#' @return list of results
#' @export
bi_read_file <- function(file, dims, missval.threshold, variables, time_dim)
{
  res <- list()

  if (typeof(file) == "character"){
    nc <- nc_open(tools::file_path_as_absolute(file))
  } else {
    nc <- file
  }

  var_names <- unname(sapply(nc[["var"]], function(x) { x[["name"]] }))

  time_var_names <- var_names[grep("^time", var_names)]
  var_names <- var_names[!(var_names %in% time_var_names)]

  ## read time variables
  
  time_vars <- list()
  for (var_name in time_var_names) {
    var <- nc[["var"]][[var_name]]
    time_values <- read_var_input(nc, var_name)
    dim_names <- sapply(var$dim, function(x) {
      ifelse(x$len > 1, x$name, "") # ncvar_get ignores dimensions of length 1
    })

    time_vars[[dim_names[1]]] <- time_values
  }

  ## guess time dimension if not given
  if (missing(time_dim)) {
    if (any(grep("_", names(time_vars)))) {
      prefixes <- sub("_.*$", "", names(time_vars))
      if (length(unique(prefixes)) == 1) {
        time_dim <- prefixes[1]
      } else {
        time_dim <- NULL
      }
    } else {
      time_dim <- NULL
    }
  }

  ## read other variables
  
  for (var_name in var_names) {
    var <- nc[["var"]][[var_name]]
    if (missing(variables) || var$name %in% variables) {
      all_values <- read_var_input(nc, var$name)
      
      dim_names <- sapply(var$dim, function(x) {
        ifelse(x$len > 1, x$name, "") # ncvar_get ignores dimensions of length 1
      })
      dim_names <- dim_names[nchar(dim_names) > 0]

      if (any(duplicated(dim_names))) {
        duplicated_dim_names <- dim_names[duplicated(dim_names)]
        for (dup_dim in duplicated_dim_names) {
          dim_names[dim_names == dup_dim] <-
            paste(dup_dim, seq_along(dim_names[dim_names == dup_dim]), sep = ".")
        }
      } else {
        duplicated_dim_names <- c()
      }
      
      if (prod(dim(all_values)) > 1) {
        ## more than just one value
        mav <- data.frame(melt(all_values, varnames = rev(dim_names)))
        ## reorder duplicates
        cols <- setdiff(colnames(mav), "value")
        for (dup_dim in duplicated_dim_names) {
          dup_cols <- grep(paste0(dup_dim, "\\.[0-9]+$"), cols)
          cols[dup_cols] <- rev(cols[dup_cols])
        }
        mav <- mav[do.call(order, mav[rev(cols)]), ]
        mav <- mav[, c(cols, "value")]
        rownames(mav) <- seq_len(nrow(mav))
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
        } else if (current.dim %in% names(time_vars)) {
          mav[[current.dim]] <- time_vars[[current.dim]]
          if (!is.null(time_dim)) {
            names(mav)[which(names(mav) == current.dim)] <- time_dim
          }
        } else {
          mav[[current.dim]] <- mav[[current.dim]] - 1
        }
      }

      res[[var$name]] <- mav
    }
  }

  if (typeof(file) == "character") nc_close(nc)

  return(res)
}
