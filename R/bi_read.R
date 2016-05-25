#' @rdname bi_read
#' @name bi_read
#' @title Bi Read
#' @description
#' This function reads all variable from a NetCDF file or the output of a
#' \code{\link{libbi}} object.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#'
#' @param read either a path to a NetCDF file, or a NetCDF connection created using \code{nc_open}, or a \code{\link{libbi}} object from which to read the output
#' @param vars variables to read; if not given, all will be read
#' @param dims factors for dimensions
#' @param missval.threshold upper threshold for the likelihood
#' @param variables only extract given variables (for space saving)
#' @param time_dim name of time dimension (if any)
#' @param vector if TRUE, will return results as vectors, not data.frames.
#' @param thin thinning (keep only 1/thin of samples)
#' @return list of results
#' @importFrom reshape2 melt
#' @importFrom ncdf4 nc_close
#' @importFrom data.table data.table setkeyv setDF is.data.table
#' @export
bi_read <- function(read, vars, dims, missval.threshold, variables, time_dim, vector, thin, verbose)
{

  nc <- bi_open(read)
  res <- list()

  if ("libbi" %in% class(read) && !is.null(read$dims)) {
    if (missing(dims)) {
      dims <- read$dims
    } else {
      warning("Given 'dims' will overwrite dimensions in passed libbi object")
    }
  }

  var_names <- unname(sapply(nc[["var"]], function(x) { x[["name"]] }))

  time_var_names <- var_names[grep("^time", var_names)]
  var_names <- var_names[!(var_names %in% time_var_names)]
  if (!missing(vars)) {
    missing_vars <- setdiff(vars, union(var_names, time_var_names))
    if (length(missing_vars) > 0) {
      warning("Variable(s) ", missing_vars, " not found")
    }
    var_names <- intersect(union(var_names, time_var_names), vars)
  }

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
    if (!missing(verbose) && verbose)
    {
      message(var_name)
    }
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
      }

      if (prod(dim(all_values)) > 1) {
        ## more than just one value
        mav <- data.table::data.table(reshape2::melt(all_values, varnames = rev(dim_names)))
        ## reorder duplicates
        cols <- setdiff(colnames(mav), "value")
        data.table::setkeyv(mav, cols)
        ## mav <- mav[do.call(order, mav[cols]), ]
        rownames(mav) <- seq_len(nrow(mav))
      } else {
        ## fixed value
        mav <- all_values
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
          mav[[current.dim]] <- time_vars[[current.dim]][mav[[current.dim]]]
          if (!is.null(time_dim)) {
            names(mav)[which(names(mav) == current.dim)] <- time_dim
          }
        } else {
          mav[[current.dim]] <- mav[[current.dim]] - 1
        }
      }

      if (!missing(thin) && "np" %in% names(mav))
      {
          mav <- mav[mav$np %% thin == 0, ]
      }

      if (!missing(vector) && vector) {
        res[[var$name]] <- mav$value
      } else {
        if (data.table::is.data.table(mav)) {
          res[[var$name]] <- data.table::setDF(mav)
        } else {
          res[[var$name]] <- mav
        }
      }
    }
  }

    if (typeof(file) == "character") nc_close(nc)

    ## if only one variable has been requested, return data frame
    if (!missing(vars) && length(vars) == 1 && length(res) > 0) {
      res <- res[[1]]
    }

  return(res)
}
