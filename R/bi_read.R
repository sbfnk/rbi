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
#' @param time_name name of time dimension (if any)
#' @param coord_name name of coord dimension (if any)
#' @param vector if TRUE, will return results as vectors, not data.frames.
#' @param thin thinning (keep only 1/thin of samples)
#' @return list of results
#' @importFrom reshape2 melt
#' @importFrom ncdf4 nc_close
#' @importFrom data.table data.table setkeyv setDF is.data.table merge
#' @export
bi_read <- function(read, vars, dims, missval.threshold, time_name, coord_name, vector, thin, verbose)
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

  all_var_names <- unname(sapply(nc[["var"]], function(x) { x[["name"]] }))

  time_coord_names <- c()
  var_names <- list()
  ## special variables
  arg_names <- names(as.list(match.call()[-1]))
  for (type in c("coord", "time")) {
    time_coord_names[type] <- ifelse(paste(type, "name", sep = "_") %in% arg_names, get(paste(type, "name", sep = "_")), type)
    var_names[[type]] <- grep(paste0("^", type), all_var_names, value = TRUE)
  }
  var_names[["other"]] <- setdiff(all_var_names, unlist(var_names))

  if (!missing(vars)) {
    missing_vars <- setdiff(vars, var_names[["other"]])
    if (length(missing_vars) > 0) {
      warning("Variable(s) ", missing_vars, " not found")
    }
  }

  ## read dimensions
  var_dims <- list()
  for (type in names(var_names)) {
    var_dims[[type]] <- list()
    for (var_name in var_names[[type]]) {
      var <- nc[["var"]][[var_name]]
      dim_names <- sapply(var$dim, function(x) {
        ifelse(x$len > 1, x$name, "") # ncvar_get ignores dimensions of length 1
      })

      var_dims[[type]][[var_name]] <- dim_names[nchar(dim_names) > 0]
    }
  }

  ## associate time and coord variables
  ## read variables
  for (var_name in var_names[["other"]]) {
    if (!missing(verbose) && verbose)
    {
      message(date(), " ", var_name)
    }
    if (missing(vars) || var_name %in% vars) {
      all_values <- read_var_input(nc, var_name)
      dim_names <- var_dims[["other"]][[var_name]]

      if (any(duplicated(dim_names))) {
        duplicated_dim_names <- dim_names[duplicated(dim_names)]
        for (dup_dim in duplicated_dim_names) {
          dim_names[dim_names == dup_dim] <-
            paste(dup_dim, seq_along(dim_names[dim_names == dup_dim]), sep = ".")
        }
      }

      value_dims <- dim(all_values)

      if (prod(value_dims) > 1) {
        ## thinning
        if (!missing(thin) && "np" %in% dim_names) {
          ## more than just one value
          if (!missing(thin) && "np" %in% dim_names) {
            if (thin <= tail(value_dims, 1)) {
              indices <- lapply(value_dims[-length(value_dims)], seq_len)
              indices <-
                c(indices, list(seq(thin, tail(value_dims, 1), thin)))
              all_values <- do.call("[", c(list(all_values), indices, list(drop = FALSE)))
            } else {
              stop("Thinning interval too large.")
            }
          }
        }

        mav <- data.table::data.table(reshape2::melt(all_values, varnames = rev(dim_names)))
        if (!missing(thin)) mav[["np"]] <- mav[["np"]] * thin

        ## find matching and coord variables
        all_matching_dims <- c()
        for (type in names(time_coord_names)) {
          matching_dims <- unlist(var_dims[[type]][unlist(var_dims[[type]]) %in% dim_names])
          matching_vars <- names(matching_dims)
          all_matching_dims <- union(all_matching_dims,  matching_dims)
          if (length(matching_vars) == 1)  {
            merge_values <- read_var_input(nc, matching_vars)
            mav_merge <- data.table::data.table(reshape2::melt(merge_values, varnames = rev(matching_dims), value.name = time_coord_names[type]))
            mav <- merge(mav_merge, mav, by = unname(matching_dims))
          } else if (length(matching_vars) > 1) {
            stop("Found multiple matching time variables for ", var_name, ": ", matching_vars)
          }
        }

        for (var in all_matching_dims) mav[[var]] <- NULL

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

      if (!missing(dims) && length(dims) == 1 && "coord" %in% colnames(mav)) setnames(mav, "coord", names(dims))

      for (col in colnames(mav)) {
        if (!missing(dims) && col %in% names(dims)) {
          mav[[col]] <- factor(mav[[col]], labels = dims[[col]])
        } else if (!(col %in% c(time_coord_names, "value"))) {
          mav[[col]] <- mav[[col]] - 1
        }
      }

      if (!missing(vector) && vector) {
        res[[var_name]] <- mav$value
      } else {
        if (data.table::is.data.table(mav)) {
          res[[var_name]] <- data.table::setDF(mav)
        } else {
          res[[var_name]] <- mav
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
