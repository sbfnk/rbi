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
  forbidden_names <-
    intersect(sapply(nc[["dim"]], function(x) {x[["name"]]}),
              time_coord_names)
  if (length(forbidden_names) > 0) {
    stop("Can't have a dimension called ", paste(forbiden_names, sep = ", "), ".")
  }
  var_names[["other"]] <- setdiff(all_var_names, unlist(var_names))

  if (!missing(vars)) {
    missing_vars <- setdiff(vars, var_names[["other"]])
    if (length(missing_vars) > 0) {
      if (all(vars %in% c(var_names[["coord"]], var_names[["time"]]))) {
        ## we're actually reading the time/coord vars
        var_names[["coord"]] <- character(0)
        var_names[["time"]] <- character(0)
        var_names[["other"]] <- vars
      } else {
        warning("Variable(s) ", missing_vars, " not found")
      }
    }
    var_names[["other"]] <- intersect(vars, var_names[["other"]])
  }

  ## read dimensions
  var_dims <- list()
  for (type in names(var_names)) {
    var_dims[[type]] <- list()
    for (var_name in var_names[[type]]) {
      var <- nc[["var"]][[var_name]]
      dim_names <- sapply(var$dim, function(x) { x$name })
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
      dim_names <- var_dims[["other"]][[var_name]]
      dim_lengths <- sapply(seq_along(dim_names), function(x)
      {
        nc[["var"]][[var_name]][["dim"]][[x]][["len"]]
      })
      names(dim_lengths) <- dim_names
      if ("np" %in% dim_names && !missing(thin) && thin > 1) {
        np_indices <- seq(1, dim_lengths["np"], by = thin)
        dim_lengths["np"] <- length(np_indices)

        all_values <- array(dim = rev(dim_lengths))

        if (!missing(verbose) && verbose) pb <- txtProgressBar(min = 0, max = length(np_indices), char = ".", style = 1)

        for (i in seq_along(np_indices))
        {
          dim_list <- lapply(rev(dim_lengths), seq_len)
          dim_list[["np"]] <- i
          start_vec <- dim_lengths
          start_vec[] <- 1
          start_vec["np"] <- np_indices[i]
          count_vec <- dim_lengths
          count_vec["np"] <- 1
          all_values <-
            do.call('[<-',
                    c(list(all_values),
                      unname(dim_list),
                      list(read_var_input(nc, var_name,
                                          start = start_vec,
                                          count = count_vec))))
          if (!missing(verbose) && verbose) setTxtProgressBar(pb, i)
        }
        if (!missing(verbose) && verbose) close(pb)
      } else {
        dim_names <- dim_names[dim_lengths > 1]
        all_values <- read_var_input(nc, var_name)
      }

      if (any(duplicated(dim_names))) {
        duplicated_dim_names <- dim_names[duplicated(dim_names)]
        for (dup_dim in duplicated_dim_names) {
          dim_names[dim_names == dup_dim] <-
            paste(dup_dim, seq_along(dim_names[dim_names == dup_dim]), sep = ".")
        }
      }

      value_dims <- dim(all_values)

      if (prod(value_dims) > 1) {
        mav <- data.table::data.table(reshape2::melt(all_values, varnames = rev(dim_names)))

        ## find matching time and coord variables
        all_matching_dims <- c()
        for (type in names(time_coord_names)) {
          matching_dims <- unlist(var_dims[[type]][unlist(var_dims[[type]]) %in% dim_names])
          matching_vars <- names(matching_dims)
          all_matching_dims <- union(all_matching_dims, matching_dims)
          if (length(matching_vars) == 1)  {
            merge_values <- read_var_input(nc, matching_vars)
            mav_merge <- data.table::data.table(reshape2::melt(merge_values, varnames = rev(matching_dims), value.name = time_coord_names[type]))
            mav <- merge(mav_merge, mav, by = unname(matching_dims))
          } else if (length(matching_vars) > 1) {
            stop("Found multiple matching time variables for ", var_name, ": ", matching_vars)
          }
        }

        for (var in all_matching_dims) {
          mav[[var]] <- NULL
        }
        table_order <- c(setdiff(colnames(mav), c(time_coord_names, "value")),
                         intersect(colnames(mav), time_coord_names),
                         "value")

        mav <- mav[, table_order, with = FALSE]

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
        } else if (!(col %in% c("value", time_coord_names["time"]))) {
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
