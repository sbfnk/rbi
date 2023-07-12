#' @rdname bi_read
#' @name bi_read
#' @title Bi Read
#' @description
#' This function reads all variable from a NetCDF file or the output of a
#' \code{\link{libbi}} object.
#' The file can be specified as a string to the filepath, in which
#' case a NetCDF connection is opened, or directly as a NetCDF connection.
#'
#' @param x either a path to a NetCDF file, or a NetCDF connection created using
#'   \code{nc_open}, or a \code{\link{libbi}} object from which to read the
#'   output
#' @param vars variables to read; if not given, all will be read
#' @param dims factors for dimensions
#' @param model model file or a \code{bi_model} object (if \code{x} is not a
#'   \code{libbi} object)
#' @param type vector of types of variable to read (out of "param", "state",
#'   "noise", "obs"). This needs 'x' to be a \code{\link{libbi}} object or
#'   \code{model} to be specified
#' @param file which file to read (if \code{x} is given as a \code{\link{libbi}}
#'   object): one of "output" (default), "init", "input", "obs"
#' @param missval_threshold upper threshold for the likelihood
#' @param coord_dims any \code{coord} dimensions, given as a named list of
#'   character vectors, where each element corresponds to the variable of the
#'   same name, and the character vector are the \code{coord} dimensions
#' @param vector deprecated; if TRUE, will return results as vectors, not
#'   data.frames
#' @param thin thinning (keep only 1/thin of samples)
#' @param verbose if TRUE, will print variables as they are read
#' @param clear_cache if TRUE, will clear the cache and re-read the file even if
#'   cached data exists
#' @param init_to_param logical; if TRUE, convert states to initial values
#' @param burn number of initial samples to discard; default: 0
#' @param missval.threshold deprecated; use missval_threshold instead
#' @param init.to.param deprecated; use init_to_param instead
#' @return a list of data frames and/or numbers that have been read
#' @importFrom ncdf4 nc_close ncvar_get
#' @importFrom data.table setkeyv setnames setDF is.data.table := dcast
#' @importFrom reshape2 melt
#' @examples
#' example_output_file <- system.file(package = "rbi", "example_output.nc")
#' d <- bi_read(example_output_file)
#' @export
bi_read <- function(x, vars, dims, model, type, file, missval_threshold,
                    coord_dims = list(), vector, thin, verbose = FALSE,
                    clear_cache = FALSE, init_to_param = FALSE, burn = 0,
                    missval.threshold, init.to.param = FALSE) { # nolint
  if (!missing(missval.threshold)) {
    warning("missval.threshold is deprecated. Use 'missval_threshold' instead.")
    if (missing(missval_threshold)) {
      missval_threshold <- missval.threshold
    } else {
      stop("Can't give 'missval.threshold' and 'missval_threshold'.")
    }
  } else if (missing(missval_threshold)) {
    missval_threshold <- NULL
  }

  if (!missing(init.to.param)) {
    warning("init.to.param is deprecated. Use 'init_to_param' instead.")
    if (missing(init_to_param)) {
      init_to_param <- init.to.param
    } else {
      stop("Can't give 'init.to.param' and 'init_to_param'.")
    }
  }

  if (!missing(vector) && vector) {
    warning("'vector' is deprecated. Will return data frame")
  }

  if (missing(file)) {
    nc <- bi_open(x)
  } else {
    nc <- bi_open(x, file)
  }
  res <- list()

  thin <- as.integer(ifelse(
    missing(thin), ifelse("libbi" %in% class(x), x$thin, 1), thin
  ))

  if ("libbi" %in% class(x) && !is.null(x$dims)) {
    if (missing(model)) {
      model <- x$model
    } else {
      stop("'model' should not be given if 'data' is a 'libbi' object'.")
    }
    if (missing(dims) || is.null(dims)) {
      dims <- x$dims
    } else {
      if (length(x$dims) > 0) {
        warning("Given 'dims' will override dimensions in passed libbi object")
        clear_cache <- TRUE ## clear if reading with different dimensions
      }
    }
    if (missing(coord_dims) || is.null(coord_dims)) {
      coord_dims <- x$coord_dims
    } else {
      if (length(x$coord_dims) > 0) {
        unequal <- vapply(names(x$coord_dims), function(y) {
          any(x$coord_dims[[y]] != coord_dims[[y]])
        }, FALSE)
        if (any(unequal)) {
          warning(
            "Given 'coord_dims' will override coord dimensions in passed ",
            "libbi object"
          )
        }
        clear_cache <- TRUE ## clear if reading with different dimensions
      }
    }

    for (coord_dim in names(coord_dims)) {
      if (!is.null(x$coord_dims[[coord_dim]]) &&
          any(x$coord_dims[[coord_dim]] != coord_dims[[coord_dim]])) {
        warning(
          "Given coord dimension ", coord_dim, " will override a coord ",
          "dimension of the same name in passed libbi object"
        )
        clear_cache <- TRUE ## clear if reading with different dimensions
      }
    }
  }

  all_nc_var_names <- unname(vapply(nc[["var"]], function(y) {
    y[["name"]]
  }, ""))

  nc_var_names <- list()
  ## special variables
  for (var_type in c("coord", "time")) {
    nc_var_names[[var_type]] <- grep(
      paste0("^", var_type), all_nc_var_names, value = TRUE
    )
  }
  nc_var_names[["other"]] <- setdiff(all_nc_var_names, unlist(nc_var_names))

  if (!missing(vars) && !missing(type)) {
    stop("Only one of 'vars' and 'type' can be given.")
  }

  if (!missing(type)) {
    vars <- var_names(model, type = type, opt = TRUE)
    ## remove vars that don't have an output
    vars <- grep("has_output[^=]*=[^[0-1]*0", vars, value = TRUE, invert = TRUE)
    ## remove any other options
    vars <- gsub("[[:space:]]*\\(.*$", "", vars)
  }

  if (!missing(vars)) {
    missing_vars <- setdiff(vars, nc_var_names[["other"]])
    if (length(missing_vars) > 0) {
      if (all(vars %in% c(nc_var_names[["coord"]], nc_var_names[["time"]]))) {
        ## we're actually reading the time/coord vars
        nc_var_names[["coord"]] <- character(0)
        nc_var_names[["time"]] <- character(0)
        nc_var_names[["other"]] <- vars
      }
    }
    nc_var_names[["other"]] <- intersect(vars, nc_var_names[["other"]])
  }

  ## read dimensions
  var_dims <- list()
  for (var_type in names(nc_var_names)) {
    var_dims[[var_type]] <- list()
    for (var_name in nc_var_names[[var_type]]) {
      var <- nc[["var"]][[var_name]]
      dim_names <- vapply(var$dim, function(y) {
        y$name
      }, "")
      var_dims[[var_type]][[var_name]] <- dim_names[nchar(dim_names) > 0]
    }
  }

  res <- base::vector("list", length(nc_var_names[["other"]]))
  names(res) <- nc_var_names[["other"]]

  ## read variables

  ## cache
  if ("libbi" %in% class(x) && x$use_cache &&
    (missing(file) || file == "output")) {
    if (clear_cache) {
      x$.cache$data <- NULL
      x$.cache$thin <- NULL
    }
    cached_other <- vapply(nc_var_names[["other"]], function(y) {
      return(y %in% names(x$.cache$data) && thin == x$.cache$thin[y])
    }, TRUE)
  } else {
    cached_other <- rep(FALSE, length(nc_var_names[["other"]]))
  }
  for (var_name in nc_var_names[["other"]][cached_other]) {
    if (verbose) {
      message(date(), " Reading ", var_name, " (cached)")
    }
    res[[var_name]] <- x$.cache$data[[var_name]]
  }
  for (var_name in nc_var_names[["other"]][!cached_other]) {
    if (verbose) {
      message(date(), " Reading ", var_name)
    }
    if (missing(vars) || var_name %in% vars) {
      dim_var_names <- var_dims[["other"]][[var_name]]
      dim_lengths <- vapply(seq_along(dim_var_names), function(y) {
        nc[["var"]][[var_name]][["dim"]][[y]][["len"]]
      }, 0)
      names(dim_lengths) <- dim_var_names
      if ("np" %in% dim_var_names && thin > 1) {
        np_indices <- seq(1, dim_lengths["np"], by = thin)
        dim_lengths["np"] <- length(np_indices)

        all_values <- array(dim = dim_lengths)

        for (i in seq_along(np_indices)) {
          dim_list <- lapply(dim_lengths, seq_len)
          dim_list[["np"]] <- i
          start_vec <- dim_lengths
          start_vec[] <- 1
          start_vec["np"] <- np_indices[i]
          count_vec <- dim_lengths
          count_vec["np"] <- 1
          all_values <- do.call(
            "[<-",
            c(
              list(all_values),
              unname(dim_list),
              list(ncvar_get(
                nc, var_name, start = start_vec, count = count_vec
              ))
            )
          )
        }
      } else {
        all_values <- ncvar_get(nc, var_name)
      }

      if ("np" %in% names(dim_lengths) && dim_lengths[["np"]] == 1) {
        dim_lengths <- dim_lengths[names(dim_lengths) != "np"]
        dim_var_names <- names(dim_lengths)
      }

      if (any(duplicated(dim_var_names))) {
        duplicated_dim_names <- dim_var_names[duplicated(dim_names)]
        for (dup_dim in duplicated_dim_names) {
          dim_var_names[dim_var_names == dup_dim] <- paste(
            dup_dim, seq_along(dim_var_names[dim_var_names == dup_dim]),
            sep = "."
          )
        }
      }

      ## preserve dimensions of length 1, except "np"
      if (length(dim_lengths) > 0) {
        all_values <- array(all_values, dim = dim_lengths)
      }

      if (!is.null(dim(all_values))) {
        mav <- data.table(melt(
          all_values, varnames = dim_var_names
        ))
        ## remove any extraneous dimensions from melting
        mav <- mav[, c(dim_var_names, "value"), with = FALSE]

        ## find matching time and coord variables
        all_matching_dims <- c()
        for (var_type in c("time", "coord")) {
          matching_dims <- unname(unlist(var_dims[[var_type]])[
            unlist(var_dims[[var_type]]) %in% dim_var_names
          ])
          matching_vars <- names(var_dims[[var_type]])[
            vapply(var_dims[[var_type]], function(x) {
              any(x %in% dim_var_names)
            }, TRUE)
          ]
          all_matching_dims <- union(all_matching_dims, matching_dims)
          if (length(matching_vars) == 1) {
            merge_values <- ncvar_get(nc, matching_vars)
            if (var_type == "coord") {
              if (length(coord_dims[[var_name]]) == 1) {
                dim(merge_values) <- c(dim(merge_values), 1)
              }
              dimnames(merge_values)[[length(dim(merge_values))]] <-
                coord_dims[[var_name]]
              mav_merge <- data.table(melt(
                merge_values, varnames = c(matching_dims, "variable"),
                value.name = var_type
              ))
              mav_merge <- data.table(dcast(
                mav_merge, ... ~ variable, value.var = "coord"
              ))
            } else {
              mav_merge <- data.table(melt(
                merge_values, varnames = c(matching_dims),
                value.name = var_type
              ))
            }
            mav <- merge(mav_merge, mav)
          } else if (length(matching_vars) > 1) {
            stop(
              "Found multiple matching ", var_type, " variables for ",
              var_name, ": ", matching_vars
            )
          }
        }

        for (var in setdiff(all_matching_dims, "ns")) {
          mav[[var]] <- NULL
        }
        table_order <- c(
          setdiff(colnames(mav), c("time", "coord", "value")),
          intersect(colnames(mav), c("time", "coord")), "value"
        )

        mav <- mav[, table_order, with = FALSE]

        ## reorder duplicates
        cols <- setdiff(colnames(mav), "value")
        if (length(cols) > 0) {
          setkeyv(mav, cols)
        }
        rownames(mav) <- seq_len(nrow(mav))

        if ("libbi" %in% class(x) && length(x$coord_dims) > 0 &&
          var_name %in% names(x$coord_dims) && "coord" %in% colnames(mav)) {
          setnames(mav, "coord", x$coord_dims[[var_name]])
        }
        if ("libbi" %in% class(x) && length(x$time_dim) == 1 &&
          "time" %in% colnames(mav)) {
          setnames(mav, "time", x$time_dim)
        }

        for (col in colnames(mav)) {
          ## strip trailing numbers, these indicate duplicate dimensions
          dim_col <- sub("\\.[0-9]+$", "", col)
          if (!missing(dims) && !is.null(dims) && dim_col %in% names(dims)) {
            mav[[col]] <- factor(mav[[col]], labels = dims[[dim_col]])
          } else if (dim_col %in% var_dims[["other"]][[var_name]]) {
            mav[[col]] <- mav[[col]] - 1
          }
        }
      } else {
        ## dimensionless values
        mav <- all_values
      }

      if (!missing(missval_threshold)) {
        if (data.table::is.data.table(mav)) {
          missing_values <- which(mav$value > missval_threshold)
          if (length(missing_values) > 0) {
            mav[missing_values, ]$value <- NA_real_
          }
        } else {
          mav[mav > missval_threshold] <- NA_real_
        }
      }

      if (data.table::is.data.table(mav)) {
        res[[var_name]] <- setDF(mav)
      } else {
        res[[var_name]] <- mav
      }
    }
  }

  if (any(class(x) %in% c("character", "libbi"))) nc_close(nc)

  if ("libbi" %in% class(x) && x$use_cache &&
    (missing(file) || file == "output")) {
    if (is.null(x$.cache[["data"]])) {
      x$.cache$data <- list()
    }
    if (is.null(x$.cache[["thin"]])) {
      x$.cache$thin <- list()
    }
    for (name in nc_var_names[["other"]][!cached_other]) {
      x$.cache$data[[name]] <- res[[name]]
      x$.cache$thin[[name]] <- thin
    }
  }

  if (init_to_param) {
    res <- lapply(res, function(x) {
      if (is.data.frame(x) && "time" %in% colnames(x)) {
        min_time <- min(x$time)
        x <- x[x$time == min_time, ]
        x$time <- NULL
      }
      x
    })
  }

  if (burn > 0) {
    burn <- lapply(res, function(x) {
      if ("np" %in% colnames(x)) {
        x <- x[x$np >= burn, ]
      }
    })
  }

  return(res)
}
