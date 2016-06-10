#' @rdname get_traces
#' @name get_traces
#' @title Get the parameter traces
#' @description This function takes the provided \code{\link{libbi}}
#'     which has been run and returns a data frame with the parameter
#'     traces.
#' @param run a \code{\link{libbi}} object which has been run, or a
#'     list of data frames containing parameter traces (as returned by
#'     from \code{bi_read}); if it is not a \code{\link{libbi}}
#'     object, either 'all' must be TRUE or a model given
#' @param all whether all variables in the run file should be
#'     considered (otherwise, just parameters)
#' @param model a model to get the parameter names from; not needed if
#'     'run' is given as a \code{\link{libbi}} object or 'all' is set
#'     to TRUE
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return data frame with parameter traces; this can be fed to
#'     \code{coda} routines
#' @importFrom reshape2 dcast
#' @export
get_traces <- function(run, all = FALSE, model, ...) {

  read_options <- list(...)

  if ("libbi" %in% class(run)) {
    if (missing(model)) {
      model <- run$model
    } else {
      warning("Given model will overwrite model contained in given 'run'.")
    }
    read_options <- c(read_options, list(read = run$output_file_name))
  } else {
    read_options <- c(read_options, list(read = run))
  }

  if (!all) {
    if (missing(model)) {
      stop("Either 'all' must be set to TRUE, or a model given (via the 'run' or 'model' options)")
    } else {
      read_options <- c(read_options, list(vars = model$get_vars("param")))
    }
  }

  if (("libbi" %in% class(run)) | ("character" %in% class(run))) {
      res <- do.call(bi_read, read_options)
  } else if ("list" %in% class(run)) {
      if (all) {
        res <- run
      } else {
        res <- run[intersect(names(run), model$get_vars("params"))]
      }
  } else {
      stop("'run' must be a 'libbi' object or a file name or a list of data frames.")
  }
      
  wide_list <- lapply(names(res), function(param) {
    extra.dims <- setdiff(colnames(res[[param]]), c("np", "param", "value"))
    if (length(extra.dims) > 0) {
      df <- dcast(res[[param]],
                  as.formula(paste("np", paste(extra.dims, collapse = "+"), sep = "~")),
                  value.var = "value")
      names(df)[-1] <- paste(param, names(df)[-1], sep = ".")
    } else {
      df <- res[[param]]
      names(df)[which(names(df) == "value")] <- param
    }
    df[, -1, drop = FALSE]
  })

  return(do.call(cbind, wide_list))
}
