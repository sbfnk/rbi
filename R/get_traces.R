#' @rdname get_traces
#' @name get_traces
#' @title Get the parameter traces
#' @description This function takes the provided \code{\link{libbi}} object
#'     which has been run and returns a data frame with the parameter
#'     traces.
#' @param x a \code{\link{libbi}} object which has been run, or a
#'     list of data frames containing parameter traces (as returned by
#'     \code{bi_read}); if it is not a \code{\link{libbi}}
#'     object, either 'all' must be TRUE or a model given
#' @param all whether all variables in the run file should be
#'     considered (otherwise, just parameters)
#' @param model a model to get the parameter names from; not needed if
#'     'run' is given as a \code{\link{libbi}} object or 'all' is set
#'     to TRUE
#' @param burnin proportion of iterations to discard as burn-in (if between 0
#'   and 1), or number of samples to discard (if >1)
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return a ata frame with parameter traces; this can be fed to
#'     \code{coda} routines
#' @importFrom data.table data.table dcast
#' @importFrom stats as.formula
#' @export
get_traces <- function(x, model, burnin, all = FALSE, ...) {
  read_options <- list(x = x)

  if ("libbi" %in% class(x)) {
    if (missing(model)) {
      model <- x$model
    } else {
      warning(
        "Given model will overwrite model contained in given libbi object."
      )
    }
  } else {
    if (!missing(model)) {
      if (!("bi_model" %in% class(model))) model <- bi_model(model)
      read_options <- c(read_options, list(model = model))
    }
  }

  if (!all) {
    if (missing(model)) {
      stop(
        "Either 'all' must be set to TRUE, or a model given (implicitly via ",
        "the 'x' or explicitly via 'model' options)"
      )
    } else {
      types <- "param"
      if ("libbi" %in% class(x) &&
        "with-transform-initial-to-param" %in% names(x$options)) {
        types <- c(types, "state")
      }
      read_options <- c(read_options, list(type = types))
    }
  }

  if (("libbi" %in% class(x)) || ("character" %in% class(x))) {
    dot_options <- list(...)
    for (dot_option in names(dot_options)) {
      read_options[[dot_option]] <- dot_options[[dot_option]]
    }
    res <- do.call(bi_read, read_options)
  } else if ("list" %in% class(x)) {
    if (all) {
      res <- x
    } else {
      res <- x[intersect(names(x), var_names(model, type = "param"))]
    }
  } else {
    stop(
      "'x' must be a 'libbi' object or a file name or a list of data frames."
    )
  }

  if (!missing(burnin) && !(burnin > 0)) {
    stop("'burnin' must be greater than 0.")
  }

  wide_list <- lapply(names(res), function(param) {
    if ("time" %in% colnames(res[[param]])) {
      res[[param]] <-
        res[[param]][res[[param]]$time == min(res[[param]]$time), ]
      res[[param]][["time"]] <- NULL
    }
    extra_dims <- setdiff(colnames(res[[param]]), c("np", "param", "value"))
    if (length(extra_dims) > 0) {
      df <-
        dcast(data.table(res[[param]]),
          as.formula(
            paste(
              "np", paste(paste0("`", extra_dims, "`"), collapse = "+"),
              sep = "~"
            )
          ), value.var = "value"
        )
      names(df)[-1] <- paste(param, names(df)[-1], sep = ".")
    } else {
      if (prod(dim(res[[param]])) == 1) {
        ## just a single value
        df <- data.frame(np = 0, value = res[[param]])
      } else {
        df <- res[[param]]
      }
      names(df)[which(names(df) == "value")] <- param
    }
    df[, -1, drop = FALSE]
  })

  ret <- do.call(cbind, wide_list)

  if (!missing(burnin)) {
    if (burnin <= 1) {
      burnin <- floor(burnin * nrow(ret))
    }
    ret <- ret[-seq_len(burnin), ]
  }

  return(ret)
}
