#' @rdname extract_sample
#' @name extract_sample
#' @title Extract a sample from a \code{\link{LibBi}} run.
#' @description This function takes the provided \code{\link{libbi}} 
#'     object which has been run and extracts a data frame.
#'     traces.
#' @param x a \code{\link{libbi}} object which has been run, or a
#'     list of data frames containing parameter traces (as returned by
#'     from \code{bi_read}); if it is not a \code{\link{libbi}}
#'     object, either 'all' must be TRUE or a model given
#' @param np iteration to extract; if not given a random sample will be extracted
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return list of parameters and trajectories
#' @export
extract_sample <- function(x, np, ...) {

  read_options <- list(...)

  if (("libbi" %in% class(x)) || ("character" %in% class(x))) {
    samples <- do.call(bi_read, read_options)
  } else if ("list" %in% class(run)) {
    samples <- x
  } else {
    stop("'x' must be a 'libbi' object or a file name or a list of data frames.")
  }

  find_np <- np

  ret <- lapply(x, function(y) {
    if (is.data.frame(y)) {
      df <- y[y[, "np"] == find_np, ]
      df$np <- NULL
      rownames(df) <- NULL
      if (nrow(df) == 1 && ncol(df) == 1) df <- unname(unlist(df))
      df
    } else {
      y
    }
  })

  return(ret)
}
