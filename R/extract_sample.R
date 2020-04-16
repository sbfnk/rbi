#' @rdname extract_sample
#' @name extract_sample
#' @title Extract a sample from a \code{LibBi} run.
#' @description This function takes the provided \code{\link{libbi}}
#'     results and extracts a data frame.
#' @param x a \code{\link{libbi}} object which has been run, or a
#'     list of data frames containing parameter traces (as returned by
#'     from \code{bi_read})
#' @param np iteration to extract; if set to "last", the last sample will be extracted. If not given a random sample will be extracted
#' @param ... parameters to \code{bi_read} (e.g., dimensions)
#' @return a list of data frames or numeric vectors containing parameters and
#' trajectories  
#' @export
extract_sample <- function(x, np, ...) {

  if (("libbi" %in% class(x)) || ("character" %in% class(x))) {
    samples <- do.call(bi_read, list(x=x, ...))
  } else if ("list" %in% class(x)) {
    samples <- x
  } else {
    stop("'x' must be a 'libbi' object or a file name or a list of data frames.")
  }

  if (length(samples) > 0) {
    max_np <- max(vapply(samples[setdiff(names(samples), "clock")], function (y) {
      if (is.data.frame(y) && "np" %in% colnames(y)) {
        max(y$np)
      } else {
        0
      }
    }, 0))

    if (missing(np)) np <- sample(seq_len(max_np), 1)
    find_np <- ifelse(tolower(np) == "last", max_np, np)
    if (find_np > max_np) {
      stop("np requested greater than the maximum ", max_np)
    }
  }

  ret <- lapply(samples[setdiff(names(samples), "clock")], function(y) {
    if (is.data.frame(y) && "np" %in% colnames(y)) {
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
