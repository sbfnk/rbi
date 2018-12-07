#' @rdname flatten
#' @name flatten
#' @title Flatten list of data frames
#' This function takes a list of data frames (such as, for example, returned by \code{\link{bi_read}}) and converts it to a flat data frame
#' @export
#' @param x The list of data frames
flatten <- function(x) {
  res <- lapply(names(x), function(y) {
    if (is.data.frame(x[[y]])) {
      x[[y]]$var <- y
    } else {
      x[[y]] <- data.frame(value=x[[y]], var=y)
    }
    x[[y]]
  })

  res <- data.table::setDF(data.table::rbindlist(res, fill=TRUE))
  early_columns <- "np"
  late_columns <- c("time", "value", "var")
  columns <- colnames(res)
  reorder <-
      c(intersect(early_columns, columns),
        setdiff(columns, union(early_columns, late_columns)),
        intersect(late_columns, columns))

  return(res[, reorder])
}
