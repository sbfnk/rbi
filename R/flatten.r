#' @importFrom data.table data.table setDF rbindlist :=
#' @export
flatten <- function(x) {
  res <- lapply(names(x), function(y) {
    if (is.data.frame(x[[y]])) {
      data.table::data.table(x[[y]])[, var := y]
    } else {
      data.table::data.table(value=x[[y]])[, var := y]
    }
  })
  res <- data.table::setDF(data.table::rbindlist(res, fill=TRUE))
  return(res)
}
