#' @rdname merge_by_name
#' @name merge_by_name
#' @title Merge lists by name
#' @description
#' This function is used to create a single list from any number of named lists. Every name is unique in the new list. If names occur in multiple of the lists passed, later options overwrite earlier ones
#'
#' @param ... the lists to be merged
#' @return the merged list
merge_by_name <- function(...){

  merged <- list()
  args <- c(...)

  for (element in names(args)) {
    merged[[element]] <- args[[element]]
  }

  return(merged)
}
