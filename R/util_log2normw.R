#' @rdname log2normw
#' @name log2normw
#' @title Normalize log weights
#' @description
#' This function takes a vector of real values,
#' then takes the exponential and divides by the sum.
#' Substracting the max of the original values
#' increases the numerical stability.
#' @param lw a vector of real values
#' @return a vector of normalized values (summing to 1)
#' @export
#' 

log2normw <- function(lw){
  w <- exp(lw - max(lw))
  return(w / sum(w))
}
