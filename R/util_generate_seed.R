#' @rdname generate_seed
#' @name generate_seed
#' @title Generate a random seed
#' @description This function generates a random seed using the random number generator of R.
#' @return an integer, the random seed
#' @importFrom stats runif
#' @export
generate_seed <- function() {
  return(ceiling(runif(1, -1, .Machine$integer.max - 1)))
}
