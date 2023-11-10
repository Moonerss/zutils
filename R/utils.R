
##' get finite number
##'
##'
##' @title get the finite number
##' @param x a vector of number
##' @return vector
##' @author Erjie Zhao
##' @export
get_finite <- function(x) {
  x[is.finite(x)]
}
