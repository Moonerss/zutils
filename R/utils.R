
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


#' Transpose dgRMatrix to dgCMatrix
#'
#' @param dgRMatrix_obj a \code{dgRMatrix} object
#'
#' @importFrom methods new
#' @return A dgCMatrix that is the transposed dgRMatrix
#' @export
transpose_dgRMatrix <- function(dgRMatrix_obj) {
  if(!inherits(dgRMatrix_obj, 'dgRMatrix'))
    stop('dgRMatrix_obj is not of class dgRMatrix')
  out <- new('dgCMatrix',
             i=dgRMatrix_obj@j,
             p=dgRMatrix_obj@p,
             x=dgRMatrix_obj@x,
             Dim=rev(dgRMatrix_obj@Dim),
             Dimnames=rev(dgRMatrix_obj@Dimnames)
  )
  out
}
