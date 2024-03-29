#' @title Impute the NA value in matrix
#'
#' @description Impute the NA value in a matrix by different method
#'
#' @param mat a matrix object
#' @param method The method used to fill NA value. Can be
#' \describe{
#'    \item{\code{median}:}{Use the row or column median value to fill NA}
#'    \item{\code{mean}:}{Use the row or column mean value to fill NA}
#'    \item{\code{min}:}{Use the global min value to fill NA}
#'    \item{\code{max}:}{Use the global max value to fill NA}
#' }
#'
#' @param MARGIN choose the direction two get the mean or median value to fill NA. It is only useful when \code{method} is \code{mean} or \code{median}
#' @param contant_value the contant value to fill NA. Default is NULL. If you set this argument, all NA value in the matrix will fill by it
#'
#' @importFrom stats median
#' @return return a matrix after fill the NA
#'
#' @examples
#' \dontrun{
#'  a <- matrix(c(1:5, NA), ncol = 2)
#'  fill_by_constant(mat = a)
#'  fill_by_constant(mat = a, contant_value = 1)
#'
#' }
#' @export
#'
fill_by_constant <- function(mat, method = c('median', 'mean', 'min', 'max'), MARGIN = c('row', 'column'), contant_value = NULL) {

  method <- match.arg(method)
  MARGIN <- match.arg(MARGIN)

  if (is.null(contant_value)) {
    if (method %in% c('median', 'mean')) {
      cli::cli_alert_info(paste("Fill the empty value by {MARGIN} {method} value"))
      mat <- apply(mat, ifelse(MARGIN == 'row', 1, 2), function(x) {
        x <- as.numeric(x)
        x[is.na(x)] <- switch(method,
                              median = median(x, na.rm = T),
                              mean = mean(x, na.rm = T)
        )
        x
      })
      if (MARGIN == 'row') {
        mat <- t(mat)
      }
    } else if (method %in% c('min', 'max')) {
      cli::cli_alert_info("Fill the empty value by the {type} value in the data")
      mat[which(is.na(mat))] <- switch(method,
                                       min = min(mat, na.rm = T),
                                       max = max(mat, na.rm = T)

      )
    }
  } else {
    cli::cli_alert_info("Fill the empty value by {contant_value}")
    mat[which(is.na(mat))] <- contant_value
  }

  return(mat)
}

#' Impute NA by normal distribution
#'
#' Impute NA values by random numbers drawn from a normal distribution that has a down-shifted mean
#' and shrunken standard deviation from the sample distribution. This is meant to be similar to imputation
#' in the Perseus software.
#'
#' @param mat A matrix
#' @param width Scale factor for the standard deviation of imputed distribution relative to the sample standard deviation.
#' @param downshift Down-shifted the mean of imputed distribution from the sample mean, in units of sample standard deviation.
#' @param seed Random seed. Defalut: 1
#'
#' @importFrom stats rnorm sd
#'
#' @return return a matrix after fill the NA
#'
#' @examples
#' \dontrun{
#'   ww <- matrix(data = 1:20, ncol = 4, nrow = 5)
#'   ww[13] <- NA
#'   fill_by_normal_distribution(ww)
#' }
#'
#' @export
fill_by_normal_distribution <- function(mat, width=0.3, downshift=1.8, seed = 1) {
  mx <- max(mat, na.rm=TRUE)
  mn <- min(mat, na.rm=TRUE)
  if (mx - mn > 20) {
    cli::cli_alert_warning('Please make sure the values are log-transformed.')
  }
  set.seed(seed)
  mat <- apply(mat, 1, function(temp) {
    temp[!is.finite(temp)] <- NA
    temp_sd <- stats::sd(temp, na.rm=TRUE)
    temp_mean <- mean(temp, na.rm=TRUE)
    shrinked_sd <- width * temp_sd   # shrink sd width
    downshifted_mean <- temp_mean - downshift * temp_sd   # shift mean of imputed values
    n_missing <- sum(is.na(temp))
    temp[is.na(temp)] <- stats::rnorm(n_missing, mean=downshifted_mean, sd=shrinked_sd)
    temp
  }) %>% t()
  return(mat)
}


#' Fill NA by KNN method
#'
#' A function to fill na value using nearest neighbor averaging.
#'
#' @param mat A matrix
#' @param ... Other arguments of \code{\link[impute]{impute.knn}}
#'
#' @return A matrix with imputed values
#'
#' @examples
#' \dontrun{
#'   ww <- matrix(data = 1:20, ncol = 4, nrow = 5)
#'   ww[13] <- NA
#'   fill_by_knn(ww)
#' }
#'
#' @export
fill_by_knn <- function(mat, ...) {
  res <- impute::impute.knn(mat, ...)$data
  colnames(res) <- colnames(mat)
  return(res)
}


#' @name na_ratio
#' @title NA ratio in data
#' @description This function get the NA ratio in the whole data
#' @param x a matrix with any type data
#' @return a value of NA ratio
#' @export
#'
na_ratio <- function(x){
  sum(is.na(x))/dim(x)[1]/dim(x)[2]
}

#' @name na_ratio_row
#' @title NA ratio of each row in data
#' @description This function get the NA ratio in the row of data
#' @param x a matrix with any type data
#' @return a vector value of NA ratio in each row
#' @export
#'
na_ratio_row <- function(x) {
  apply(x, 1, function(y) sum(is.na(y))/length(y))
}

#' @name na_ratio_col
#' @title NA ratio of each column in data
#' @description This function get the NA ratio in the column of data
#' @param x a matrix with any type data
#' @return a vector value of NA ratio in each column
#' @export
#'
na_ratio_col <- function(x) {
  apply(x, 2, function(y) sum(is.na(y))/length(y))
}





