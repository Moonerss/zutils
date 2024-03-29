#' @name dirichlet_regression
#' @title Dirichlet multinomial regression to detect changes in cell frequencies
#' @description This function use Dirichlet multinomial regression calculate the cell proportion of single cell data
#' @param counts a matrix with cell number, a samples x cell types matrix
#' @param covariates additional data to use in the regression
#' @param formula the formula of regression
#' @return a matrix of cell proportion
#' @examples
#' \dontrun{
#'   counts = do.call(cbind, tapply(seur@data.info$orig.ident, seur@ident, table))
#'   covariates = data.frame(condition=gsub('[12].*', '', rownames(counts)))
#'   res = dirichlet_regression(counts, covariates, counts ~ condition)
#' }
#'
#' @export

dirichlet_regression <- function(counts, covariates, formula){

  # Dirichlet multinomial regression to detect changes in cell frequencies
  # formula is not quoted, example: counts ~ condition
  # counts is a [samples x cell types] matrix
  # covariates holds additional data to use in the regression
  #
  # Example:
  # counts = do.call(cbind, tapply(seur@data.info$orig.ident, seur@ident, table))
  # covariates = data.frame(condition=gsub('[12].*', '', rownames(counts)))
  # res = dirichlet_regression(counts, covariates, counts ~ condition)

  # Calculate regression
  counts = as.data.frame(counts)
  counts$counts = DirichletReg::DR_data(counts)
  data = cbind(counts, covariates)
  fit = DirichletReg::DirichReg(counts ~ condition, data)

  # Get p-values
  u = summary(fit)
  pvals = u$coef.mat[grep('Intercept', rownames(u$coef.mat), invert=T), 4]
  v = names(pvals)
  pvals = matrix(pvals, ncol=length(u$varnames))
  rownames(pvals) = gsub('condition', '', v[1:nrow(pvals)])
  colnames(pvals) = u$varnames
  fit$pvals = pvals

  fit
}
