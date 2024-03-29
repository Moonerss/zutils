#' Read all excel sheets
#'
#' Read specific sheet or all sheets in an excel.
#'
#' @param file Path to the xls/xlsx file.
#' @param sheets name or number to read, if NULL read all sheets.
#' @param merge Whether merge the data, default FALSE.
#' @param verbose Whether print useful message.
#' @param ... Other arguments of \code{\link[readxl]{read_excel}}
#'
#' @export
#'
read_all_sheets <- function(file, sheets = NULL, merge = FALSE, verbose = TRUE, ...) {

  if (verbose) cli::cli_alert_info("=> Starting")
  # get sheets
  if (is.null(sheets)) {
    all_sheets <- readxl::excel_sheets(file)
  } else {
    all_sheets <- readxl::excel_sheets(file)
    if (is.numeric(sheets)) {
      all_sheets <- all_sheets[sheets]
    } else if (is.character(sheets)) {
      all_sheets <- intersect(all_sheets, sheets)
    } else {
      cli::cli_abort('`sheets` argument must be a numeric or character vector')
    }
  }

  # read sheets
  if (verbose)
    cli::cli_alert_info("==> Reading sheets: \n", paste0('==> ', paste(all_sheets, collapse = ' ')))
  all_list <- lapply(all_sheets, function(x) {readxl::read_excel(file, sheet = x, ...)})
  names(all_list) <- all_sheets
  if (merge) {
    res <- dplyr::bind_rows(all_list, .id = 'sheet')
  } else {
    res <- all_list
  }
  if (verbose) cli::cli_alert_info("=> Done")
  return(res)
}
