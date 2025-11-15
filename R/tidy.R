#' Tidy bank statement
#' @param dat A data.frame as returned by [read()].
#' @export
tidy <- function(dat) {
  colnames(dat) <- tolower(colnames(dat))
  colnames(dat) <- sub("\\.", "_", colnames(dat))
  dat[["data"]] <- date_from_excel(dat[["data"]])
  dat
}
