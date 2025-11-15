#' Read bank statement
#' @param path A path.
#' @export
read <- function(path) {
  if (!(is.character(path) && length(path) == 1)) {
    stop("Path is not a character of length one.")
  }

  if (!file.exists(path)) {
    stop("File does not exist.")
  }

  # suppress new names message
  tmp <- suppressMessages(
    readxl::read_xlsx(
      path,
      n_max = 50,
      .name_repair = "unique"
    )
  )

  idx <- which(grepl("Data", tmp[[1L]])) + 5

  dat <- validate(as.data.frame(readxl::read_xlsx(path, skip = idx, col_types = "text")))
  dat
}
