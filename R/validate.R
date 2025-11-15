validate <- function(dat) {
  expected_cols <- list(
    Data = "double",
    Operazione = "character",
    Dettagli = "character",
    `Conto o carta` = "character",
    Contabilizzazione = "character",
    Categoria = "character",
    Valuta = "character",
    Importo = "double"
  )

  if (!is.data.frame(dat)) {
    stop("Input must be a data.frame")
  }

  if (!nrow(dat) > 0) {
    stop("Input have at least one row")
  }

  if (!all(names(expected_cols) %in% colnames(dat))) {
    missing_cols <- setdiff(names(expected_cols), colnames(dat))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!all(colnames(dat) %in% names(expected_cols))) {
    extra_cols <- setdiff(colnames(dat), expected_cols)
    warning("Unexpected columns found: ", paste(extra_cols, collapse = ", "))
  }

  for (col in expected_cols) {
    if (col %in% colnames(dat)) {
      col_class <- class(dat[[col]])
      expected_class <- expected_cols[[col]]
      if (!any(col_class %in% expected_class)) {
        stop(
          "Column '",
          col,
          "' has incorrect type. Expected: ",
          paste(expected_class, collapse = " or "),
          ", Got: ",
          paste(col_class, collapse = ", ")
        )
      }
    }
  }

  # Validate date format for DATA and VALUTA columns
  date_pattern <- "\\d{5}\\.0"
  if ("Data" %in% colnames(dat)) {
    invalid_data <- !grepl(date_pattern, dat[["Data"]], perl = TRUE)
    if (any(invalid_data)) {
      stop(
        "Column 'Data' contains invalid date formats. Expected numeric."
      )
    }
  }

  # Validate Italian number format for DARE and AVERE columns
  # Pattern matches: optional digits with dots as thousand separators, comma for decimal, then 2 digits
  italian_number_pattern <- "^-?\\d{1,3}(\\.\\d{3})*.\\d{2}$"
  if ("Importo" %in% colnames(dat)) {
    non_na_dare <- !is.na(dat[["Importo"]])
    if (any(non_na_dare)) {
      invalid_dare <- !grepl(
        italian_number_pattern,
        dat[["Importo"]][non_na_dare],
        perl = TRUE
      )
      if (any(invalid_dare)) {
        stop(
          "Column 'Importo' contains invalid number formats. Expected format (e.g., '-20.97')."
        )
      }
    }
  }

  # Check that required columns don't contain NA values
  required_no_na_cols <- c("Data", "Importo", "Categoria", "Operazione")
  for (col in required_no_na_cols) {
    if (col %in% colnames(dat)) {
      if (anyNA(dat[[col]])) {
        stop("Column '", col, "' contains NA values.")
      }
    }
  }

  invisible(dat)
}
