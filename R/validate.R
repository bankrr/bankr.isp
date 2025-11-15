validate <- function(dat) {
  expected_cols <- list(
    Data = "character",
    Operazione = "character",
    Dettagli = "character",
    `Conto o carta` = "character",
    Contabilizzazione = "character",
    Categoria = "character",
    Valuta = "character",
    Importo = "character"
  )

  validate_is_dataframe(dat)
  validate_has_rows(dat)
  validate_required_columns(dat, expected_cols)
  validate_no_extra_columns(dat, expected_cols)
  validate_column_types(dat, expected_cols)

  if ("Data" %in% colnames(dat)) {
    invalid_data <- !validate_date_format_excel(dat[["Data"]])
    if (any(invalid_data)) {
      stop(
        "Column 'Data' contains invalid date formats. Expected numeric Excel format.",
        call. = FALSE
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
  validate_no_na_columns(dat, required_no_na_cols)

  invisible(dat)
}
