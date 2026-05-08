# Internal data-boundary helpers ---------------------------------------------
#
# The package ships data from 1987-10 through 2024-06.
# Locally downloaded update data must begin with 2024-07.

slc_schema_version <- 1L

slc_bundled_start_year <- 1987L
slc_bundled_start_month <- 10L

slc_bundled_end_year <- 2024L
slc_bundled_end_month <- 6L

slc_first_download_year <- 2024L
slc_first_download_month <- 7L


# Constructors ---------------------------------------------------------------

#' Construct an internal slcflights year-month object
#'
#' Creates the small internal object used to represent a single calendar
#' year-month throughout update and cache code.
#'
#' @param year Integer-like calendar year.
#' @param month Integer-like calendar month.
#'
#' @returns
#' A list with integer `year` and `month` fields and class `"slc_year_month"`.
#'
#' @noRd
new_year_month <- function(year, month) {
  structure(
    list(
      year = as.integer(year),
      month = as.integer(month)
    ),
    class = "slc_year_month"
  )
}


# Validators -----------------------------------------------------------------

#' Validate an internal slcflights year-month object
#'
#' Checks that an object is a single, complete, four-digit calendar year and
#' month represented as a `"slc_year_month"` object.
#'
#' @param x Object to validate.
#' @param arg Argument name to use in error messages.
#'
#' @returns
#' The validated `x`, invisibly unchanged.
#'
#' @noRd
validate_year_month <- function(x, arg = "x") {
  if (!inherits(x, "slc_year_month")) {
    stop(
      sprintf("`%s` must be a slcflights year-month object.", arg),
      call. = FALSE
    )
  }

  if (!is.list(x) || !all(c("year", "month") %in% names(x))) {
    stop(
      sprintf("`%s` must contain `year` and `month`.", arg),
      call. = FALSE
    )
  }

  year <- x$year
  month <- x$month

  if (length(year) != 1L || length(month) != 1L) {
    stop(
      sprintf("`%s` must identify exactly one year and one month.", arg),
      call. = FALSE
    )
  }

  if (!is.integer(year) || !is.integer(month)) {
    stop(
      sprintf("`%s` must contain integer `year` and `month` values.", arg),
      call. = FALSE
    )
  }

  if (is.na(year) || is.na(month)) {
    stop(
      sprintf("`%s` must not contain missing values.", arg),
      call. = FALSE
    )
  }

  if (year < 1000L || year > 9999L) {
    stop(
      sprintf("`%s$year` must be a four-digit calendar year.", arg),
      call. = FALSE
    )
  }

  if (month < 1L || month > 12L) {
    stop(
      sprintf("`%s$month` must be an integer from 1 to 12.", arg),
      call. = FALSE
    )
  }

  x
}

#' Coerce numeric year and month values to a year-month object
#'
#' Converts whole-number numeric year and month values into the internal
#' `"slc_year_month"` representation.
#'
#' @param year Numeric calendar year.
#' @param month Numeric calendar month.
#' @param arg Argument name to use in error messages.
#'
#' @returns
#' A validated `"slc_year_month"` object.
#'
#' @noRd
as_year_month <- function(year, month, arg = "x") {
  if (length(year) != 1L || length(month) != 1L) {
    stop(
      sprintf("`%s` must identify exactly one year and one month.", arg),
      call. = FALSE
    )
  }

  if (is.na(year) || is.na(month)) {
    stop(
      sprintf("`%s` must not contain missing or non-finite values.", arg),
      call. = FALSE
    )
  }

  if (!is.numeric(year) || !is.numeric(month)) {
    stop(
      sprintf("`%s` must identify a numeric year and month.", arg),
      call. = FALSE
    )
  }

  if (!is.finite(year) || !is.finite(month)) {
    stop(
      sprintf("`%s` must not contain missing or non-finite values.", arg),
      call. = FALSE
    )
  }

  if (year != trunc(year) || month != trunc(month)) {
    stop(
      sprintf("`%s` must use whole-number year and month values.", arg),
      call. = FALSE
    )
  }

  validate_year_month(
    new_year_month(year, month),
    arg = arg
  )
}

#' Parse a YYYY-MM string as a year-month object
#'
#' Converts a string of the form `"YYYY-MM"` into the internal
#' `"slc_year_month"` representation.
#'
#' @param x Character string to parse.
#' @param arg Argument name to use in error messages.
#'
#' @returns
#' A validated `"slc_year_month"` object.
#'
#' @noRd
as_year_month_string <- function(x, arg = "x") {
  if (length(x) != 1L || !is.character(x) || is.na(x)) {
    stop(
      sprintf("`%s` must be a string of the form \"YYYY-MM\".", arg),
      call. = FALSE
    )
  }

  if (!grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    stop(
      sprintf("`%s` must be a string of the form \"YYYY-MM\".", arg),
      call. = FALSE
    )
  }

  as_year_month(
    year = as.integer(substr(x, 1L, 4L)),
    month = as.integer(substr(x, 6L, 7L)),
    arg = arg
  )
}


# Formatting and indexing ----------------------------------------------------

#' Format a year-month object
#'
#' Formats an internal year-month object as a `"YYYY-MM"` string.
#'
#' @param x Internal `"slc_year_month"` object.
#'
#' @returns
#' A character string of the form `"YYYY-MM"`.
#'
#' @noRd
format_year_month <- function(x) {
  x <- validate_year_month(x)
  sprintf("%04d-%02d", x$year, x$month)
}

#' Convert a year-month object to a sortable month index
#'
#' Converts an internal year-month object to a numeric month index used for
#' ordering and constructing consecutive month sequences.
#'
#' @param x Internal `"slc_year_month"` object.
#'
#' @returns
#' Integer month index.
#'
#' @noRd
year_month_index <- function(x) {
  x <- validate_year_month(x)
  x$year * 12L + x$month
}

#' Convert a month index to a year-month object
#'
#' Converts an integer month index back to the internal `"slc_year_month"`
#' representation.
#'
#' @param index Whole-number month index.
#'
#' @returns
#' A validated `"slc_year_month"` object.
#'
#' @noRd
index_to_year_month <- function(index) {
  if (length(index) != 1L || is.na(index)) {
    stop("`index` must be a single non-missing numeric value.", call. = FALSE)
  }

  if (!is.numeric(index)) {
    stop("`index` must be a single non-missing numeric value.", call. = FALSE)
  }

  if (!is.finite(index) || index != trunc(index)) {
    stop("`index` must be a finite whole-number value.", call. = FALSE)
  }

  index <- as.integer(index)

  validate_year_month(
    new_year_month(
      year = (index - 1L) %/% 12L,
      month = ((index - 1L) %% 12L) + 1L
    )
  )
}


# Boundary helpers -----------------------------------------------------------

#' Return the first bundled slcflights month
#'
#' Returns the first month represented in the installed historical data.
#'
#' @returns
#' A `"slc_year_month"` object for October 1987.
#'
#' @noRd
slc_bundled_start <- function() {
  new_year_month(
    slc_bundled_start_year,
    slc_bundled_start_month
  )
}

#' Return the last bundled slcflights month
#'
#' Returns the final month represented in the installed historical data.
#'
#' @returns
#' A `"slc_year_month"` object for June 2024.
#'
#' @noRd
slc_bundled_end <- function() {
  new_year_month(
    slc_bundled_end_year,
    slc_bundled_end_month
  )
}

#' Return the first downloadable update month
#'
#' Returns the first month that local update workflows may download after the
#' bundled historical data.
#'
#' @returns
#' A `"slc_year_month"` object for July 2024.
#'
#' @noRd
slc_first_download <- function() {
  new_year_month(
    slc_first_download_year,
    slc_first_download_month
  )
}


# Update endpoint helpers ----------------------------------------------------

#' Normalize an update endpoint
#'
#' Normalizes the user-facing `until` argument used by update workflows.
#'
#' @param until Update endpoint: `"latest"`, a `"YYYY-MM"` string, or
#'   `c(year, month)`.
#'
#' @returns
#' Either the string `"latest"` or a validated `"slc_year_month"` object.
#'
#' @noRd
normalize_update_until <- function(until = "latest") {
  if (is.character(until) && identical(until, "latest")) {
    return("latest")
  }

  if (is.character(until)) {
    return(as_year_month_string(until, arg = "until"))
  }

  if (is.numeric(until) && length(until) == 2L) {
    return(as_year_month(until[[1]], until[[2]], arg = "until"))
  }

  stop(
    paste(
      "`until` must be \"latest\", a string of the form \"YYYY-MM\",",
      "or c(year, month)."
    ),
    call. = FALSE
  )
}

#' Validate a concrete update endpoint
#'
#' Checks that a concrete update endpoint is not earlier than the first
#' downloadable update month.
#'
#' @param until Internal `"slc_year_month"` object.
#'
#' @returns
#' The validated `"slc_year_month"` object.
#'
#' @noRd
validate_update_until <- function(until) {
  until <- validate_year_month(until, arg = "until")
  first_download <- slc_first_download()

  if (year_month_index(until) < year_month_index(first_download)) {
    stop(
      paste(
        "The bundled slcflights data end in June 2024.",
        "Downloaded data must begin with July 2024."
      ),
      call. = FALSE
    )
  }

  until
}

#' Build the consecutive update month table
#'
#' Builds the consecutive sequence of update months from July 2024 through the
#' requested endpoint.
#'
#' @param until Internal `"slc_year_month"` update endpoint.
#'
#' @returns
#' A data frame with integer `year` and `month` columns.
#'
#' @noRd
update_month_sequence <- function(until) {
  until <- validate_update_until(until)

  indexes <- seq.int(
    year_month_index(slc_first_download()),
    year_month_index(until)
  )

  months <- lapply(indexes, index_to_year_month)

  data.frame(
    year = vapply(months, `[[`, integer(1), "year"),
    month = vapply(months, `[[`, integer(1), "month")
  )
}
