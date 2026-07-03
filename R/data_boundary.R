# Internal data-boundary helpers ---------------------------------------------
#
# The package builds a local SLC database from BTS monthly files. It does not
# ship flight data or support-table data. The first supported database month is
# October 1987, matching the start of BTS On-Time Performance data.

slc_schema_version <- 1L

slc_db_start_year <- 1987L
slc_db_start_month <- 10L

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

#' Return the first slcflights database month
#'
#' Returns the first month supported by the local slcflights database.
#'
#' @returns
#' A `"slc_year_month"` object for October 1987.
#'
#' @noRd
slc_db_start <- function() {
  new_year_month(
    slc_db_start_year,
    slc_db_start_month
  )
}

# Database endpoint helpers --------------------------------------------------

#' Normalize a database endpoint
#'
#' Normalizes the user-facing `until` argument used by local database build
#' workflows.
#'
#' @param until Database endpoint: `"latest"`, a four-digit year, a
#'   `"YYYY-MM"` string, or `c(year, month)`.
#'
#' @returns
#' Either the string `"latest"` or a validated `"slc_year_month"` object.
#'
#' @noRd
normalize_db_until <- function(until) {
  if (is.character(until) && identical(until, "latest")) {
    return("latest")
  }

  if (is.character(until)) {
    return(as_year_month_string(until, arg = "until"))
  }

  if (is.numeric(until) && length(until) == 1L) {
    return(as_year_month(until, 12, arg = "until"))
  }

  if (is.numeric(until) && length(until) == 2L) {
    return(as_year_month(until[[1]], until[[2]], arg = "until"))
  }

  stop(
    paste(
      "`until` must be \"latest\", a four-digit year,",
      "a string of the form \"YYYY-MM\", or c(year, month)."
    ),
    call. = FALSE
  )
}

#' Validate a concrete database endpoint
#'
#' Checks that a concrete database endpoint is not earlier than the first
#' supported slcflights database month.
#'
#' @param until Internal `"slc_year_month"` object.
#'
#' @returns
#' The validated `"slc_year_month"` object.
#'
#' @noRd
validate_db_until <- function(until) {
  until <- validate_year_month(until, arg = "until")
  db_start <- slc_db_start()

  if (year_month_index(until) < year_month_index(db_start)) {
    stop(
      "The slcflights database must begin with October 1987.",
      call. = FALSE
    )
  }

  until
}

#' Build the consecutive database month table
#'
#' Builds the consecutive sequence of database months from October 1987 through
#' the requested endpoint.
#'
#' @param until Internal `"slc_year_month"` database endpoint.
#'
#' @returns
#' A data frame with integer `year` and `month` columns.
#'
#' @noRd
db_month_sequence <- function(until) {
  until <- validate_db_until(until)

  indexes <- seq.int(
    year_month_index(slc_db_start()),
    year_month_index(until)
  )

  months <- lapply(indexes, index_to_year_month)

  data.frame(
    year = vapply(months, `[[`, integer(1), "year"),
    month = vapply(months, `[[`, integer(1), "month")
  )
}

#' Build the contiguous database extension month table
#'
#' Builds the consecutive sequence of months needed to extend a local database
#' from its current endpoint through a requested endpoint.
#'
#' @param current_end Current local database endpoint.
#' @param until Requested local database endpoint.
#'
#' @returns
#' A data frame with integer `year` and `month` columns.
#'
#' @noRd
db_extension_months <- function(current_end, until) {
  current_end <- validate_year_month(current_end, arg = "current_end")
  until <- validate_db_until(until)

  first_index <- year_month_index(current_end) + 1L
  last_index <- year_month_index(until)

  if (last_index < first_index) {
    return(data.frame(year = integer(), month = integer()))
  }

  months <- lapply(seq.int(first_index, last_index), index_to_year_month)

  data.frame(
    year = vapply(months, `[[`, integer(1), "year"),
    month = vapply(months, `[[`, integer(1), "month")
  )
}
