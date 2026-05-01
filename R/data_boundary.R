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

format_year_month <- function(x) {
  x <- validate_year_month(x)
  sprintf("%04d-%02d", x$year, x$month)
}

year_month_index <- function(x) {
  x <- validate_year_month(x)
  x$year * 12L + x$month
}

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

slc_bundled_start <- function() {
  new_year_month(
    slc_bundled_start_year,
    slc_bundled_start_month
  )
}

slc_bundled_end <- function() {
  new_year_month(
    slc_bundled_end_year,
    slc_bundled_end_month
  )
}

slc_first_download <- function() {
  new_year_month(
    slc_first_download_year,
    slc_first_download_month
  )
}


# Update endpoint helpers ----------------------------------------------------

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
