# Internal reader path wrappers ----------------------------------------------
#
# These wrappers keep the reader-facing internal interface small while
# delegating file discovery to the installed-data and cache-aware path helpers.

.slcflights_file <- function(...) {
  slc_extdata_file(...)
}

.coords_path <- function() {
  slc_coords_path()
}

.parquet_root <- function() {
  slc_installed_parquet_root()
}

.normalize_years <- function(years) {
  normalize_data_years(years)
}

.available_years_internal <- function(type = c("main", "div")) {
  type <- match.arg(type)
  slc_available_data_years(type)
}

.parquet_paths <- function(type = c("main", "div"), years = NULL) {
  type <- match.arg(type)

  yrs <- .normalize_years(years)

  if (is.null(yrs)) {
    paths <- rbind(
      slc_installed_data_paths(type),
      slc_cached_data_paths(type)
    )
  } else {
    paths <- rbind(
      slc_installed_data_paths(type, years = yrs),
      slc_cached_data_paths(type, years = yrs)
    )
  }

  label <- switch(type,
    main = "main",
    div = "diversion"
  )

  if (!nrow(paths)) {
    if (is.null(yrs)) {
      stop(
        sprintf("No %s parquet files are available", label),
        call. = FALSE
      )
    }

    stop(
      sprintf(
        "No %s parquet file found for year%s %s",
        label,
        if (length(yrs) > 1L) "s" else "",
        paste(yrs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(yrs)) {
    missing <- setdiff(yrs, paths$year)

    if (length(missing)) {
      stop(
        sprintf(
          "No %s parquet file found for year%s %s",
          label,
          if (length(missing) > 1L) "s" else "",
          paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  paths <- paths[order(
    paths$year, match(paths$source, c("installed", "cache"))
  ), ]
  row.names(paths) <- NULL

  unname(paths$path)
}
