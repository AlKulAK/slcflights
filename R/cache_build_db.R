# Internal cache build database helpers --------------------------------------
#
# These helpers provide low-level DuckDB and file-discovery utilities for the
# runtime cache builder. They do not download data, build Parquet files, write
# manifests, or change reader behavior.

cache_build_connect <- function() {
  DBI::dbConnect(duckdb::duckdb())
}

cache_build_disconnect <- function(con) {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }

  invisible(TRUE)
}

cache_build_quote_path <- function(con, path, must_work = TRUE) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = must_work
  )

  DBI::dbQuoteString(con, path)
}

cache_build_quote_paths <- function(con, paths, must_work = TRUE) {
  if (!length(paths)) {
    stop("`paths` must contain at least one path.", call. = FALSE)
  }

  quoted <- vapply(
    paths,
    function(path) {
      as.character(cache_build_quote_path(
        con,
        path,
        must_work = must_work
      ))
    },
    character(1)
  )

  paste(quoted, collapse = ", ")
}

cache_raw_ontime_csvs <- function(year, months = NULL) {
  year <- normalize_cache_year(year)

  if (is.null(months)) {
    root <- slc_cache_raw_ontime_root(create = FALSE)

    if (!dir.exists(root)) {
      return(character())
    }

    dirs <- list.dirs(
      root,
      recursive = FALSE,
      full.names = FALSE
    )

    pattern <- sprintf("^%04d_([0-9]{2})$", year)
    dirs <- grep(pattern, dirs, value = TRUE)

    if (!length(dirs)) {
      return(character())
    }

    months <- as.integer(sub(pattern, "\\1", dirs))
  }

  months <- normalize_cache_months(months)

  files <- unlist(
    lapply(
      months,
      function(month) {
        month_dir <- slc_cache_raw_ontime_month_dir(
          year,
          month,
          create = FALSE
        )

        bts_csv_files(month_dir)
      }
    ),
    use.names = FALSE
  )

  sort(files)
}

normalize_cache_months <- function(months) {
  if (length(months) == 0L) {
    stop("`months` must contain at least one month.", call. = FALSE)
  }

  if (anyNA(months)) {
    stop("`months` must not contain missing values.", call. = FALSE)
  }

  if (!is.numeric(months)) {
    stop("`months` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(months))) {
    stop("`months` must contain only finite values.", call. = FALSE)
  }

  if (any(months != trunc(months))) {
    stop("`months` must contain whole-number months.", call. = FALSE)
  }

  months <- sort(unique(as.integer(months)))

  if (any(months < 1L | months > 12L)) {
    stop("`months` must contain values from 1 to 12.", call. = FALSE)
  }

  months
}

cache_build_read_parquet_cols <- function(con, path) {
  names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet(%s) LIMIT 0",
      cache_build_quote_path(con, path)
    )
  ))
}

cache_build_read_csv_cols <- function(con, path, all_varchar = TRUE) {
  all_varchar_sql <- if (isTRUE(all_varchar)) "true" else "false"

  names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto(%s, all_varchar = %s) LIMIT 0",
      cache_build_quote_path(con, path),
      all_varchar_sql
    )
  ))
}

cache_build_read_bts_csv_cols <- function(con, paths) {
  qpaths <- cache_build_quote_paths(con, paths)

  names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto([%s], union_by_name = true) LIMIT 0",
      qpaths
    )
  ))
}

cache_build_airport_seq_cols <- function(cols) {
  c(
    intersect(c("OriginAirportSeqID", "DestAirportSeqID"), cols),
    grep("^Div[0-9]+AirportSeqID$", cols, value = TRUE)
  )
}

cache_build_airport_id_cols <- function(cols) {
  c(
    intersect(c("OriginAirportID", "DestAirportID"), cols),
    grep("^Div[0-9]+AirportID$", cols, value = TRUE)
  )
}

cache_build_main_ap_id_cols <- function(cols) {
  intersect(c("OriginAirportID", "DestAirportID"), cols)
}

cache_build_div_ap_id_cols <- function(cols) {
  grep("^Div[0-9]+AirportID$", cols, value = TRUE)
}

cache_build_has_req_sort_cols <- function(cols) {
  all(c("FlightDate", "CRSDepTime") %in% cols)
}

cache_build_sql_ider_list <- function(con, cols) {
  if (!length(cols)) {
    stop("`cols` must contain at least one column name.", call. = FALSE)
  }

  paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")
}
