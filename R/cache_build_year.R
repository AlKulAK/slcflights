# Internal annual cache file builders ----------------------------------------
#
# These helpers build annual cache Parquet files from downloaded monthly BTS
# CSV files. They preserve the package's Year=YYYY/data_0_*.parquet layout.
#
# They do not download data, reduce coordinates, enrich coordinates, write a
# manifest, or expose user-facing update behavior.

#' Build a DuckDB CSV relation for monthly BTS files
#'
#' Creates the DuckDB `read_csv_auto()` relation used to read one or more
#' monthly BTS CSV files with name-based column union.
#'
#' @param con DuckDB connection.
#' @param csv_files Character vector of monthly BTS CSV paths.
#'
#' @returns
#' Character SQL relation expression.
#'
#' @noRd
cache_build_csv_relation <- function(con, csv_files) {
  if (!length(csv_files)) {
    stop("`csv_files` must contain at least one CSV file.", call. = FALSE)
  }

  qfiles <- cache_build_quote_paths(con, csv_files)

  sprintf(
    "read_csv_auto([%s], union_by_name = true)",
    qfiles
  )
}

#' Keep valid BTS CSV columns
#'
#' @param cols Character vector of column names returned by DuckDB.
#'
#' @returns Character vector of valid BTS column names.
#'
#' @noRd
cache_build_bts_cols <- function(cols) {
  cols <- as.character(cols)

  cols <- cols[nzchar(cols)]

  cols[!grepl("^column[0-9]+$", cols)]
}

cache_build_order_clause <- function(cols, con) {
  if (!("FlightDate" %in% cols)) {
    return("")
  }

  order_terms <- c("FlightDate")

  if ("CRSDepTime" %in% cols) {
    order_terms <- c(
      order_terms,
      "CRSDepTime IS NULL",
      "lpad(CAST(CRSDepTime AS VARCHAR), 4, '0')"
    )
  }

  tie_cols <- intersect(
    c(
      "OriginAirportID",
      "DestAirportID",
      "Reporting_Airline",
      "Flight_Number_Reporting_Airline",
      "OriginAirportSeqID",
      "DestAirportSeqID",
      "DOT_ID_Reporting_Airline"
    ),
    cols
  )

  order_terms <- c(
    order_terms,
    as.character(DBI::dbQuoteIdentifier(con, tie_cols))
  )

  sprintf(
    "ORDER BY %s",
    paste(order_terms, collapse = ", ")
  )
}

cache_build_any_equals_clause <- function(con, cols, value) {
  if (!length(cols)) {
    stop("`cols` must contain at least one column name.", call. = FALSE)
  }

  paste(
    sprintf(
      "%s = %s",
      DBI::dbQuoteIdentifier(con, cols),
      as.integer(value)
    ),
    collapse = " OR "
  )
}

#' Locate raw monthly BTS CSV files for a cached year
#'
#' Resolves the raw BTS On-Time Performance CSV files needed to build one
#' cached annual Parquet file set.
#'
#' @param year Integer-like calendar year.
#' @param months Integer vector of months to include for `year`.
#'
#' @returns
#' Character vector of raw BTS CSV paths.
#'
#' @noRd
cache_build_year_csvs <- function(year, months) {
  csvs <- cache_raw_ontime_csvs(
    year = year,
    months = months
  )

  if (!length(csvs)) {
    stop(
      sprintf(
        "No raw BTS on-time CSV files were found for %s.",
        normalize_cache_year(year)
      ),
      call. = FALSE
    )
  }

  csvs
}

#' Resolve cached annual Parquet output paths
#'
#' Resolves the main and diversion-only Parquet paths for one cached year.
#'
#' @param year Integer-like calendar year.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#' @param create If `TRUE`, create parent directories as needed.
#'
#' @returns
#' List with `main` and `div` path elements.
#'
#' @noRd
cache_build_year_file_paths <- function(year, root = NULL, create = TRUE) {
  list(
    main = slc_cache_parquet_path(
      "main",
      year,
      root = root,
      create = create
    ),
    div = slc_cache_parquet_path(
      "div",
      year,
      root = root,
      create = create
    )
  )
}

#' Write one cached annual main Parquet file
#'
#' Filters a BTS CSV relation to flights where Salt Lake City's airport ID
#' appears as the scheduled origin or destination and writes the result to a
#' cached annual main Parquet file.
#'
#' @param con DuckDB connection.
#' @param relation Character SQL relation expression for the source BTS CSVs.
#' @param cols Character vector of source column names.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param out_main Destination path for the main Parquet file.
#'
#' @returns
#' Invisibly, `out_main`.
#'
#' @noRd
cache_build_write_main_file <- function(
  con,
  relation,
  cols,
  slc_id,
  out_main
) {
  main_cols <- cache_build_main_ap_id_cols(cols)

  if (!length(main_cols)) {
    stop(
      "No Origin/Dest airport ID columns were found in the BTS CSV data.",
      call. = FALSE
    )
  }

  select_all <- cache_build_sql_ider_list(con, cols)

  where_main <- cache_build_any_equals_clause(
    con,
    main_cols,
    slc_id
  )

  order_clause <- cache_build_order_clause(cols, con)

  DBI::dbExecute(
    con,
    sprintf(
      "
      COPY (
        SELECT %s
        FROM %s
        WHERE %s
        %s
      )
      TO %s
      (FORMAT parquet)
      ",
      select_all,
      relation,
      where_main,
      order_clause,
      DBI::dbQuoteString(
        con,
        normalizePath(out_main, winslash = "/", mustWork = FALSE)
      )
    )
  )

  invisible(out_main)
}

cache_build_div_only_count <- function(
  con,
  relation,
  main_cols,
  div_cols,
  slc_id
) {
  if (!length(div_cols)) {
    return(0L)
  }

  where_main <- cache_build_any_equals_clause(
    con,
    main_cols,
    slc_id
  )

  where_div <- cache_build_any_equals_clause(
    con,
    div_cols,
    slc_id
  )

  out <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT count(*) AS n
      FROM %s
      WHERE (%s)
        AND NOT (%s)
      ",
      relation,
      where_div,
      where_main
    )
  )

  as.integer(out$n[[1]])
}

#' Write one cached annual diversion-only Parquet file
#'
#' Filters a BTS CSV relation to flights where Salt Lake City's airport ID
#' appears in a diversion airport field but not as the scheduled origin or
#' destination. If no diversion-only rows are present, no file is written.
#'
#' @param con DuckDB connection.
#' @param relation Character SQL relation expression for the source BTS CSVs.
#' @param cols Character vector of source column names.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param out_div Destination path for the diversion-only Parquet file.
#'
#' @returns
#' Invisibly, `out_div` when a diversion-only file is written; otherwise an
#' invisible empty character vector.
#'
#' @noRd
cache_build_write_div_file <- function(
  con,
  relation,
  cols,
  slc_id,
  out_div
) {
  main_cols <- cache_build_main_ap_id_cols(cols)
  div_cols <- cache_build_div_ap_id_cols(cols)

  if (!length(main_cols)) {
    stop(
      "No Origin/Dest airport ID columns were found in the BTS CSV data.",
      call. = FALSE
    )
  }

  div_n <- cache_build_div_only_count(
    con = con,
    relation = relation,
    main_cols = main_cols,
    div_cols = div_cols,
    slc_id = slc_id
  )

  if (div_n == 0L) {
    if (file.exists(out_div)) {
      unlink(out_div)
    }

    return(invisible(character()))
  }

  select_all <- cache_build_sql_ider_list(con, cols)

  where_main <- cache_build_any_equals_clause(
    con,
    main_cols,
    slc_id
  )

  where_div <- cache_build_any_equals_clause(
    con,
    div_cols,
    slc_id
  )

  order_clause <- cache_build_order_clause(cols, con)

  DBI::dbExecute(
    con,
    sprintf(
      "
      COPY (
        SELECT %s
        FROM %s
        WHERE (%s)
          AND NOT (%s)
        %s
      )
      TO %s
      (FORMAT parquet)
      ",
      select_all,
      relation,
      where_div,
      where_main,
      order_clause,
      DBI::dbQuoteString(
        con,
        normalizePath(out_div, winslash = "/", mustWork = FALSE)
      )
    )
  )

  invisible(out_div)
}

#' Build cached annual Parquet files
#'
#' Builds the main and diversion-only Parquet files for one cached year from
#' downloaded monthly BTS CSV files.
#'
#' @param year Integer-like calendar year.
#' @param months Integer vector of months to include for `year`.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param csv_files Optional character vector of raw monthly BTS CSV paths.
#'   When `NULL`, paths are resolved from the raw local cache.
#' @param con Optional DuckDB connection. When `NULL`,
#'   a temporary connection is opened and closed by this function.
#'
#' @returns
#' Character vector of cached Parquet files that were written.
#'
#' @noRd
cache_build_write_year_files <- function(
  year,
  months,
  root = NULL,
  slc_id = 14869L,
  csv_files = NULL,
  con = NULL
) {
  year <- normalize_cache_year(year)
  months <- normalize_cache_months(months)

  if (is.null(csv_files)) {
    csv_files <- cache_build_year_csvs(
      year = year,
      months = months
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  paths <- cache_build_year_file_paths(
    year = year,
    root = root,
    create = TRUE
  )

  relation <- cache_build_csv_relation(con, csv_files)
  cols <- cache_build_bts_cols(
    cache_build_read_bts_csv_cols(con, csv_files)
  )

  if (!length(cols)) {
    stop(
      sprintf(
        "No readable columns were found for cached BTS data in %s.", year
      ),
      call. = FALSE
    )
  }

  cache_build_write_main_file(
    con = con,
    relation = relation,
    cols = cols,
    slc_id = slc_id,
    out_main = paths$main
  )

  div_path <- cache_build_write_div_file(
    con = con,
    relation = relation,
    cols = cols,
    slc_id = slc_id,
    out_div = paths$div
  )

  out <- c(paths$main, div_path)
  out[file.exists(out)]
}
