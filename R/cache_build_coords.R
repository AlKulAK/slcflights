# Internal cache coordinate reduction helpers --------------------------------
#
# These helpers reduce the raw BTS Master Coordinate CSV to only airport
# sequence IDs used by available flight Parquet files.
#
# They do not download data, enrich Parquet files, write a manifest, or expose
# user-facing update behavior.

cache_build_parquet_seq_cols <- function(con, path) {
  cols <- cache_build_read_parquet_cols(con, path)
  cache_build_airport_seq_cols(cols)
}

#' Find airport sequence ID columns used by Parquet files
#'
#' Reads the schemas of available flight Parquet files and finds the airport
#' sequence ID columns used for coordinate reduction.
#'
#' @param con DuckDB connection.
#' @param parquet_files Character vector of Parquet file paths.
#'
#' @returns
#' Character vector of airport sequence ID column names.
#'
#' @noRd
cache_build_used_seq_cols <- function(con, parquet_files) {
  if (!length(parquet_files)) {
    stop(
      "`parquet_files` must contain at least one Parquet file.",
      call. = FALSE
    )
  }

  seq_cols <- unique(unlist(
    lapply(
      parquet_files,
      function(path) {
        cache_build_parquet_seq_cols(con, path)
      }
    ),
    use.names = FALSE
  ))

  if (!length(seq_cols)) {
    stop(
      "No AirportSeqID columns were found for coordinate reduction.",
      call. = FALSE
    )
  }

  seq_cols
}

cache_build_seq_id_union_sql <- function(con, parquet_files, seq_cols) {
  if (!length(seq_cols)) {
    stop("`seq_cols` must contain at least one column name.", call. = FALSE)
  }

  qfiles <- cache_build_quote_paths(con, parquet_files)

  paste(
    vapply(
      seq_cols,
      function(col) {
        sprintf(
          "
          SELECT CAST(%s AS BIGINT) AS AIRPORT_SEQ_ID
          FROM read_parquet([%s], union_by_name = true)
          WHERE %s IS NOT NULL
          ",
          DBI::dbQuoteIdentifier(con, col),
          qfiles,
          DBI::dbQuoteIdentifier(con, col)
        )
      },
      character(1)
    ),
    collapse = "\nUNION ALL\n"
  )
}

cache_build_vldte_coords_cols <- function(cols) {
  if (!("AIRPORT_SEQ_ID" %in% cols)) {
    stop(
      "T_MASTER_CORD.csv does not contain AIRPORT_SEQ_ID.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Build the reduced coordinate CSV for a cache
#'
#' Reduces the raw BTS Master Coordinate CSV to rows whose airport sequence IDs
#' are referenced by the supplied flight Parquet files.
#'
#' @param parquet_files Character vector of main and diversion-only Parquet
#'   files used to identify referenced airport sequence IDs.
#' @param coords_in Path to the raw BTS Master Coordinate CSV.
#' @param coords_out Destination path for the reduced coordinate CSV.
#' @param con Optional DuckDB connection. When `NULL`, a temporary connection
#'   is opened and closed by this function.
#'
#' @returns
#' Invisibly, the normalized path to the reduced coordinate CSV.
#'
#' @noRd
cache_build_reduce_coords_csv <- function(
  parquet_files,
  coords_in = slc_cache_raw_coords_path(create = FALSE),
  coords_out = slc_cache_coords_path(),
  con = NULL
) {
  if (!length(parquet_files)) {
    stop(
      "`parquet_files` must contain at least one Parquet file.",
      call. = FALSE
    )
  }

  if (!file.exists(coords_in)) {
    stop(
      sprintf("Coordinate CSV not found: %s", coords_in),
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  seq_cols <- cache_build_used_seq_cols(
    con = con,
    parquet_files = parquet_files
  )

  coord_cols <- cache_build_read_csv_cols(
    con = con,
    path = coords_in,
    all_varchar = TRUE
  )

  cache_build_vldte_coords_cols(coord_cols)

  union_sql <- cache_build_seq_id_union_sql(
    con = con,
    parquet_files = parquet_files,
    seq_cols = seq_cols
  )

  dir.create(dirname(coords_out), recursive = TRUE, showWarnings = FALSE)

  DBI::dbExecute(
    con,
    sprintf(
      "
      COPY (
        WITH used_seq_ids AS (
          SELECT DISTINCT AIRPORT_SEQ_ID
          FROM (
            %s
          )
        ),
        coords_with_rownum AS (
          SELECT
            row_number() OVER () AS csv_row_num,
            *
          FROM read_csv_auto(%s, all_varchar = true)
        )
        SELECT * EXCLUDE (csv_row_num)
        FROM coords_with_rownum
        WHERE CAST(AIRPORT_SEQ_ID AS BIGINT) IN (
          SELECT AIRPORT_SEQ_ID
          FROM used_seq_ids
        )
        ORDER BY csv_row_num
      )
      TO %s
      (HEADER, DELIMITER ',')
      ",
      union_sql,
      cache_build_quote_path(con, coords_in),
      DBI::dbQuoteString(
        con,
        normalizePath(coords_out, winslash = "/", mustWork = FALSE)
      )
    )
  )

  bts_validate_csv_file(
    coords_out,
    label = "Reduced BTS Master Coordinate CSV file"
  )
}

#' List Parquet files available for coordinate reduction
#'
#' Lists cached Parquet files for the requested years, optionally combined
#' with installed package Parquet files. The combined file set is used to
#' reduce the coordinate table to all airports referenced by installed and
#' cached data.
#'
#' @param years Integer vector of years to inspect.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#' @param include_installed If `TRUE`, include installed package Parquet files
#'   in addition to cached Parquet files.
#'
#' @returns
#' Character vector of existing Parquet file paths.
#'
#' @noRd
cache_build_avail_pq_files <- function(
  years,
  root = NULL,
  include_installed = TRUE
) {
  years <- normalize_data_years(years)

  cached <- rbind(
    slc_cached_data_paths("main", years = years, root = root),
    slc_cached_data_paths("div", years = years, root = root)
  )

  if (!isTRUE(include_installed)) {
    return(unname(cached$path))
  }

  installed <- rbind(
    slc_installed_data_paths("main", years = years),
    slc_installed_data_paths("div", years = years)
  )

  paths <- c(installed$path, cached$path)
  unname(paths[file.exists(paths)])
}
