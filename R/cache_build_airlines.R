# Internal cache Airline ID reduction helpers --------------------------------
#
# These helpers reduce the raw BTS Airline ID lookup CSV to only DOT reporting
# airline IDs used by available flight Parquet files.
#
# They do not download data, enrich Parquet files, write a manifest, or expose
# user-facing update behavior.

cache_build_pq_airline_cols <- function(con, path) {
  cols <- cache_build_read_parquet_cols(con, path)
  intersect("DOT_ID_Reporting_Airline", cols)
}

#' Find reporting airline ID columns used by Parquet files
#'
#' Reads the schemas of available flight Parquet files and finds the reporting
#' airline ID column used for Airline ID lookup reduction.
#'
#' @param con DuckDB connection.
#' @param parquet_files Character vector of Parquet file paths.
#'
#' @returns
#' Character vector of reporting airline ID column names.
#'
#' @noRd
cache_build_used_airline_cols <- function(con, parquet_files) {
  if (!length(parquet_files)) {
    stop(
      "`parquet_files` must contain at least one Parquet file.",
      call. = FALSE
    )
  }

  airline_cols <- unique(unlist(
    lapply(
      parquet_files,
      function(path) {
        cache_build_pq_airline_cols(con, path)
      }
    ),
    use.names = FALSE
  ))

  if (!length(airline_cols)) {
    stop(
      paste(
        "No DOT_ID_Reporting_Airline columns were found for",
        "Airline ID reduction."
      ),
      call. = FALSE
    )
  }

  airline_cols
}

cache_build_airline_union_sql <- function(
  con,
  parquet_files,
  airline_cols
) {
  if (!length(airline_cols)) {
    stop(
      "`airline_cols` must contain at least one column name.",
      call. = FALSE
    )
  }

  qfiles <- cache_build_quote_paths(con, parquet_files)

  paste(
    vapply(
      airline_cols,
      function(col) {
        sprintf(
          "
          SELECT CAST(%s AS BIGINT) AS DOT_ID_Reporting_Airline
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

cache_build_vldte_airline_cols <- function(cols) {
  missing_cols <- setdiff(c("Code", "Description"), cols)

  if (length(missing_cols)) {
    stop(
      sprintf(
        "L_AIRLINE_ID.csv does not contain required column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Build the reduced Airline ID lookup CSV for a cache
#'
#' Reduces the raw BTS Airline ID lookup CSV to rows whose DOT reporting
#' airline IDs are referenced by the supplied flight Parquet files.
#'
#' @param parquet_files Character vector of main and diversion-only Parquet
#'   files used to identify referenced DOT reporting airline IDs.
#' @param airlines_in Path to the raw BTS Airline ID lookup CSV.
#' @param airlines_out Destination path for the reduced Airline ID lookup CSV.
#' @param con Optional DuckDB connection. When `NULL`, a temporary connection
#'   is opened and closed by this function.
#'
#' @returns
#' Invisibly, the normalized path to the reduced Airline ID lookup CSV.
#'
#' @noRd
cache_build_reduce_air_csv <- function(
  parquet_files,
  airlines_in = slc_cache_raw_airlines_path(create = FALSE),
  airlines_out = slc_cache_airlines_path(),
  con = NULL
) {
  if (!length(parquet_files)) {
    stop(
      "`parquet_files` must contain at least one Parquet file.",
      call. = FALSE
    )
  }

  if (!file.exists(airlines_in)) {
    stop(
      sprintf("Airline ID CSV not found: %s", airlines_in),
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  airline_cols <- cache_build_used_airline_cols(
    con = con,
    parquet_files = parquet_files
  )

  lookup_cols <- cache_build_read_csv_cols(
    con = con,
    path = airlines_in,
    all_varchar = TRUE
  )

  cache_build_vldte_airline_cols(lookup_cols)

  union_sql <- cache_build_airline_union_sql(
    con = con,
    parquet_files = parquet_files,
    airline_cols = airline_cols
  )

  lookup_code_re <- paste0(
    "^[^:]*:[[:space:]]*",
    "([^[:space:]]+(?:[[:space:]]*\\([0-9]+\\))?)"
  )

  dir.create(dirname(airlines_out), recursive = TRUE, showWarnings = FALSE)

  DBI::dbExecute(
    con,
    sprintf(
      "
      COPY (
        WITH used_airline_ids AS (
          SELECT DISTINCT DOT_ID_Reporting_Airline
          FROM (
            %s
          )
        ),
        airlines_with_rownum AS (
          SELECT
            row_number() OVER () AS csv_row_num,
            CAST(Code AS BIGINT) AS DOT_ID_Reporting_Airline,
            trim(
              regexp_extract(
                Description,
                '^(.*):[[:space:]]*(.*)$',
                1
              )
            ) AS Reporting_Airline_Name,
            trim(
              regexp_extract(
                Description,
                %s,
                1
              )
            ) AS Reporting_Airline_Lookup_Code
          FROM read_csv_auto(%s, all_varchar = true)
        )
        SELECT
          DOT_ID_Reporting_Airline,
          Reporting_Airline_Name,
          Reporting_Airline_Lookup_Code
        FROM airlines_with_rownum
        WHERE DOT_ID_Reporting_Airline IN (
          SELECT DOT_ID_Reporting_Airline
          FROM used_airline_ids
        )
        ORDER BY csv_row_num
      )
      TO %s
      (HEADER, DELIMITER ',')
      ",
      union_sql,
      DBI::dbQuoteString(con, lookup_code_re),
      cache_build_quote_path(con, airlines_in),
      DBI::dbQuoteString(
        con,
        normalizePath(airlines_out, winslash = "/", mustWork = FALSE)
      )
    )
  )

  bts_validate_csv_file(
    airlines_out,
    label = "Reduced BTS Airline ID CSV file"
  )
}
