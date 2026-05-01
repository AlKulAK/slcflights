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
