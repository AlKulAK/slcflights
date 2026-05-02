# Internal cache coordinate enrichment helpers --------------------------------
#
# These helpers enrich cached annual Parquet files with airport coordinate
# metadata from the reduced BTS Master Coordinate CSV.
#
# They do not download data, reduce coordinates, write a manifest, or expose
# user-facing update behavior.

cache_build_req_coord_cols <- function() {
  c(
    "AIRPORT_SEQ_ID",
    "LATITUDE",
    "LONGITUDE",
    "AIRPORT_START_DATE",
    "AIRPORT_THRU_DATE",
    "AIRPORT_IS_CLOSED",
    "AIRPORT_IS_LATEST"
  )
}

cache_build_val_enrich_cols <- function(cols) {
  missing <- setdiff(cache_build_req_coord_cols(), cols)

  if (length(missing)) {
    stop(
      sprintf(
        "Reduced coordinate CSV is missing required columns: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

cache_build_coord_extra_cols <- function(seq_cols) {
  unlist(
    lapply(
      sub("AirportSeqID$", "", seq_cols),
      function(prefix) {
        c(
          paste0(prefix, "Latitude"),
          paste0(prefix, "Longitude"),
          paste0(prefix, "AirportStartDate"),
          paste0(prefix, "AirportThruDate"),
          paste0(prefix, "AirportIsClosed"),
          paste0(prefix, "AirportIsLatest")
        )
      }
    ),
    use.names = FALSE
  )
}

cache_build_create_coord_tbl <- function(con, coords_in) {
  coord_cols <- cache_build_read_csv_cols(
    con = con,
    path = coords_in,
    all_varchar = TRUE
  )

  cache_build_val_enrich_cols(coord_cols)

  DBI::dbExecute(con, "DROP TABLE IF EXISTS coords")

  DBI::dbExecute(
    con,
    sprintf(
      "
      CREATE TEMP TABLE coords AS
      SELECT
        CAST(AIRPORT_SEQ_ID AS BIGINT) AS AIRPORT_SEQ_ID,
        TRY_CAST(LATITUDE AS DOUBLE) AS LATITUDE,
        TRY_CAST(LONGITUDE AS DOUBLE) AS LONGITUDE,
        CAST(
          TRY_STRPTIME(
            AIRPORT_START_DATE,
            '%%m/%%d/%%Y %%I:%%M:%%S %%p'
          ) AS DATE
        ) AS AIRPORT_START_DATE,
        CAST(
          TRY_STRPTIME(
            AIRPORT_THRU_DATE,
            '%%m/%%d/%%Y %%I:%%M:%%S %%p'
          ) AS DATE
        ) AS AIRPORT_THRU_DATE,
        TRY_CAST(AIRPORT_IS_CLOSED AS INTEGER) AS AIRPORT_IS_CLOSED,
        TRY_CAST(AIRPORT_IS_LATEST AS INTEGER) AS AIRPORT_IS_LATEST
      FROM read_csv_auto(%s, all_varchar = true)
      ",
      cache_build_quote_path(con, coords_in)
    )
  )

  invisible(TRUE)
}

cache_build_coord_join_sql <- function(con, seq_cols) {
  role_alias <- stats::setNames(
    paste0("c", seq_along(seq_cols)),
    seq_cols
  )

  paste(
    vapply(
      seq_cols,
      function(col) {
        sprintf(
          "LEFT JOIN coords %s ON CAST(f.%s AS BIGINT) = %s.AIRPORT_SEQ_ID",
          role_alias[[col]],
          DBI::dbQuoteIdentifier(con, col),
          role_alias[[col]]
        )
      },
      character(1)
    ),
    collapse = "\n      "
  )
}

cache_build_coord_sel_terms <- function(con, cols, seq_cols) {
  extra_cols <- cache_build_coord_extra_cols(seq_cols)
  cols_base <- cols[!(cols %in% extra_cols)]

  role_alias <- stats::setNames(
    paste0("c", seq_along(seq_cols)),
    seq_cols
  )

  select_terms <- character()

  for (col in cols_base) {
    select_terms <- c(
      select_terms,
      sprintf("f.%s", DBI::dbQuoteIdentifier(con, col))
    )

    if (col %in% seq_cols) {
      prefix <- sub("AirportSeqID$", "", col)
      alias <- role_alias[[col]]

      select_terms <- c(
        select_terms,
        sprintf(
          "%s.LATITUDE AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "Latitude"))
        ),
        sprintf(
          "%s.LONGITUDE AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "Longitude"))
        ),
        sprintf(
          "%s.AIRPORT_START_DATE AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "AirportStartDate"))
        ),
        sprintf(
          "%s.AIRPORT_THRU_DATE AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "AirportThruDate"))
        ),
        sprintf(
          "%s.AIRPORT_IS_CLOSED AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "AirportIsClosed"))
        ),
        sprintf(
          "%s.AIRPORT_IS_LATEST AS %s",
          alias,
          DBI::dbQuoteIdentifier(con, paste0(prefix, "AirportIsLatest"))
        )
      )
    }
  }

  select_terms
}

cache_build_replace_file <- function(src_tmp, dest) {
  bak <- paste0(dest, ".bak")

  if (file.exists(bak)) {
    unlink(bak)
  }

  if (file.exists(dest)) {
    ok1 <- file.rename(dest, bak)
    if (!ok1) {
      unlink(src_tmp)
      stop(
        sprintf("Failed to rename original to backup: %s", dest),
        call. = FALSE
      )
    }
  }

  ok2 <- file.rename(src_tmp, dest)
  if (!ok2) {
    if (file.exists(bak)) {
      file.rename(bak, dest)
    }

    unlink(src_tmp)

    stop(
      sprintf("Failed to promote temp file into place: %s", dest),
      call. = FALSE
    )
  }

  if (file.exists(bak)) {
    unlink(bak)
  }

  invisible(TRUE)
}

cache_build_enrich_file <- function(path, coords_in, con = NULL) {
  if (!file.exists(path)) {
    stop(
      sprintf("Parquet file not found: %s", path),
      call. = FALSE
    )
  }

  if (!file.exists(coords_in)) {
    stop(
      sprintf("Reduced coordinate CSV not found: %s", coords_in),
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  cache_build_create_coord_tbl(con, coords_in)

  cols <- cache_build_read_parquet_cols(con, path)

  if (!length(cols)) {
    stop(
      sprintf("No readable columns found in file: %s", path),
      call. = FALSE
    )
  }

  seq_cols <- cache_build_airport_seq_cols(cols)

  if (!length(seq_cols)) {
    stop(
      sprintf("No AirportSeqID columns found in file: %s", path),
      call. = FALSE
    )
  }

  join_sql <- cache_build_coord_join_sql(con, seq_cols)
  select_terms <- cache_build_coord_sel_terms(con, cols, seq_cols)
  sel <- paste(select_terms, collapse = ",\n        ")

  tmp <- paste0(path, ".tmp")

  DBI::dbExecute(
    con,
    sprintf(
      "
      COPY (
        SELECT
          %s
        FROM read_parquet(%s) f
        %s
      )
      TO %s
      (FORMAT parquet)
      ",
      sel,
      cache_build_quote_path(con, path),
      join_sql,
      DBI::dbQuoteString(
        con,
        normalizePath(tmp, winslash = "/", mustWork = FALSE)
      )
    )
  )

  cache_build_replace_file(tmp, path)

  invisible(path)
}

cache_build_enrich_files <- function(parquet_files, coords_in, con = NULL) {
  if (!length(parquet_files)) {
    stop(
      "`parquet_files` must contain at least one Parquet file.",
      call. = FALSE
    )
  }

  if (!file.exists(coords_in)) {
    stop(
      sprintf("Reduced coordinate CSV not found: %s", coords_in),
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  out <- vapply(
    parquet_files,
    cache_build_enrich_file,
    character(1),
    coords_in = coords_in,
    con = con
  )

  unname(out)
}
