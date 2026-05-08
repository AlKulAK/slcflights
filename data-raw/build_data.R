# Maintainer-only data build script.
#
# This script rebuilds the bundled Parquet files, reduced coordinate CSV, and
# field dictionary under inst/extdata/.
# Runtime user updates must not use this script and must not write to inst/.
#
# If data-raw/T_MASTER_CORD.csv is missing, build_slc_data() downloads it from
# the BTS TranStats Master Coordinate support table before reducing it into the
# packaged coordinate CSV.

years_default <- 1987:2024
slc_id_default <- 14869L
base_url_default <- "https://blobs.duckdb.org/flight-data-partitioned/"
build_dir_default <- tempfile("slcflights-build-")

safe_replace_file <- function(src_tmp, dest) {
  bak <- paste0(dest, ".bak")

  if (file.exists(bak)) unlink(bak)

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
    if (file.exists(bak)) file.rename(bak, dest)
    unlink(src_tmp)
    stop(
      sprintf("Failed to promote temp file into place: %s", dest),
      call. = FALSE
    )
  }

  if (file.exists(bak)) unlink(bak)
  invisible(TRUE)
}

safe_promote_pair <-
  function(path_original, tmp_main, out_main, tmp_div, out_div, wrote_div) {
    bak <- paste0(path_original, ".bak")

    if (file.exists(bak)) unlink(bak)

    ok1 <- file.rename(path_original, bak)
    if (!ok1) {
      if (file.exists(tmp_main)) unlink(tmp_main)
      if (wrote_div && file.exists(tmp_div)) unlink(tmp_div)
      stop(
        sprintf("Failed to rename original to backup: %s", path_original),
        call. = FALSE
      )
    }

    ok2 <- file.rename(tmp_main, out_main)
    if (!ok2) {
      file.rename(bak, path_original)
      if (file.exists(tmp_main)) unlink(tmp_main)
      if (wrote_div && file.exists(tmp_div)) unlink(tmp_div)
      stop(
        sprintf("Failed to promote main temp file into place: %s", out_main),
        call. = FALSE
      )
    }

    if (wrote_div) {
      ok3 <- file.rename(tmp_div, out_div)
      if (!ok3) {
        unlink(out_main)
        file.rename(bak, path_original)
        if (file.exists(tmp_div)) unlink(tmp_div)
        stop(
          sprintf("Failed to promote div temp file into place: %s", out_div),
          call. = FALSE
        )
      }
    } else {
      if (file.exists(out_div)) unlink(out_div)
      if (file.exists(tmp_div)) unlink(tmp_div)
    }

    unlink(bak)
    invisible(TRUE)
  }

year_files <- function(years = years_default, build_dir = build_dir_default) {
  file.path(build_dir, paste0("Year=", years), "data_0.parquet")
}

year_main_files <-
  function(years = years_default, build_dir = build_dir_default) {
    file.path(build_dir, paste0("Year=", years), "data_0_main.parquet")
  }

year_div_files <-
  function(years = years_default, build_dir = build_dir_default) {
    file.path(build_dir, paste0("Year=", years), "data_0_div.parquet")
  }

open_con <- function() {
  DBI::dbConnect(duckdb::duckdb())
}

download_full_collection <- function(
  years = years_default,
  base_url = base_url_default,
  build_dir = build_dir_default
) {
  rel_files <- paste0("Year=", years, "/data_0.parquet")
  out_files <- year_files(years, build_dir = build_dir)

  for (dir in dirname(out_files)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  curl::multi_download(
    urls = paste0(base_url, rel_files),
    destfiles = out_files,
    resume = TRUE
  )

  invisible(out_files)
}

globally_non_null_columns <- function(files, con) {
  qfiles <- paste(
    DBI::dbQuoteString(
      con,
      normalizePath(files, winslash = "/", mustWork = TRUE)
    ),
    collapse = ", "
  )

  DBI::dbGetQuery(
    con,
    sprintf(
      "
      WITH md AS (
        SELECT
          file_name,
          path_in_schema,
          min(column_id) AS column_id,
          sum(row_group_num_rows) AS rows_total,
          sum(stats_null_count) AS nulls_total,
          count(*) AS row_groups,
          count(stats_null_count) AS row_groups_with_null_stats
        FROM parquet_metadata([%s])
        GROUP BY file_name, path_in_schema
      ),
      per_col AS (
        SELECT
          path_in_schema,
          min(column_id) AS column_id,
          bool_and(
            row_groups_with_null_stats = row_groups
            AND nulls_total = rows_total
          ) AS all_null_globally
        FROM md
        GROUP BY path_in_schema
      )
      SELECT path_in_schema
      FROM per_col
      WHERE NOT all_null_globally
      ORDER BY column_id
      ",
      qfiles
    )
  )[[1]]
}

rewrite_with_keep <- function(path, keep, con) {
  qpath <- DBI::dbQuoteString(
    con,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
  tmp <- paste0(path, ".tmp")
  sel <- paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", ")

  sql <- sprintf(
    "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
    )
    TO %s
    (FORMAT parquet)
    ",
    sel,
    qpath,
    DBI::dbQuoteString(con, tmp)
  )

  DBI::dbExecute(con, sql)
  safe_replace_file(tmp, path)
  TRUE
}

pass_keep_non_null_columns <- function(files, con) {
  keep <- globally_non_null_columns(files, con)
  if (!length(keep)) {
    stop("No columns survived global all-NULL filtering", call. = FALSE)
  }
  vapply(files, rewrite_with_keep, logical(1), keep = keep, con = con)
}

rewrite_with_slc_only <- function(path, slc_id, con) {
  qpath <- DBI::dbQuoteString(
    con,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
  tmp <- paste0(path, ".tmp")

  cols <- names(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet(%s) LIMIT 0", qpath)
  ))

  if (!length(cols)) {
    stop(
      sprintf("No readable columns found in file: %s", path),
      call. = FALSE
    )
  }

  slc_cols <- c(
    intersect(c("OriginAirportID", "DestAirportID"), cols),
    grep("^Div[0-9]+AirportID$", cols, value = TRUE)
  )

  if (!length(slc_cols)) {
    stop(
      sprintf("No Salt Lake City airport ID columns found in file: %s", path),
      call. = FALSE
    )
  }

  sel <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")

  where_slc <- paste(
    sprintf(
      "%s = %s",
      DBI::dbQuoteIdentifier(con, slc_cols),
      as.integer(slc_id)
    ),
    collapse = " OR "
  )

  sql <- sprintf(
    "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
      WHERE %s
    )
    TO %s
    (FORMAT parquet)
    ",
    sel,
    qpath,
    where_slc,
    DBI::dbQuoteString(con, tmp)
  )

  DBI::dbExecute(con, sql)
  safe_replace_file(tmp, path)
  TRUE
}

pass_keep_slc_rows <- function(files, slc_id, con) {
  vapply(
    files, rewrite_with_slc_only, logical(1),
    slc_id = slc_id, con = con
  )
}

rewrite_sorted <- function(path, con) {
  qpath <- DBI::dbQuoteString(
    con,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
  tmp <- paste0(path, ".tmp")

  cols <- names(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet(%s) LIMIT 0", qpath)
  ))

  if (!length(cols)) {
    stop(
      sprintf("No readable columns found in file: %s", path),
      call. = FALSE
    )
  }

  if (!("FlightDate" %in% cols)) {
    stop("Missing required column for sorting: FlightDate", call. = FALSE)
  }

  sel <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")

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

  sql <- sprintf(
    "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
      ORDER BY
        %s
    )
    TO %s
    (FORMAT parquet)
    ",
    sel,
    qpath,
    paste(order_terms, collapse = ",\n        "),
    DBI::dbQuoteString(con, tmp)
  )

  DBI::dbExecute(con, sql)
  safe_replace_file(tmp, path)
  TRUE
}

pass_sort_rows <- function(files, con) {
  existing_cols <- names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet(%s) LIMIT 0",
      DBI::dbQuoteString(
        con,
        normalizePath(files[[1]], winslash = "/", mustWork = TRUE)
      )
    )
  ))

  needed_cols <- "FlightDate"
  missing_cols <- setdiff(needed_cols, existing_cols)

  if (length(missing_cols)) {
    stop(
      sprintf(
        "Missing required columns for sorting: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  vapply(files, rewrite_sorted, logical(1), con = con)
}

rewrite_split_slc <- function(path, slc_id, con) {
  qpath <- DBI::dbQuoteString(
    con,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )

  out_main <- sub("\\.parquet$", "_main.parquet", path)
  out_div <- sub("\\.parquet$", "_div.parquet", path)
  tmp_main <- paste0(out_main, ".tmp")
  tmp_div <- paste0(out_div, ".tmp")

  cols <- names(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet(%s) LIMIT 0", qpath)
  ))

  if (!length(cols)) {
    stop(
      sprintf("No readable columns found in file: %s", path),
      call. = FALSE
    )
  }

  main_cols <- intersect(c("OriginAirportID", "DestAirportID"), cols)
  div_cols <- grep("^Div[0-9]+AirportID$", cols, value = TRUE)

  if (!length(main_cols)) {
    stop(
      sprintf("No Origin/Dest airport ID columns found in file: %s", path),
      call. = FALSE
    )
  }

  if (!length(div_cols)) {
    stop(
      sprintf("No diversion airport ID columns found in file: %s", path),
      call. = FALSE
    )
  }

  sel <- paste(DBI::dbQuoteIdentifier(con, cols), collapse = ", ")

  where_main <- paste(
    sprintf(
      "%s = %s",
      DBI::dbQuoteIdentifier(con, main_cols),
      as.integer(slc_id)
    ),
    collapse = " OR "
  )

  where_div_only <- paste(
    sprintf(
      "%s = %s",
      DBI::dbQuoteIdentifier(con, div_cols),
      as.integer(slc_id)
    ),
    collapse = " OR "
  )

  sql_main <- sprintf(
    "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
      WHERE %s
    )
    TO %s
    (FORMAT parquet)
    ",
    sel,
    qpath,
    where_main,
    DBI::dbQuoteString(con, tmp_main)
  )

  div_n <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT count(*) AS n
      FROM read_parquet(%s)
      WHERE (%s)
        AND NOT (%s)
      ",
      qpath,
      where_div_only,
      where_main
    )
  )[[1]]

  DBI::dbExecute(con, sql_main)

  wrote_div <- FALSE
  if (div_n > 0) {
    sql_div <- sprintf(
      "
      COPY (
        SELECT %s
        FROM read_parquet(%s)
        WHERE (%s)
          AND NOT (%s)
      )
      TO %s
      (FORMAT parquet)
      ",
      sel,
      qpath,
      where_div_only,
      where_main,
      DBI::dbQuoteString(con, tmp_div)
    )
    DBI::dbExecute(con, sql_div)
    wrote_div <- TRUE
  }

  safe_promote_pair(
    path_original = path,
    tmp_main = tmp_main,
    out_main = out_main,
    tmp_div = tmp_div,
    out_div = out_div,
    wrote_div = wrote_div
  )

  TRUE
}

pass_split_main_vs_diversion <- function(files, slc_id, con) {
  vapply(files, rewrite_split_slc, logical(1), slc_id = slc_id, con = con)
}

rewrite_used_seqid_coords <-
  function(path_in, path_out, parquet_files, seq_cols, con) {
    qpath_in <- DBI::dbQuoteString(
      con,
      normalizePath(path_in, winslash = "/", mustWork = TRUE)
    )
    qfiles <- paste(
      DBI::dbQuoteString(
        con,
        normalizePath(parquet_files, winslash = "/", mustWork = TRUE)
      ),
      collapse = ", "
    )

    tmp <- paste0(path_out, ".tmp")

    union_sql <- paste(
      vapply(
        seq_cols,
        function(col) {
          sprintf(
            "
          SELECT CAST(%s AS BIGINT) AS AIRPORT_SEQ_ID
          FROM read_parquet([%s])
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

    sql <- sprintf(
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
        FROM read_csv_auto(%s, all_varchar = TRUE)
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
      qpath_in,
      DBI::dbQuoteString(con, tmp)
    )

    DBI::dbExecute(con, sql)
    safe_replace_file(tmp, path_out)
    TRUE
  }

pass_reduce_coords_csv <- function(files, coords_in, coords_out, con) {
  if (!length(files)) {
    stop(
      "No final parquet files found for coordinate reduction",
      call. = FALSE
    )
  }

  if (!file.exists(coords_in)) {
    stop(sprintf("Coordinate CSV not found: %s", coords_in), call. = FALSE)
  }

  seq_cols <- unique(unlist(lapply(files, function(path) {
    cols <- names(DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT * FROM read_parquet(%s) LIMIT 0",
        DBI::dbQuoteString(
          con,
          normalizePath(path, winslash = "/", mustWork = TRUE)
        )
      )
    ))

    c(
      intersect(c("OriginAirportSeqID", "DestAirportSeqID"), cols),
      grep("^Div[0-9]+AirportSeqID$", cols, value = TRUE)
    )
  }), use.names = FALSE))

  if (!length(seq_cols)) {
    stop("No AirportSeqID columns found after split", call. = FALSE)
  }

  coord_cols <- names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto(%s, all_varchar = TRUE) LIMIT 0",
      DBI::dbQuoteString(
        con,
        normalizePath(coords_in, winslash = "/", mustWork = TRUE)
      )
    )
  ))

  if (!("AIRPORT_SEQ_ID" %in% coord_cols)) {
    stop("T_MASTER_CORD.csv does not contain AIRPORT_SEQ_ID", call. = FALSE)
  }

  rewrite_used_seqid_coords(
    path_in = coords_in,
    path_out = coords_out,
    parquet_files = files,
    seq_cols = seq_cols,
    con = con
  )
}

rewrite_with_coords <- function(path, coords_in, con) {
  qpath <- DBI::dbQuoteString(
    con,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
  tmp <- paste0(path, ".tmp")

  coord_cols <- names(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto(%s, all_varchar = TRUE) LIMIT 0",
      DBI::dbQuoteString(
        con,
        normalizePath(coords_in, winslash = "/", mustWork = TRUE)
      )
    )
  ))

  needed_coord_cols <- c(
    "AIRPORT_SEQ_ID",
    "LATITUDE",
    "LONGITUDE",
    "AIRPORT_START_DATE",
    "AIRPORT_THRU_DATE",
    "AIRPORT_IS_CLOSED",
    "AIRPORT_IS_LATEST"
  )

  missing_coord_cols <- setdiff(needed_coord_cols, coord_cols)

  if (length(missing_coord_cols)) {
    stop(
      sprintf(
        "Reduced coordinate CSV is missing required columns: %s",
        paste(missing_coord_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

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
      FROM read_csv_auto(%s, all_varchar = TRUE)
      ",
      DBI::dbQuoteString(
        con,
        normalizePath(coords_in, winslash = "/", mustWork = TRUE)
      )
    )
  )

  cols <- names(DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM read_parquet(%s) LIMIT 0", qpath)
  ))

  if (!length(cols)) {
    stop(
      sprintf("No readable columns found in file: %s", path),
      call. = FALSE
    )
  }

  seq_cols <- c(
    intersect(c("OriginAirportSeqID", "DestAirportSeqID"), cols),
    grep("^Div[0-9]+AirportSeqID$", cols, value = TRUE)
  )

  if (!length(seq_cols)) {
    stop(
      sprintf("No AirportSeqID columns found in file: %s", path),
      call. = FALSE
    )
  }

  extra_cols <- unlist(
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

  cols_base <- cols[!(cols %in% extra_cols)]

  role_alias <- setNames(
    paste0("c", seq_along(seq_cols)),
    seq_cols
  )

  join_sql <- paste(
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

  select_terms <- character(0)

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

  sel <- paste(select_terms, collapse = ",\n        ")

  sql <- sprintf(
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
    qpath,
    join_sql,
    DBI::dbQuoteString(con, tmp)
  )

  DBI::dbExecute(con, sql)
  safe_replace_file(tmp, path)
  TRUE
}

pass_enrich_with_coords <- function(files, coords_in, con) {
  if (!length(files)) {
    stop(
      "No final parquet files found for coordinate enrichment",
      call. = FALSE
    )
  }

  if (!file.exists(coords_in)) {
    stop(
      sprintf("Reduced coordinate CSV not found: %s", coords_in),
      call. = FALSE
    )
  }

  vapply(
    files, rewrite_with_coords, logical(1),
    coords_in = coords_in, con = con
  )
}

copy_outputs <- function(
  years = years_default,
  coords_out,
  build_dir,
  output_root = file.path("inst", "extdata")
) {
  main_files <- year_main_files(years, build_dir = build_dir)
  div_files <- year_div_files(years, build_dir = build_dir)

  missing_main <- main_files[!file.exists(main_files)]
  if (length(missing_main)) {
    stop(
      sprintf(
        "Expected main Parquet file was not built: %s",
        missing_main[[1]]
      ),
      call. = FALSE
    )
  }

  if (!file.exists(coords_out)) {
    stop(
      sprintf("Reduced coordinate CSV was not built: %s", coords_out),
      call. = FALSE
    )
  }

  for (yr in years) {
    target_year_dir <- file.path(
      output_root,
      "parquet",
      sprintf("Year=%s", yr)
    )

    if (dir.exists(target_year_dir)) {
      unlink(target_year_dir, recursive = TRUE, force = TRUE)
    }

    dir.create(
      target_year_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  for (path in main_files) {
    yr_dir <- basename(dirname(path))
    target <- file.path(output_root, "parquet", yr_dir, basename(path))

    ok <- file.copy(path, target, overwrite = TRUE)
    if (!ok) {
      stop(
        sprintf("Failed to copy file into output root: %s", target),
        call. = FALSE
      )
    }
  }

  for (path in div_files[file.exists(div_files)]) {
    yr_dir <- basename(dirname(path))
    target <- file.path(output_root, "parquet", yr_dir, basename(path))

    ok <- file.copy(path, target, overwrite = TRUE)
    if (!ok) {
      stop(
        sprintf("Failed to copy file into output root: %s", target),
        call. = FALSE
      )
    }
  }

  csv_dir <- file.path(output_root, "csv")

  if (dir.exists(csv_dir)) {
    unlink(csv_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(
    csv_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  coords_target <- file.path(csv_dir, basename(coords_out))

  ok <- file.copy(
    coords_out,
    coords_target,
    overwrite = TRUE
  )
  if (!ok) {
    stop(
      sprintf(
        "Failed to copy coordinate CSV into output root: %s",
        coords_target
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

clean_build_cache <- function(build_dir = build_dir_default) {
  if (dir.exists(build_dir)) {
    unlink(build_dir, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

ensure_master_coords_csv <- function(coords_in) {
  if (file.exists(coords_in)) {
    return(normalizePath(coords_in, winslash = "/", mustWork = TRUE))
  }

  dir.create(dirname(coords_in), recursive = TRUE, showWarnings = FALSE)

  if (!exists("download_bts_master_coords", mode = "function")) {
    source(file.path("R", "bts_download.R"))
  }

  download_bts_master_coords(
    destfile = coords_in,
    overwrite = FALSE
  )
}

build_slc_data <- function(
  years = years_default,
  slc_id = slc_id_default,
  base_url = base_url_default,
  coords_in = file.path("data-raw", "T_MASTER_CORD.csv"),
  build_dir = build_dir_default,
  output_root = file.path("inst", "extdata"),
  coords_out = file.path(build_dir, "T_MASTER_CORD_reduced.csv")
) {
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(clean_build_cache(build_dir), add = TRUE)

  files <- year_files(years, build_dir = build_dir)
  coords_in <- ensure_master_coords_csv(coords_in)

  download_full_collection(
    years = years,
    base_url = base_url,
    build_dir = build_dir
  )

  con <- open_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  pass_keep_non_null_columns(files, con)
  pass_keep_slc_rows(files, slc_id, con)
  pass_keep_non_null_columns(files, con)
  pass_sort_rows(files, con)
  pass_split_main_vs_diversion(files, slc_id, con)

  final_files <- c(
    year_main_files(years, build_dir = build_dir),
    year_div_files(years, build_dir = build_dir)
  )
  final_files <- final_files[file.exists(final_files)]

  pass_reduce_coords_csv(final_files, coords_in, coords_out, con)
  pass_enrich_with_coords(final_files, coords_out, con)

  # Coordinate enrichment rewrites the final Parquet files. Sort again after
  # enrichment so reader-facing files are chronologically ordered.
  pass_sort_rows(final_files, con)

  copy_outputs(
    years = years,
    coords_out = coords_out,
    build_dir = build_dir,
    output_root = output_root
  )

  if (!exists("build_field_dictionary", mode = "function")) {
    source(file.path("data-raw", "build_field_dictionary.R"))
  }

  build_field_dictionary_fun <- get(
    "build_field_dictionary",
    mode = "function"
  )

  build_field_dictionary_fun(
    output = file.path(output_root, "csv", "field_dictionary.csv")
  )

  invisible(TRUE)
}
