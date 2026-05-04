# Internal annual cache file builders ----------------------------------------
#
# These helpers build annual cache Parquet files from downloaded monthly BTS
# CSV files. They preserve the package's Year=YYYY/data_0_*.parquet layout.
#
# They do not download data, reduce coordinates, enrich coordinates, write a
# manifest, or expose user-facing update behavior.

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
  cols <- cache_build_read_bts_csv_cols(con, csv_files)

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
