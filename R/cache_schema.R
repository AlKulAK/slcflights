# Internal cache schema finalization helpers ---------------------------------
#
# These helpers finalize cached Parquet files after annual construction and
# enrichment. They preserve all columns already present in the cached files and
# rewrite each file in the package's canonical flight order.
#
# They do not download data, enrich coordinates, write manifests, or expose
# user-facing update behavior.

cache_schema_type_for_file <- function(path) {
  filename <- basename(path)

  if (identical(filename, "data_0_main.parquet")) {
    return("main")
  }

  if (identical(filename, "data_0_div.parquet")) {
    return("div")
  }

  stop(
    sprintf("Cannot determine cache data type from file name: %s", filename),
    call. = FALSE
  )
}

cache_schema_year_for_file <- function(path) {
  year_dir <- basename(dirname(path))

  if (!grepl("^Year=[0-9]{4}$", year_dir)) {
    stop(
      sprintf("Cannot determine cache year from path: %s", path),
      call. = FALSE
    )
  }

  as.integer(sub("^Year=", "", year_dir))
}

cache_schema_cols <- function(con, path) {
  cache_build_read_parquet_cols(con, path)
}

#' Finalize one cached Parquet file
#'
#' Rewrites one cached Parquet file in canonical flight order while preserving
#' all columns already present in the file.
#'
#' @param path Cached Parquet file to finalize.
#' @param con Optional DuckDB connection. When `NULL`, a temporary connection
#'   is opened and closed by this function.
#'
#' @returns
#' Invisibly, `path`.
#'
#' @noRd
cache_schema_align_file <- function(path, con = NULL) {
  if (!file.exists(path)) {
    stop(
      sprintf("Cached Parquet file not found: %s", path),
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  cache_schema_type_for_file(path)
  cache_schema_year_for_file(path)

  cols <- cache_schema_cols(con, path)
  sel <- cache_build_sql_ider_list(con, cols)
  order_clause <- cache_build_order_clause(cols, con)
  tmp <- paste0(path, ".tmp")

  DBI::dbExecute(
    con,
    sprintf(
      "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
      %s
    )
    TO %s
    (FORMAT parquet)
    ",
      sel,
      cache_build_quote_path(con, path),
      order_clause,
      DBI::dbQuoteString(
        con,
        normalizePath(tmp, winslash = "/", mustWork = FALSE)
      )
    )
  )

  cache_build_replace_file(tmp, path)

  invisible(path)
}

#' Finalize cached Parquet files
#'
#' Applies cache schema finalization to one or more cached Parquet files.
#'
#' @param files Character vector of cached Parquet files to finalize.
#' @param con Optional DuckDB connection. When `NULL`, a temporary connection
#'   is opened and closed by this function.
#'
#' @returns
#' Character vector of finalized cached Parquet file paths.
#'
#' @noRd
cache_schema_align_files <- function(files, con = NULL) {
  if (!length(files)) {
    stop(
      "`files` must contain at least one cached Parquet file.",
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- cache_build_connect()
    on.exit(cache_build_disconnect(con), add = TRUE)
  }

  out <- vapply(
    files,
    cache_schema_align_file,
    character(1),
    con = con
  )

  unname(out)
}
