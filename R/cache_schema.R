# Internal cache schema finalization helpers ---------------------------------
#
# These helpers make cached Parquet files conform to the installed package
# schema contract. They drop cache-only columns, preserve installed column
# order, and error if a cached file is missing an installed template column.
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

cache_schema_template <- function(type = c("main", "div"), year) {
  type <- match.arg(type)
  year <- normalize_cache_year(year)

  candidate <- slc_installed_parquet_path(type, year)

  if (file.exists(candidate)) {
    return(candidate)
  }

  years <- slc_available_installed_years(type)

  if (!length(years)) {
    stop(
      sprintf("No installed %s schema template is available.", type),
      call. = FALSE
    )
  }

  slc_installed_parquet_path(type, max(years))
}

cache_schema_cols <- function(con, path) {
  cache_build_read_parquet_cols(con, path)
}

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

  type <- cache_schema_type_for_file(path)
  year <- cache_schema_year_for_file(path)

  template <- cache_schema_template(type, year)

  template_cols <- cache_schema_cols(con, template)
  cached_cols <- cache_schema_cols(con, path)

  missing_cols <- setdiff(template_cols, cached_cols)

  if (length(missing_cols)) {
    stop(
      sprintf(
        "Cached %s Parquet file is missing required columns: %s",
        type,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  sel <- cache_build_sql_ider_list(con, template_cols)
  tmp <- paste0(path, ".tmp")

  DBI::dbExecute(
    con,
    sprintf(
      "
    COPY (
      SELECT %s
      FROM read_parquet(%s)
    )
    TO %s
    (FORMAT parquet)
    ",
      sel,
      cache_build_quote_path(con, path),
      DBI::dbQuoteString(
        con,
        normalizePath(tmp, winslash = "/", mustWork = FALSE)
      )
    )
  )

  cache_build_replace_file(tmp, path)

  invisible(path)
}

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
