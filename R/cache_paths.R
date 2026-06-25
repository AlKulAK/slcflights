# Internal cache path helpers -------------------------------------------------
#
# Runtime user updates write to tools::R_user_dir("slcflights", "cache").
# Tests may set SLCFLIGHTS_TEST_CACHE_ROOT to isolate cache state.
# These helpers only construct paths. They do not validate cache contents and
# they do not download, build, or read data.

slc_cache_root <- function(create = TRUE) {
  test_root <- Sys.getenv("SLCFLIGHTS_TEST_CACHE_ROOT", unset = "")

  if (nzchar(test_root)) {
    path <- path.expand(test_root)
  } else {
    path <- tools::R_user_dir("slcflights", which = "cache")
  }

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_active_root <- function(create = TRUE) {
  path <- file.path(slc_cache_root(create = create), "active")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_db_root <- function(create = TRUE) {
  slc_cache_active_root(create = create)
}

slc_db_staging_root <- function(create = TRUE) {
  slc_cache_staging_root(create = create)
}

slc_cache_staging_root <- function(create = TRUE) {
  path <- file.path(slc_cache_root(create = create), "staging")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_raw_root <- function(create = TRUE) {
  path <- file.path(slc_cache_root(create = create), "raw")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_extdata_root <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_cache_active_root(create = create)
  }

  path <- file.path(root, "extdata")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_parquet_root <- function(root = NULL, create = TRUE) {
  path <- file.path(
    slc_cache_extdata_root(root = root, create = create),
    "parquet"
  )

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_csv_root <- function(root = NULL, create = TRUE) {
  path <- file.path(
    slc_cache_extdata_root(root = root, create = create),
    "csv"
  )

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_db_extdata_root <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_extdata_root(root = root, create = create)
}

slc_db_parquet_root <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_parquet_root(root = root, create = create)
}

slc_db_csv_root <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_csv_root(root = root, create = create)
}

slc_cache_manifest_path <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_cache_active_root(create = create)
  }

  if (isTRUE(create)) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }

  file.path(root, "manifest.json")
}

slc_db_manifest_path <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_manifest_path(root = root, create = create)
}

slc_cache_year_dir <- function(year, root = NULL, create = TRUE) {
  year <- normalize_cache_year(year)

  path <- file.path(
    slc_cache_parquet_root(root = root, create = create),
    sprintf("Year=%s", year)
  )

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_parquet_path <- function(
  type = c("main", "div"),
  year,
  root = NULL,
  create = TRUE
) {
  type <- match.arg(type)
  year <- normalize_cache_year(year)

  filename <- switch(type,
    main = "data_0_main.parquet",
    div = "data_0_div.parquet"
  )

  file.path(
    slc_cache_year_dir(year, root = root, create = create),
    filename
  )
}

slc_db_year_dir <- function(year, root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_year_dir(year, root = root, create = create)
}

slc_db_parquet_path <- function(
  type = c("main", "div"),
  year,
  root = NULL,
  create = TRUE
) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_parquet_path(
    type = type,
    year = year,
    root = root,
    create = create
  )
}

slc_cache_coords_path <- function(root = NULL, create = TRUE) {
  file.path(
    slc_cache_csv_root(root = root, create = create),
    "T_MASTER_CORD_reduced.csv"
  )
}

slc_cache_airlines_path <- function(root = NULL, create = TRUE) {
  file.path(
    slc_cache_csv_root(root = root, create = create),
    "L_AIRLINE_ID_reduced.csv"
  )
}

slc_db_coords_path <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_coords_path(root = root, create = create)
}

slc_db_airlines_path <- function(root = NULL, create = TRUE) {
  if (is.null(root)) {
    root <- slc_db_root(create = create)
  }

  slc_cache_airlines_path(root = root, create = create)
}

slc_cache_raw_ontime_root <- function(create = TRUE) {
  path <- file.path(slc_cache_raw_root(create = create), "bts_ontime")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_raw_coords_root <- function(create = TRUE) {
  path <- file.path(slc_cache_raw_root(create = create), "bts_coords")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_raw_airlines_root <- function(create = TRUE) {
  path <- file.path(slc_cache_raw_root(create = create), "bts_airlines")

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_raw_ontime_month_dir <- function(year, month, create = TRUE) {
  ym <- as_year_month(year, month, arg = "year/month")

  path <- file.path(
    slc_cache_raw_ontime_root(create = create),
    sprintf("%04d_%02d", ym$year, ym$month)
  )

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}

slc_cache_raw_coords_path <- function(create = TRUE) {
  file.path(
    slc_cache_raw_coords_root(create = create),
    "T_MASTER_CORD.csv"
  )
}

slc_cache_raw_airlines_path <- function(create = TRUE) {
  file.path(
    slc_cache_raw_airlines_root(create = create),
    "L_AIRLINE_ID.csv"
  )
}

normalize_cache_year <- function(year) {
  if (length(year) != 1L) {
    stop("`year` must identify exactly one year.", call. = FALSE)
  }

  if (!is.numeric(year) || is.na(year) || !is.finite(year)) {
    stop("`year` must be a non-missing numeric value.", call. = FALSE)
  }

  if (year != trunc(year)) {
    stop("`year` must be a whole-number year.", call. = FALSE)
  }

  year <- as.integer(year)

  if (year < 1000L || year > 9999L) {
    stop("`year` must be a four-digit calendar year.", call. = FALSE)
  }

  year
}
