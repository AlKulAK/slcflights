# Internal data path helpers --------------------------------------------------
#
# These helpers discover installed and cached data files. They do not read
# Parquet files, build cache files, download data, or change reader behavior.

#' Locate an installed slcflights extdata file
#'
#' Resolves a path under the package's installed `inst/` tree using
#' [system.file()] and errors if the file is not present.
#'
#' @param ... Path components passed to [system.file()].
#'
#' @returns
#' Character path to an installed package file.
#'
#' @noRd
slc_extdata_file <- function(...) {
  rel_path <- file.path(...)

  path <- system.file(
    ...,
    package = "slcflights"
  )

  if (!nzchar(path)) {
    stop(
      sprintf("Installed slcflights file not found: %s", rel_path),
      call. = FALSE
    )
  }

  path
}

slc_installed_extdata_root <- function() {
  slc_extdata_file("extdata")
}

slc_installed_parquet_root <- function() {
  slc_extdata_file("extdata", "parquet")
}

slc_installed_csv_root <- function() {
  slc_extdata_file("extdata", "csv")
}

slc_installed_year_dir <- function(year) {
  year <- normalize_cache_year(year)

  file.path(
    slc_installed_parquet_root(),
    sprintf("Year=%s", year)
  )
}

slc_installed_parquet_path <- function(type = c("main", "div"), year) {
  type <- match.arg(type)
  year <- normalize_cache_year(year)

  filename <- switch(type,
    main = "data_0_main.parquet",
    div = "data_0_div.parquet"
  )

  file.path(
    slc_installed_year_dir(year),
    filename
  )
}

slc_installed_coords_path <- function() {
  slc_extdata_file(
    "extdata",
    "csv",
    "T_MASTER_CORD_reduced.csv"
  )
}

slc_installed_year_dirs <- function() {
  root <- slc_installed_parquet_root()

  dirs <- list.dirs(
    root,
    recursive = FALSE,
    full.names = FALSE
  )

  dirs[grepl("^Year=[0-9]{4}$", dirs)]
}

slc_years_from_dirs <- function(dirs) {
  if (!length(dirs)) {
    return(integer())
  }

  years <- as.integer(sub("^Year=", "", dirs))
  sort(unique(years[!is.na(years)]))
}

#' List installed years for a flight-data grouping
#'
#' Lists years for which the installed package contains a main or
#' diversion-only Parquet file.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#'
#' @returns
#' Integer vector of installed years sorted in ascending order.
#'
#' @noRd
slc_available_installed_years <- function(type = c("main", "div")) {
  type <- match.arg(type)

  years <- slc_years_from_dirs(slc_installed_year_dirs())

  if (!length(years)) {
    return(integer())
  }

  paths <- vapply(
    years,
    function(year) {
      slc_installed_parquet_path(type, year)
    },
    character(1)
  )

  years[file.exists(paths)]
}

slc_cached_year_dirs <- function(root = NULL) {
  parquet_root <- slc_cache_parquet_root(
    root = root,
    create = FALSE
  )

  if (!dir.exists(parquet_root)) {
    return(character())
  }

  dirs <- list.dirs(
    parquet_root,
    recursive = FALSE,
    full.names = FALSE
  )

  dirs[grepl("^Year=[0-9]{4}$", dirs)]
}

#' List cached years for a flight-data grouping
#'
#' Lists years for which the active local cache contains a main or
#' diversion-only Parquet file and has a readable manifest.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#'
#' @returns
#' Integer vector of cached years sorted in ascending order.
#'
#' @noRd
slc_available_cached_years <- function(type = c("main", "div"), root = NULL) {
  type <- match.arg(type)

  manifest <- read_cache_manifest(root = root)
  if (is.null(manifest)) {
    return(integer())
  }

  months <- cache_months_from_manifest(manifest)
  years <- sort(unique(months$year))

  if (!length(years)) {
    return(integer())
  }

  paths <- vapply(
    years,
    function(year) {
      slc_cache_parquet_path(
        type,
        year,
        root = root,
        create = FALSE
      )
    },
    character(1)
  )

  years[file.exists(paths)]
}

#' List all available years for a flight-data grouping
#'
#' Combines installed and cached years for a main or diversion-only data
#' grouping.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#'
#' @returns
#' Integer vector of available years sorted in ascending order.
#'
#' @noRd
slc_available_data_years <- function(type = c("main", "div"), root = NULL) {
  type <- match.arg(type)

  sort(unique(c(
    slc_available_installed_years(type),
    slc_available_cached_years(type, root = root)
  )))
}

#' Resolve installed Parquet paths
#'
#' Resolves installed package Parquet files for a main or diversion-only data
#' grouping.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param years Optional integer vector of years. Uses all installed years for
#'   `type` when `NULL`.
#'
#' @returns
#' Data frame with `year`, `source`, and `path` columns. `source` is always
#' `"installed"`.
#'
#' @noRd
slc_installed_data_paths <- function(type = c("main", "div"), years = NULL) {
  type <- match.arg(type)

  if (is.null(years)) {
    years <- slc_available_installed_years(type)
  } else {
    years <- normalize_data_years(years)
  }

  if (!length(years)) {
    return(data.frame(
      year = integer(),
      source = character(),
      path = character()
    ))
  }

  paths <- vapply(
    years,
    function(year) {
      slc_installed_parquet_path(type, year)
    },
    character(1)
  )

  keep <- file.exists(paths)

  data.frame(
    year = years[keep],
    source = rep("installed", sum(keep)),
    path = unname(paths[keep])
  )
}

#' Resolve cached Parquet paths
#'
#' Resolves active-cache Parquet files for a main or diversion-only data
#' grouping.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param years Optional integer vector of years. Uses all cached years for
#'   `type` when `NULL`.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#'
#' @returns
#' Data frame with `year`, `source`, and `path` columns. `source` is always
#' `"cache"`.
#'
#' @noRd
slc_cached_data_paths <- function(
  type = c("main", "div"),
  years = NULL,
  root = NULL
) {
  type <- match.arg(type)

  manifest <- read_cache_manifest(root = root)
  if (is.null(manifest)) {
    return(data.frame(
      year = integer(),
      source = character(),
      path = character()
    ))
  }

  if (is.null(years)) {
    years <- slc_available_cached_years(type, root = root)
  } else {
    years <- normalize_data_years(years)
  }

  if (!length(years)) {
    return(data.frame(
      year = integer(),
      source = character(),
      path = character()
    ))
  }

  paths <- vapply(
    years,
    function(year) {
      slc_cache_parquet_path(
        type,
        year,
        root = root,
        create = FALSE
      )
    },
    character(1)
  )

  keep <- file.exists(paths)

  data.frame(
    year = years[keep],
    source = rep("cache", sum(keep)),
    path = unname(paths[keep])
  )
}

#' Resolve installed and cached Parquet paths
#'
#' Resolves reader-facing Parquet paths for a main or diversion-only data
#' grouping, combining installed package files with compatible cached files.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param years Optional integer vector of years. Uses all available years for
#'   `type` when `NULL`.
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#'
#' @returns
#' Data frame with `year`, `source`, and `path` columns, sorted by year and
#' then source.
#'
#' @noRd
slc_data_paths <- function(
  type = c("main", "div"), years = NULL, root = NULL
) {
  type <- match.arg(type)

  if (is.null(years)) {
    years <- slc_available_data_years(type, root = root)
  } else {
    years <- normalize_data_years(years)
  }

  installed <- slc_installed_data_paths(type, years)
  cached <- slc_cached_data_paths(type, years, root = root)

  out <- rbind(installed, cached)

  if (!nrow(out)) {
    label <- switch(type,
      main = "main",
      div = "diversion-only"
    )

    stop(
      sprintf("No %s Parquet files are available.", label),
      call. = FALSE
    )
  }

  out <- out[order(out$year, match(out$source, c("installed", "cache"))), ]
  row.names(out) <- NULL

  out
}

#' Resolve the active coordinate CSV path
#'
#' Returns the cached coordinate CSV when a compatible cache is active and the
#' cached coordinate file exists. Otherwise returns the installed coordinate
#' CSV.
#'
#' @param root Optional cache root. Uses the active cache root when `NULL`.
#'
#' @returns
#' Character path to the coordinate CSV used by [read_coords()].
#'
#' @noRd
slc_coords_path <- function(root = NULL) {
  manifest <- read_cache_manifest(root = root)
  cached <- slc_cache_coords_path(root = root, create = FALSE)

  if (!is.null(manifest) && file.exists(cached)) {
    return(cached)
  }

  slc_installed_coords_path()
}

#' Normalize a reader year vector
#'
#' Validates and normalizes the `years` argument used by reader and path
#' helpers.
#'
#' @param years `NULL` or a numeric vector of whole four-digit calendar years.
#'
#' @returns
#' `NULL` when `years` is `NULL`; otherwise a sorted unique integer vector.
#'
#' @noRd
normalize_data_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }

  if (length(years) == 0L) {
    stop(
      "`years` must contain at least one year or be NULL.",
      call. = FALSE
    )
  }

  if (anyNA(years)) {
    stop("`years` must not contain missing values.", call. = FALSE)
  }

  if (!is.numeric(years)) {
    stop(
      paste(
        "`years` must be a numeric vector of whole years,",
        "such as 1987 or c(1987, 1988)."
      ),
      call. = FALSE
    )
  }

  if (any(!is.finite(years))) {
    stop("`years` must contain only finite values.", call. = FALSE)
  }

  if (any(years != trunc(years))) {
    stop("`years` must contain whole-number years.", call. = FALSE)
  }

  if (any(years < 1000 | years > 9999)) {
    stop("`years` must contain four-digit calendar years.", call. = FALSE)
  }

  sort(unique(as.integer(years)))
}
