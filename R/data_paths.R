# Internal data path helpers --------------------------------------------------
#
# These helpers discover installed and cached data files. They do not read
# Parquet files, build cache files, download data, or change reader behavior.

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

slc_available_data_years <- function(type = c("main", "div"), root = NULL) {
  type <- match.arg(type)

  sort(unique(c(
    slc_available_installed_years(type),
    slc_available_cached_years(type, root = root)
  )))
}

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

slc_data_paths <- function(type = c("main", "div"), years = NULL, root = NULL) {
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

slc_coords_path <- function(root = NULL) {
  manifest <- read_cache_manifest(root = root)
  cached <- slc_cache_coords_path(root = root, create = FALSE)

  if (!is.null(manifest) && file.exists(cached)) {
    return(cached)
  }

  slc_installed_coords_path()
}

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
