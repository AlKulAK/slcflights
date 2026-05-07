# Internal cache manifest helpers --------------------------------------------
#
# The active runtime cache uses a JSON manifest. These helpers construct,
# validate, read, and write that manifest. They do not download data, build
# Parquet files, or change reader behavior.

slc_manifest_created_at <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

slc_manifest_package_version <- function() {
  version <- utils::packageDescription("slcflights", fields = "Version")

  if (is.na(version)) {
    return(NA_character_)
  }

  version
}

validate_cache_months <- function(months, arg = "months") {
  if (!is.data.frame(months)) {
    stop(sprintf("`%s` must be a data frame.", arg), call. = FALSE)
  }

  if (!all(c("year", "month") %in% names(months))) {
    stop(
      sprintf("`%s` must contain `year` and `month` columns.", arg),
      call. = FALSE
    )
  }

  if (!nrow(months)) {
    stop(sprintf("`%s` must contain at least one month.", arg), call. = FALSE)
  }

  months <- months[, c("year", "month"), drop = FALSE]

  months$year <- as.integer(months$year)
  months$month <- as.integer(months$month)

  for (i in seq_len(nrow(months))) {
    as_year_month(
      months$year[[i]],
      months$month[[i]],
      arg = sprintf("%s[%s, ]", arg, i)
    )
  }

  months <- months[order(months$year, months$month), , drop = FALSE]
  row.names(months) <- NULL

  first <- as_year_month(months$year[[1]], months$month[[1]])
  expected_first <- slc_first_download()

  if (year_month_index(first) != year_month_index(expected_first)) {
    stop(
      paste(
        "Cached months must begin with July 2024.",
        "Downloaded data must be consecutive."
      ),
      call. = FALSE
    )
  }

  last <- as_year_month(
    months$year[[nrow(months)]],
    months$month[[nrow(months)]]
  )

  expected <- update_month_sequence(last)

  if (!identical(months, expected)) {
    stop(
      paste(
        "Cached months must be consecutive.",
        "Missing or duplicated months are not allowed."
      ),
      call. = FALSE
    )
  }

  months
}

new_cache_manifest <- function(
  months,
  package_version = slc_manifest_package_version(),
  created_at = slc_manifest_created_at()
) {
  months <- validate_cache_months(months)

  cached_start <- as_year_month(months$year[[1]], months$month[[1]])
  cached_end <- as_year_month(
    months$year[[nrow(months)]],
    months$month[[nrow(months)]]
  )

  list(
    schema_version = slc_schema_version,
    package_version = package_version,
    bundled_start = format_year_month(slc_bundled_start()),
    bundled_end = format_year_month(slc_bundled_end()),
    cached_start = format_year_month(cached_start),
    cached_end = format_year_month(cached_end),
    created_at = created_at,
    months = lapply(seq_len(nrow(months)), function(i) {
      list(
        year = months$year[[i]],
        month = months$month[[i]],
        status = "complete"
      )
    })
  )
}

validate_cache_manifest <- function(manifest, arg = "manifest") {
  if (!is.list(manifest)) {
    stop(sprintf("`%s` must be a list.", arg), call. = FALSE)
  }

  required <- c(
    "schema_version",
    "package_version",
    "bundled_start",
    "bundled_end",
    "cached_start",
    "cached_end",
    "created_at",
    "months"
  )

  missing <- setdiff(required, names(manifest))

  if (length(missing)) {
    stop(
      sprintf(
        "`%s` is missing required field: %s",
        arg,
        missing[[1]]
      ),
      call. = FALSE
    )
  }

  if (!identical(as.integer(manifest$schema_version), slc_schema_version)) {
    stop(
      paste(
        "The slcflights cache was built with an incompatible schema version.",
        "Run clear_slcflights_cache() and update_slcflights_data()."
      ),
      call. = FALSE
    )
  }

  if (
    !identical(manifest$bundled_start, format_year_month(slc_bundled_start()))
  ) {
    stop("The cache manifest has an unexpected bundled start.", call. = FALSE)
  }

  if (!identical(manifest$bundled_end, format_year_month(slc_bundled_end()))) {
    stop("The cache manifest has an unexpected bundled end.", call. = FALSE)
  }

  months <- cache_months_from_manifest(manifest)
  months <- validate_cache_months(months)

  cached_start <- format_year_month(
    as_year_month(months$year[[1]], months$month[[1]])
  )

  cached_end <- format_year_month(
    as_year_month(
      months$year[[nrow(months)]],
      months$month[[nrow(months)]]
    )
  )

  if (!identical(manifest$cached_start, cached_start)) {
    stop("The cache manifest has an inconsistent cached start.", call. = FALSE)
  }

  if (!identical(manifest$cached_end, cached_end)) {
    stop("The cache manifest has an inconsistent cached end.", call. = FALSE)
  }

  manifest
}

cache_months_from_manifest <- function(manifest) {
  if (is.null(manifest$months) || !length(manifest$months)) {
    return(data.frame(year = integer(), month = integer()))
  }

  data.frame(
    year = vapply(manifest$months, function(x) as.integer(x$year), integer(1)),
    month = vapply(
      manifest$months,
      function(x) as.integer(x$month),
      integer(1)
    )
  )
}

write_cache_manifest <- function(
  months,
  root = NULL,
  package_version = slc_manifest_package_version(),
  created_at = slc_manifest_created_at()
) {
  manifest <- new_cache_manifest(
    months = months,
    package_version = package_version,
    created_at = created_at
  )

  path <- slc_cache_manifest_path(root = root, create = TRUE)

  jsonlite::write_json(
    manifest,
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(manifest)
}

read_cache_manifest <- function(root = NULL) {
  path <- slc_cache_manifest_path(root = root, create = FALSE)

  if (!file.exists(path)) {
    return(NULL)
  }

  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  validate_cache_manifest(manifest)
}

cache_endpoint_from_manifest <- function(manifest) {
  manifest <- validate_cache_manifest(manifest)
  as_year_month_string(manifest$cached_end, arg = "cached_end")
}

cache_months_are_complete <- function(root = NULL) {
  manifest <- read_cache_manifest(root = root)

  if (is.null(manifest)) {
    return(FALSE)
  }

  months <- cache_months_from_manifest(manifest)
  years <- sort(unique(months$year))

  for (year in years) {
    main <- slc_cache_parquet_path(
      "main",
      year,
      root = root,
      create = FALSE
    )

    if (!file.exists(main)) {
      return(FALSE)
    }
  }

  coords <- slc_cache_coords_path(root = root, create = FALSE)

  if (!file.exists(coords)) {
    return(FALSE)
  }

  TRUE
}

validate_cache_files <- function(root = NULL) {
  manifest <- read_cache_manifest(root = root)

  if (is.null(manifest)) {
    return(invisible(FALSE))
  }

  months <- cache_months_from_manifest(manifest)
  years <- sort(unique(months$year))
  missing <- character()

  for (year in years) {
    main <- slc_cache_parquet_path(
      "main",
      year,
      root = root,
      create = FALSE
    )

    if (!file.exists(main)) {
      missing <- c(missing, main)
    }
  }

  coords <- slc_cache_coords_path(root = root, create = FALSE)

  if (!file.exists(coords)) {
    missing <- c(missing, coords)
  }

  if (length(missing)) {
    stop(
      paste(
        "The slcflights cache appears incomplete.",
        "Run repair_slcflights_cache() or clear_slcflights_cache().",
        "Missing file:",
        missing[[1]]
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
