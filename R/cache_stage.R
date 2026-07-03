# Internal cache staging helpers ---------------------------------------------
#
# These helpers orchestrate a complete cache build inside a staging directory
# and can promote a validated staging directory to the active cache location.
#
# They do not download data and they do not expose user-facing update behavior.

cache_stage_prepare <- function(root = slc_cache_staging_root()) {
  if (dir.exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }

  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  root
}

cache_stage_months_by_year <- function(months) {
  months <- validate_cache_months(months)

  split(
    months$month,
    months$year
  )
}

db_stage_months_by_year <- function(months) {
  months <- validate_db_months(months)

  split(
    months$month,
    months$year
  )
}

cache_stage_parquet_files <- function(root, years) {
  years <- normalize_data_years(years)

  paths <- unlist(
    lapply(
      years,
      function(year) {
        c(
          slc_cache_parquet_path(
            "main",
            year,
            root = root,
            create = FALSE
          ),
          slc_cache_parquet_path(
            "div",
            year,
            root = root,
            create = FALSE
          )
        )
      }
    ),
    use.names = FALSE
  )

  unname(paths[file.exists(paths)])
}

cache_stage_coord_files <- function(root, years) {
  cache_stage_parquet_files(
    root = root,
    years = years
  )
}

#' Validate a staged cache
#'
#' Checks that a staged cache root contains a valid manifest and all files
#' required by that manifest.
#'
#' @param root Staging cache root to validate.
#'
#' @returns
#' Invisibly, `TRUE` when the staged cache is valid.
#'
#' @noRd
cache_stage_validate <- function(root) {
  validate_cache_files(root = root)
  invisible(TRUE)
}

#' Validate a staged local database
#'
#' Checks that a staged database root contains a valid database manifest and
#' all files required by that manifest.
#'
#' @param root Staging database root to validate.
#'
#' @returns
#' Invisibly, `TRUE` when the staged database is valid.
#'
#' @noRd
db_stage_validate <- function(root) {
  manifest <- read_db_manifest(root = root)

  if (is.null(manifest)) {
    stop(
      "The staged slcflights database is missing a manifest.",
      call. = FALSE
    )
  }

  validate_db_files(root = root)
  invisible(TRUE)
}

#' Build a local cache in a staging directory
#'
#' Builds annual main and diversion-only Parquet files for the requested cached
#' months, reduces metadata tables to values used by staged cache records,
#' enriches cached files with coordinate and airline metadata, finalizes cached
#' Parquet files, writes a cache manifest, and validates the staged cache.
#'
#' @param months Data frame with `year` and `month` columns. The months must
#'   form a consecutive sequence.
#' @param root Staging cache root to create and populate.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param coords_in Path to the raw BTS Master Coordinate CSV.
#' @param airlines_in Path to the raw BTS Airline ID lookup CSV.
#' @param csv_files Optional named list of monthly BTS CSV files by year. Used
#'   by tests and lower-level workflows.
#' @param finalize_schema If `TRUE`, finalize staged Parquet files after
#'   metadata enrichment.
#'
#' @returns
#' Invisibly, a list containing the staging root, requested months,
#' built files, metadata CSV paths, and manifest.
#'
#' @noRd
cache_stage_build <- function(
  months,
  root = slc_cache_staging_root(),
  slc_id = 14869L,
  coords_in = slc_cache_raw_coords_path(create = FALSE),
  airlines_in = slc_cache_raw_airlines_path(create = FALSE),
  csv_files = NULL,
  finalize_schema = TRUE
) {
  months <- validate_cache_months(months)
  root <- cache_stage_prepare(root)

  by_year <- cache_stage_months_by_year(months)
  years <- as.integer(names(by_year))

  con <- cache_build_connect()
  on.exit(
    {
      if (!is.null(con)) {
        cache_build_disconnect(con)
      }
    },
    add = TRUE
  )

  built_files <- character()

  for (year in years) {
    year_name <- as.character(year)
    year_csv <- NULL

    if (!is.null(csv_files)) {
      year_csv <- csv_files[[year_name]]
    }

    built_files <- c(
      built_files,
      cache_build_write_year_files(
        year = year,
        months = by_year[[year_name]],
        root = root,
        slc_id = slc_id,
        csv_files = year_csv,
        con = con
      )
    )
  }

  coord_files <- cache_stage_coord_files(
    root = root,
    years = years
  )

  coords_out <- slc_cache_coords_path(
    root = root,
    create = TRUE
  )

  airlines_out <- slc_cache_airlines_path(
    root = root,
    create = TRUE
  )

  cache_build_reduce_coords_csv(
    parquet_files = coord_files,
    coords_in = coords_in,
    coords_out = coords_out,
    con = con
  )

  cache_build_reduce_air_csv(
    parquet_files = coord_files,
    airlines_in = airlines_in,
    airlines_out = airlines_out,
    con = con
  )

  cache_build_enrich_files(
    parquet_files = built_files,
    coords_in = coords_out,
    con = con
  )

  cache_build_disconnect(con)
  con <- cache_build_connect()

  cache_build_enrich_air_files(
    parquet_files = built_files,
    airlines_in = airlines_out,
    con = con
  )

  if (isTRUE(finalize_schema)) {
    cache_build_disconnect(con)
    con <- NULL

    cache_schema_align_files(
      files = built_files
    )
  }

  manifest <- write_cache_manifest(
    months = months,
    root = root
  )

  cache_stage_validate(root)

  invisible(list(
    root = root,
    months = months,
    files = unname(built_files),
    coords = coords_out,
    airlines = airlines_out,
    manifest = manifest
  ))
}

#' Build a local database in a staging directory
#'
#' Builds annual main and diversion-only Parquet files for a contiguous local
#' database month sequence, reduces metadata tables to values used by the
#' staged database, enriches staged files with coordinate and airline metadata,
#' finalizes staged Parquet files, writes a database manifest, and validates the
#' staged database.
#'
#' @param months Data frame with `year` and `month` columns. The months must
#'   begin in October 1987 and form a consecutive sequence.
#' @param root Staging database root to create and populate.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param coords_in Path to the raw BTS Master Coordinate CSV.
#' @param airlines_in Path to the raw BTS Airline ID lookup CSV.
#' @param csv_files Optional named list of monthly BTS CSV files by year. Used
#'   by tests and lower-level workflows.
#' @param finalize_schema If `TRUE`, finalize staged Parquet files after
#'   metadata enrichment.
#'
#' @returns
#' Invisibly, a list containing the staging root, requested months, built files,
#' metadata CSV paths, and manifest.
#'
#' @noRd
db_stage_build <- function(
  months,
  root = slc_db_staging_root(),
  slc_id = 14869L,
  coords_in = slc_cache_raw_coords_path(create = FALSE),
  airlines_in = slc_cache_raw_airlines_path(create = FALSE),
  csv_files = NULL,
  finalize_schema = TRUE
) {
  months <- validate_db_months(months)
  root <- cache_stage_prepare(root)

  by_year <- db_stage_months_by_year(months)
  years <- as.integer(names(by_year))

  con <- cache_build_connect()
  on.exit(
    {
      if (!is.null(con)) {
        cache_build_disconnect(con)
      }
    },
    add = TRUE
  )

  built_files <- character()

  for (year in years) {
    year_name <- as.character(year)
    year_csv <- NULL

    if (!is.null(csv_files)) {
      year_csv <- csv_files[[year_name]]
    }

    built_files <- c(
      built_files,
      cache_build_write_year_files(
        year = year,
        months = by_year[[year_name]],
        root = root,
        slc_id = slc_id,
        csv_files = year_csv,
        con = con
      )
    )
  }

  coord_files <- cache_stage_parquet_files(
    root = root,
    years = years
  )

  coords_out <- slc_db_coords_path(
    root = root,
    create = TRUE
  )

  airlines_out <- slc_db_airlines_path(
    root = root,
    create = TRUE
  )

  cache_build_reduce_coords_csv(
    parquet_files = coord_files,
    coords_in = coords_in,
    coords_out = coords_out,
    con = con
  )

  cache_build_reduce_air_csv(
    parquet_files = coord_files,
    airlines_in = airlines_in,
    airlines_out = airlines_out,
    con = con
  )

  cache_build_enrich_files(
    parquet_files = built_files,
    coords_in = coords_out,
    con = con
  )

  cache_build_disconnect(con)
  con <- cache_build_connect()

  cache_build_enrich_air_files(
    parquet_files = built_files,
    airlines_in = airlines_out,
    con = con
  )

  if (isTRUE(finalize_schema)) {
    cache_build_disconnect(con)
    con <- NULL

    cache_schema_align_files(
      files = built_files
    )
  }

  manifest <- write_db_manifest(
    months = months,
    root = root
  )

  db_stage_validate(root)

  invisible(list(
    root = root,
    months = months,
    files = unname(built_files),
    coords = coords_out,
    airlines = airlines_out,
    manifest = manifest
  ))
}

#' Copy the active database into staging
#'
#' Prepares a staging root and copies the active local database into it.
#'
#' @param active_root Active local database root.
#' @param root Staging database root.
#'
#' @returns
#' Invisibly, the staging root.
#'
#' @noRd
db_stage_copy_active <- function(
  active_root = slc_db_root(create = FALSE),
  root = slc_db_staging_root(create = TRUE)
) {
  if (!dir.exists(active_root)) {
    stop(
      "No active slcflights database was found.",
      call. = FALSE
    )
  }

  root <- cache_stage_prepare(root)

  files <- list.files(
    active_root,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )

  if (length(files)) {
    ok <- file.copy(
      from = files,
      to = root,
      recursive = TRUE,
      copy.date = TRUE
    )

    if (!all(ok)) {
      stop(
        "Failed to copy the active slcflights database into staging.",
        call. = FALSE
      )
    }
  }

  invisible(root)
}

#' Extend a local database in a staging directory
#'
#' Copies the active local database into staging, rebuilds annual files affected
#' by the extension months, refreshes metadata, writes a database manifest, and
#' validates the staged database.
#'
#' @param months Full database month sequence through the requested endpoint.
#' @param extend_months Extension months to add to the active database.
#' @param active_root Active local database root.
#' @param root Staging database root.
#' @param slc_id BTS airport ID for Salt Lake City.
#' @param coords_in Path to the raw BTS Master Coordinate CSV.
#' @param airlines_in Path to the raw BTS Airline ID lookup CSV.
#' @param csv_files Optional named list of monthly BTS CSV files by year. Used
#'   by tests and lower-level workflows.
#' @param finalize_schema If `TRUE`, finalize staged Parquet files after
#'   metadata enrichment.
#'
#' @returns
#' Invisibly, a list containing the staging root, requested months, extension
#' months, rebuilt files, metadata CSV paths, and manifest.
#'
#' @noRd
db_stage_extend <- function(
  months,
  extend_months,
  active_root = slc_db_root(create = FALSE),
  root = slc_db_staging_root(create = TRUE),
  slc_id = 14869L,
  coords_in = slc_cache_raw_coords_path(create = FALSE),
  airlines_in = slc_cache_raw_airlines_path(create = FALSE),
  csv_files = NULL,
  finalize_schema = TRUE
) {
  months <- validate_db_months(months)

  if (!is.data.frame(extend_months)) {
    stop("`extend_months` must be a data frame.", call. = FALSE)
  }

  if (!all(c("year", "month") %in% names(extend_months))) {
    stop(
      "`extend_months` must contain `year` and `month` columns.",
      call. = FALSE
    )
  }

  if (!nrow(extend_months)) {
    stop("`extend_months` must contain at least one month.", call. = FALSE)
  }

  extend_months <- extend_months[, c("year", "month"), drop = FALSE]
  extend_months$year <- as.integer(extend_months$year)
  extend_months$month <- as.integer(extend_months$month)

  root <- db_stage_copy_active(
    active_root = active_root,
    root = root
  )

  years <- sort(unique(extend_months$year))
  con <- cache_build_connect()

  on.exit(
    {
      if (!is.null(con)) {
        cache_build_disconnect(con)
      }
    },
    add = TRUE
  )

  built_files <- character()

  for (year in years) {
    year_name <- as.character(year)
    year_months <- months$month[months$year == year]
    year_csv <- NULL

    if (!is.null(csv_files)) {
      year_csv <- csv_files[[year_name]]
    }

    unlink(
      c(
        slc_db_parquet_path(
          "main",
          year,
          root = root,
          create = FALSE
        ),
        slc_db_parquet_path(
          "div",
          year,
          root = root,
          create = FALSE
        )
      ),
      force = TRUE
    )

    built_files <- c(
      built_files,
      cache_build_write_year_files(
        year = year,
        months = year_months,
        root = root,
        slc_id = slc_id,
        csv_files = year_csv,
        con = con
      )
    )
  }

  all_years <- sort(unique(months$year))

  coord_files <- cache_stage_parquet_files(
    root = root,
    years = all_years
  )

  coords_out <- slc_db_coords_path(
    root = root,
    create = TRUE
  )

  airlines_out <- slc_db_airlines_path(
    root = root,
    create = TRUE
  )

  cache_build_reduce_coords_csv(
    parquet_files = coord_files,
    coords_in = coords_in,
    coords_out = coords_out,
    con = con
  )

  cache_build_reduce_air_csv(
    parquet_files = coord_files,
    airlines_in = airlines_in,
    airlines_out = airlines_out,
    con = con
  )

  cache_build_enrich_files(
    parquet_files = built_files,
    coords_in = coords_out,
    con = con
  )

  cache_build_disconnect(con)
  con <- cache_build_connect()

  cache_build_enrich_air_files(
    parquet_files = built_files,
    airlines_in = airlines_out,
    con = con
  )

  if (isTRUE(finalize_schema)) {
    cache_build_disconnect(con)
    con <- NULL

    cache_schema_align_files(
      files = built_files
    )
  }

  manifest <- write_db_manifest(
    months = months,
    root = root
  )

  db_stage_validate(root)

  invisible(list(
    root = root,
    months = months,
    extend_months = extend_months,
    files = unname(built_files),
    coords = coords_out,
    airlines = airlines_out,
    manifest = manifest
  ))
}

#' Promote a staged cache to the active cache
#'
#' Validates a staged cache and atomically promotes it to the active cache
#' location. If an active cache already exists, it is first moved to a backup
#' location so it can be restored if promotion fails.
#'
#' @param staging_root Staging cache root to promote.
#' @param active_root Active cache root to replace.
#'
#' @returns
#' Invisibly, the active cache root.
#'
#' @noRd
cache_stage_promote <- function(
  staging_root = slc_cache_staging_root(create = FALSE),
  active_root = slc_cache_active_root(create = FALSE)
) {
  if (!dir.exists(staging_root)) {
    stop(
      sprintf("Staging cache directory not found: %s", staging_root),
      call. = FALSE
    )
  }

  cache_stage_validate(staging_root)

  cache_root <- slc_cache_root(create = TRUE)
  backup_root <- file.path(cache_root, "active-backup")

  if (dir.exists(backup_root)) {
    unlink(backup_root, recursive = TRUE, force = TRUE)
  }

  active_existed <- dir.exists(active_root)

  if (active_existed) {
    ok_backup <- file.rename(active_root, backup_root)

    if (!ok_backup) {
      stop(
        sprintf("Failed to move active cache to backup: %s", active_root),
        call. = FALSE
      )
    }
  }

  dir.create(dirname(active_root), recursive = TRUE, showWarnings = FALSE)

  ok_promote <- file.rename(staging_root, active_root)

  if (!ok_promote) {
    if (active_existed && dir.exists(backup_root)) {
      file.rename(backup_root, active_root)
    }

    stop(
      sprintf("Failed to promote staging cache to active: %s", active_root),
      call. = FALSE
    )
  }

  if (dir.exists(backup_root)) {
    unlink(backup_root, recursive = TRUE, force = TRUE)
  }

  invisible(active_root)
}

#' Promote a staged local database
#'
#' Validates a staged local database, removes any existing active database, and
#' promotes the staged database to the active database location.
#'
#' @param staging_root Staged database root.
#' @param active_root Active database root.
#'
#' @returns
#' Invisibly, the active database root.
#'
#' @noRd
db_stage_promote <- function(
  staging_root = slc_db_staging_root(create = FALSE),
  active_root = slc_db_root(create = FALSE)
) {
  db_stage_validate(staging_root)

  if (dir.exists(active_root)) {
    unlink(active_root, recursive = TRUE, force = TRUE)
  }

  parent <- dirname(active_root)

  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  ok <- file.rename(staging_root, active_root)

  if (!isTRUE(ok)) {
    stop(
      "Failed to activate the staged slcflights database.",
      call. = FALSE
    )
  }

  invisible(active_root)
}
