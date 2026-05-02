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

cache_stage_installed_files <- function() {
  paths <- rbind(
    slc_installed_data_paths("main"),
    slc_installed_data_paths("div")
  )

  unname(paths$path[file.exists(paths$path)])
}

cache_stage_coord_files <- function(
  root,
  years,
  include_installed = TRUE
) {
  cache_files <- cache_stage_parquet_files(
    root = root,
    years = years
  )

  if (!isTRUE(include_installed)) {
    return(cache_files)
  }

  unname(c(cache_stage_installed_files(), cache_files))
}

cache_stage_validate <- function(root) {
  validate_cache_files(root = root)
  invisible(TRUE)
}

cache_stage_build <- function(
  months,
  root = slc_cache_staging_root(),
  slc_id = 14869L,
  coords_in = slc_cache_raw_coords_path(create = FALSE),
  csv_files = NULL,
  include_installed = TRUE
) {
  months <- validate_cache_months(months)
  root <- cache_stage_prepare(root)

  by_year <- cache_stage_months_by_year(months)
  years <- as.integer(names(by_year))

  con <- cache_build_connect()
  on.exit(cache_build_disconnect(con), add = TRUE)

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
    years = years,
    include_installed = include_installed
  )

  coords_out <- slc_cache_coords_path(
    root = root,
    create = TRUE
  )

  cache_build_reduce_coords_csv(
    parquet_files = coord_files,
    coords_in = coords_in,
    coords_out = coords_out,
    con = con
  )

  cache_build_enrich_files(
    parquet_files = built_files,
    coords_in = coords_out,
    con = con
  )

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
    manifest = manifest
  ))
}

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
