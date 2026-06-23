test_that("latest month skips unavailable candidate months", {
  available <- slcflights:::as_year_month(2025L, 11L)
  today <- as.Date("2026-02-15")

  testthat::local_mocked_bindings(
    bts_probe_month_url = function(year, month) {
      identical(
        slcflights:::as_year_month(year, month),
        available
      )
    },
    .package = "slcflights"
  )

  out <- slcflights:::bts_latest_month(today = today)

  expect_equal(out, available)
})

test_that("BTS candidate months run backward to first downloadable month", {
  out <- slcflights:::bts_candidate_months(
    today = as.Date("2024-09-15")
  )

  expect_equal(
    out,
    data.frame(
      year = c(2024L, 2024L, 2024L),
      month = c(9L, 8L, 7L)
    )
  )
})

test_that("explicit update endpoint resolves without network probing", {
  out <- slcflights:::resolve_update_until("2024-07")

  expect_equal(out$year, 2024L)
  expect_equal(out$month, 7L)
})

test_that("update months for explicit endpoint are consecutive", {
  out <- slcflights:::update_months_for_until("2024-09")

  expect_equal(
    out,
    data.frame(
      year = c(2024L, 2024L, 2024L),
      month = c(7L, 8L, 9L)
    )
  )
})

test_that("illegal update endpoint fails clearly", {
  expect_error(
    slcflights:::update_months_for_until("2024-06"),
    "bundled slcflights data end in June 2024"
  )
})

test_that("update Airline ID download writes to raw cache path", {
  testthat::local_mocked_bindings(
    download_bts_airline_id = function(destfile, overwrite = FALSE) {
      expect_equal(
        destfile,
        slcflights:::slc_cache_raw_airlines_path(create = TRUE)
      )
      expect_false(overwrite)
      destfile
    },
    .package = "slcflights"
  )

  out <- slcflights:::download_update_airlines(overwrite = FALSE)

  expect_equal(
    out,
    slcflights:::slc_cache_raw_airlines_path(create = TRUE)
  )
})

test_that("cache build passes raw metadata paths to staging", {
  months <- data.frame(year = 2024L, month = 7L)
  root <- tempfile("slc-update-cache-")

  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    slc_cache_root = function(create = TRUE) {
      if (isTRUE(create)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
      }

      root
    },
    cache_stage_build = function(months,
                                 root,
                                 coords_in,
                                 airlines_in,
                                 include_installed) {
      expect_equal(months, data.frame(year = 2024L, month = 7L))
      expect_equal(
        root,
        slcflights:::slc_cache_staging_root(create = TRUE)
      )
      expect_equal(
        coords_in,
        slcflights:::slc_cache_raw_coords_path(create = FALSE)
      )
      expect_equal(
        airlines_in,
        slcflights:::slc_cache_raw_airlines_path(create = FALSE)
      )
      expect_true(include_installed)
      invisible(TRUE)
    },
    cache_stage_promote = function(staging_root, active_root) {
      expect_equal(
        staging_root,
        slcflights:::slc_cache_staging_root(create = FALSE)
      )
      expect_equal(
        active_root,
        slcflights:::slc_cache_active_root(create = FALSE)
      )
      invisible(active_root)
    },
    .package = "slcflights"
  )

  out <- suppressMessages(
    slcflights:::build_update_cache(months)
  )

  expect_equal(out, slcflights:::slc_cache_active_root(create = FALSE))

  out_norm <- normalizePath(out, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)

  expect_true(startsWith(out_norm, root_norm))
})

test_that("cache info reports absent cache", {
  root <- tempfile("slc-update-cache-")

  info <- slcflights:::slc_cache_info(root = root)

  expect_equal(
    info$root,
    normalizePath(root, winslash = "/", mustWork = FALSE)
  )
  expect_false(info$exists)
  expect_false(info$complete)
  expect_equal(
    info$months,
    data.frame(year = integer(), month = integer())
  )
  expect_null(info$endpoint)
  expect_null(info$manifest)
})

test_that("cache info reports present cache", {
  root <- tempfile("slc-update-cache-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  coords <- slcflights:::slc_cache_coords_path(
    root = root,
    create = TRUE
  )

  airlines <- slcflights:::slc_cache_airlines_path(
    root = root,
    create = TRUE
  )

  writeLines("main", main)
  writeLines("coords", coords)
  writeLines("airlines", airlines)

  info <- slcflights:::slc_cache_info(root = root)

  expect_true(info$exists)
  expect_true(info$complete)
  expect_equal(info$endpoint$year, 2024L)
  expect_equal(info$endpoint$month, 7L)
  expect_equal(info$manifest$cached_end, "2024-07")

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("clearing cache root deletes directory without confirmation", {
  root <- tempfile("slc-update-cache-")
  dir.create(root, recursive = TRUE)
  writeLines("x", file.path(root, "x.txt"))

  out <- slcflights:::clear_cache_root(
    root = root,
    confirm = FALSE
  )

  expect_true(out)
  expect_false(dir.exists(root))
})

test_that("explicit database endpoint resolves without network probing", {
  out <- slcflights:::resolve_db_until("2024-06")

  expect_equal(out$year, 2024L)
  expect_equal(out$month, 6L)
})

test_that("database months for endpoint begin in October 1987", {
  out <- slcflights:::db_months_for_until("1987-12")

  expect_equal(
    out,
    data.frame(
      year = c(1987L, 1987L, 1987L),
      month = c(10L, 11L, 12L)
    )
  )
})

test_that("database month download uses BTS monthly downloader", {
  months <- data.frame(
    year = c(1987L, 1987L),
    month = c(10L, 11L)
  )

  calls <- list()

  testthat::local_mocked_bindings(
    download_bts_ontime_month = function(year,
                                         month,
                                         overwrite = FALSE,
                                         keep_zip = TRUE) {
      calls[[length(calls) + 1L]] <<- list(
        year = year,
        month = month,
        overwrite = overwrite,
        keep_zip = keep_zip
      )

      paste0(year, "-", month, ".csv")
    },
    .package = "slcflights"
  )

  out <- slcflights:::download_db_months(
    months = months,
    overwrite = TRUE
  )

  expect_equal(out, c("1987-10.csv", "1987-11.csv"))
  expect_equal(length(calls), 2L)
  expect_true(calls[[1L]]$overwrite)
  expect_true(calls[[1L]]$keep_zip)
})

test_that("database metadata downloads write to raw cache paths", {
  testthat::local_mocked_bindings(
    download_bts_master_coords = function(destfile, overwrite = FALSE) {
      expect_equal(
        destfile,
        slcflights:::slc_cache_raw_coords_path(create = TRUE)
      )
      expect_false(overwrite)
      destfile
    },
    download_bts_airline_id = function(destfile, overwrite = FALSE) {
      expect_equal(
        destfile,
        slcflights:::slc_cache_raw_airlines_path(create = TRUE)
      )
      expect_false(overwrite)
      destfile
    },
    .package = "slcflights"
  )

  expect_equal(
    slcflights:::download_db_coords(overwrite = FALSE),
    slcflights:::slc_cache_raw_coords_path(create = TRUE)
  )

  expect_equal(
    slcflights:::download_db_airlines(overwrite = FALSE),
    slcflights:::slc_cache_raw_airlines_path(create = TRUE)
  )
})

test_that("database build passes raw metadata paths to staging", {
  months <- data.frame(year = 1987L, month = 10L)
  root <- tempfile("slc-db-cache-")

  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    slc_cache_root = function(create = TRUE) {
      if (isTRUE(create)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
      }

      root
    },
    db_stage_build = function(months,
                              root,
                              coords_in,
                              airlines_in) {
      expect_equal(months, data.frame(year = 1987L, month = 10L))
      expect_equal(
        root,
        slcflights:::slc_db_staging_root(create = TRUE)
      )
      expect_equal(
        coords_in,
        slcflights:::slc_cache_raw_coords_path(create = FALSE)
      )
      expect_equal(
        airlines_in,
        slcflights:::slc_cache_raw_airlines_path(create = FALSE)
      )

      invisible(TRUE)
    },
    db_stage_promote = function(staging_root, active_root) {
      expect_equal(
        staging_root,
        slcflights:::slc_db_staging_root(create = FALSE)
      )
      expect_equal(
        active_root,
        slcflights:::slc_db_root(create = FALSE)
      )

      invisible(active_root)
    },
    .package = "slcflights"
  )

  out <- suppressMessages(
    slcflights:::build_db_cache(months)
  )

  expect_equal(out, slcflights:::slc_db_root(create = FALSE))
})

test_that("database info reports absent database", {
  root <- tempfile("slc-db-info-")

  info <- slcflights:::slc_db_info(root = root)

  expect_equal(
    info$root,
    normalizePath(root, winslash = "/", mustWork = FALSE)
  )
  expect_false(info$exists)
  expect_false(info$complete)
  expect_equal(
    info$months,
    data.frame(year = integer(), month = integer())
  )
  expect_null(info$endpoint)
  expect_null(info$manifest)
})

test_that("database info reports present database", {
  root <- tempfile("slc-db-info-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  main <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = root,
    create = TRUE
  )

  coords <- slcflights:::slc_db_coords_path(
    root = root,
    create = TRUE
  )

  airlines <- slcflights:::slc_db_airlines_path(
    root = root,
    create = TRUE
  )

  writeLines("main", main)
  writeLines("coords", coords)
  writeLines("airlines", airlines)

  info <- slcflights:::slc_db_info(root = root)

  expect_true(info$exists)
  expect_true(info$complete)
  expect_equal(info$endpoint$year, 1987L)
  expect_equal(info$endpoint$month, 10L)
  expect_equal(info$manifest$db_end, "1987-10")

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("large initial database build requires confirmation", {
  testthat::local_mocked_bindings(
    read_db_manifest = function(root = NULL) NULL,
    .package = "slcflights"
  )

  expect_error(
    suppressMessages(
      slcflights:::build_slcflights_db(until = "1989-12")
    ),
    "confirm = TRUE"
  )
})

test_that("database build creates one-month active database", {
  root <- tempfile("slc-db-build-")

  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    slc_cache_root = function(create = TRUE) {
      if (isTRUE(create)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
      }

      root
    },
    download_bts_ontime_month = function(year,
                                         month,
                                         overwrite = FALSE,
                                         keep_zip = TRUE) {
      expect_equal(year, 1987L)
      expect_equal(month, 10L)
      expect_false(overwrite)
      expect_true(keep_zip)

      month_dir <- slcflights:::slc_cache_raw_ontime_month_dir(
        year,
        month,
        create = TRUE
      )

      path <- file.path(month_dir, "bts_ontime.csv")
      make_stage_bts_csv(path)
    },
    download_bts_master_coords = function(destfile, overwrite = FALSE) {
      expect_equal(
        destfile,
        slcflights:::slc_cache_raw_coords_path(create = TRUE)
      )
      expect_false(overwrite)

      make_stage_coords_csv(destfile)
    },
    download_bts_airline_id = function(destfile, overwrite = FALSE) {
      expect_equal(
        destfile,
        slcflights:::slc_cache_raw_airlines_path(create = TRUE)
      )
      expect_false(overwrite)

      make_stage_airlines_csv(destfile)
    },
    .package = "slcflights"
  )

  info <- suppressMessages(
    slcflights:::build_slcflights_db(
      until = "1987-10",
      confirm = TRUE
    )
  )

  expect_true(info$exists)
  expect_true(info$complete)
  expect_equal(info$endpoint$year, 1987L)
  expect_equal(info$endpoint$month, 10L)
  expect_equal(info$manifest$db_start, "1987-10")
  expect_equal(info$manifest$db_end, "1987-10")

  main <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = slcflights:::slc_db_root(create = FALSE),
    create = FALSE
  )

  coords <- slcflights:::slc_db_coords_path(
    root = slcflights:::slc_db_root(create = FALSE),
    create = FALSE
  )

  airlines <- slcflights:::slc_db_airlines_path(
    root = slcflights:::slc_db_root(create = FALSE),
    create = FALSE
  )

  manifest <- slcflights:::slc_db_manifest_path(
    root = slcflights:::slc_db_root(create = FALSE),
    create = FALSE
  )

  expect_true(file.exists(main))
  expect_true(file.exists(coords))
  expect_true(file.exists(airlines))
  expect_true(file.exists(manifest))

  x <- arrow::read_parquet(main)

  expect_equal(nrow(x), 2L)
  expect_true("OriginLatitude" %in% names(x))
  expect_true("DestLatitude" %in% names(x))
  expect_true("Reporting_Airline_Name" %in% names(x))
  expect_true("Reporting_Airline_Lookup_Code" %in% names(x))
})
