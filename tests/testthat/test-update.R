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

test_that("BTS candidate months use a recent lookback window", {
  out <- slcflights:::bts_candidate_months(
    today = as.Date("2024-09-15"),
    lookback = 4L
  )

  expect_equal(
    out,
    data.frame(
      year = c(2024L, 2024L, 2024L, 2024L),
      month = c(9L, 8L, 7L, 6L)
    )
  )
})

test_that("explicit update endpoint resolves without network probing", {
  out <- slcflights:::resolve_update_until("2024-07")

  expect_equal(out$year, 2024L)
  expect_equal(out$month, 7L)
})

test_that("update months for explicit endpoint use database sequence", {
  out <- slcflights:::update_months_for_until("1987-12")

  expect_equal(
    out,
    data.frame(
      year = c(1987L, 1987L, 1987L),
      month = c(10L, 11L, 12L)
    )
  )
})

test_that("update endpoint before database start fails clearly", {
  expect_error(
    slcflights:::update_months_for_until("1987-09"),
    "must begin with October 1987"
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
                                 airlines_in) {
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

  out_dir <- normalizePath(dirname(out), winslash = "/", mustWork = TRUE)
  root_dir <- normalizePath(root, winslash = "/", mustWork = TRUE)

  expect_equal(out_dir, root_dir)
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

  arrow::write_parquet(
    data.frame(
      Year = 1987L,
      Month = 10L
    ),
    main
  )
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

test_that("large database operation accepts programmatic confirmation", {
  out <- slcflights:::confirm_large_db_operation(
    n_months = 25L,
    confirm = TRUE
  )

  expect_true(out)
})

test_that("large database operation requires confirmation in tests", {
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  expect_error(
    slcflights:::confirm_large_db_operation(
      n_months = 25L,
      confirm = FALSE
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
      make_stage_bts_csv(
        path,
        year = year,
        month = month
      )
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

test_that("database build refuses to overwrite active database", {
  root <- tempfile("slc-db-build-")

  testthat::local_mocked_bindings(
    slc_db_root = function(create = TRUE) {
      root
    },
    .package = "slcflights"
  )

  months <- data.frame(
    year = 1987L,
    month = 10L
  )

  slcflights:::write_db_manifest(
    months = months,
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_error(
    slcflights:::build_slcflights_db(
      until = "1987-11",
      confirm = TRUE
    ),
    "Use `update_slcflights_db\\(\\)` to extend it"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("download month validation allows extension months", {
  months <- slcflights:::validate_download_months(
    data.frame(
      year = c(2024L, 2024L),
      month = c(7L, 8L)
    )
  )

  expect_equal(
    months,
    data.frame(
      year = c(2024L, 2024L),
      month = c(7L, 8L)
    )
  )
})

test_that("download month validation rejects duplicates", {
  expect_error(
    slcflights:::validate_download_months(
      data.frame(
        year = c(2024L, 2024L),
        month = c(7L, 7L)
      )
    ),
    "duplicate"
  )
})

test_that("database info ignores old cache manifests", {
  root <- tempfile("slc-db-info-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  info <- slcflights:::slc_db_info(root = root)

  expect_false(info$exists)
  expect_false(info$complete)
  expect_equal(info$months, data.frame(year = integer(), month = integer()))
  expect_null(info$endpoint)
  expect_null(info$manifest)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("update_slcflights_db requires active database", {
  root <- tempfile("slc-db-update-")

  testthat::local_mocked_bindings(
    slc_db_root = function(create = TRUE) {
      root
    },
    .package = "slcflights"
  )

  expect_error(
    slcflights::update_slcflights_db(
      until = "2025-06",
      confirm = TRUE
    ),
    "Use `build_slcflights_db\\(\\)` before calling"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("update_slcflights_db delegates to database engine", {
  root <- tempfile("slc-db-update-")
  called <- FALSE
  got_until <- NULL
  got_overwrite <- NULL
  got_confirm <- NULL

  testthat::local_mocked_bindings(
    slc_db_root = function(create = TRUE) {
      root
    },
    run_db_build = function(until,
                            overwrite = FALSE,
                            confirm = FALSE) {
      called <<- TRUE
      got_until <<- until
      got_overwrite <<- overwrite
      got_confirm <<- confirm

      invisible(list(ok = TRUE))
    },
    .package = "slcflights"
  )

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  out <- slcflights::update_slcflights_db(
    until = "2025-06",
    overwrite = TRUE,
    confirm = TRUE
  )

  expect_true(called)
  expect_equal(got_until, "2025-06")
  expect_true(got_overwrite)
  expect_true(got_confirm)
  expect_equal(out, list(ok = TRUE))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("failed same-year update preserves active annual database", {
  root <- withr::local_tempdir()
  withr::local_envvar(SLCFLIGHTS_TEST_CACHE_ROOT = root)

  active <- slcflights:::slc_db_root(create = TRUE)

  active_months <- slcflights:::db_months_for_until("2026-02")

  slcflights:::write_db_manifest(
    months = active_months,
    root = active,
    package_version = "0.0.0.9000",
    created_at = "2026-09-27T00:00:00Z"
  )

  main <- slcflights:::slc_db_parquet_path(
    "main",
    2026,
    root = active,
    create = TRUE
  )

  arrow::write_parquet(
    data.frame(
      Year = c(2026L, 2026L),
      Month = c(1L, 2L),
      Record = c("January", "February")
    ),
    main
  )

  manifest <- slcflights:::slc_db_manifest_path(
    root = active,
    create = FALSE
  )

  main_before <- unname(tools::md5sum(main))
  manifest_before <- unname(tools::md5sum(manifest))

  retained_zips <- character(2L)

  for (month in 1:2) {
    month_dir <- slcflights:::slc_cache_raw_ontime_month_dir(
      year = 2026,
      month = month,
      create = TRUE
    )

    zip <- file.path(
      month_dir,
      slcflights:::bts_ontime_zip_name(2026, month)
    )

    writeBin(charToRaw("retained"), zip)
    retained_zips[[month]] <- zip
  }

  expect_true(all(file.exists(retained_zips)))

  expect_length(
    slcflights:::cache_raw_ontime_csvs(
      year = 2026,
      months = 1:2
    ),
    0L
  )

  downloaded <- integer()

  testthat::local_mocked_bindings(
    download_bts_ontime_month = function(year,
                                         month,
                                         overwrite = FALSE,
                                         keep_zip = TRUE) {
      expect_equal(year, 2026L)
      expect_true(month %in% 3:7)
      expect_false(overwrite)
      expect_true(keep_zip)

      downloaded <<- c(downloaded, month)

      month_dir <- slcflights:::slc_cache_raw_ontime_month_dir(
        year = year,
        month = month,
        create = TRUE
      )

      path <- file.path(
        month_dir,
        "bts_ontime.csv"
      )

      make_stage_bts_csv(
        path,
        year = year,
        month = month
      )
    },
    download_bts_master_coords = function(
      destfile,
      overwrite = FALSE
    ) {
      expect_false(overwrite)
      make_stage_coords_csv(destfile)
    },
    download_bts_airline_id = function(
      destfile,
      overwrite = FALSE
    ) {
      expect_false(overwrite)
      make_stage_airlines_csv(destfile)
    },
    .package = "slcflights"
  )

  expect_error(
    suppressMessages(
      slcflights::update_slcflights_db(
        until = "2026-07",
        confirm = TRUE
      )
    ),
    "Raw BTS on-time CSV data are missing for 2026-01.",
    fixed = TRUE
  )

  expect_equal(downloaded, 3:7)

  expect_identical(
    unname(tools::md5sum(main)),
    main_before
  )

  expect_identical(
    unname(tools::md5sum(manifest)),
    manifest_before
  )

  active_data <- arrow::read_parquet(main)

  expect_equal(
    sort(unique(active_data$Month)),
    c(1L, 2L)
  )

  active_manifest <- slcflights:::read_db_manifest_if_active(
    root = active
  )

  expect_equal(active_manifest$db_end, "2026-02")
})

test_that("status_slcflights_db reports database info", {
  info <- list(
    root = "db-root",
    exists = TRUE,
    complete = TRUE,
    months = data.frame(year = 1987L, month = 10L),
    endpoint = slcflights:::as_year_month(1987L, 10L),
    manifest = list(db_start = "1987-10", db_end = "1987-10")
  )

  testthat::local_mocked_bindings(
    slc_db_info = function(root = NULL) {
      info
    },
    print_db_info = function(info) {
      invisible(info)
    },
    .package = "slcflights"
  )

  out <- slcflights::status_slcflights_db()

  expect_equal(out, info)
})

test_that("delete_slcflights_db clears database root", {
  got_root <- NULL
  got_confirm <- NULL

  testthat::local_mocked_bindings(
    slc_db_root = function(create = TRUE) {
      "db-root"
    },
    clear_cache_root = function(root, confirm = interactive()) {
      got_root <<- root
      got_confirm <<- confirm

      invisible(TRUE)
    },
    .package = "slcflights"
  )

  out <- slcflights::delete_slcflights_db(confirm = FALSE)

  expect_true(out)
  expect_equal(got_root, "db-root")
  expect_false(got_confirm)
})
