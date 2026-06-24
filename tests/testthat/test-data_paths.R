test_that("installed CSV extdata paths exist", {
  field_dict <- system.file(
    "extdata",
    "csv",
    "field_dictionary.csv",
    package = "slcflights",
    mustWork = TRUE
  )

  expect_true(dir.exists(slcflights:::slc_installed_extdata_root()))
  expect_true(dir.exists(slcflights:::slc_installed_csv_root()))
  expect_true(file.exists(field_dict))
})

test_that("cached years are empty without a manifest", {
  root <- tempfile("slc-data-paths-")

  expect_equal(
    slcflights:::slc_available_cached_years("main", root = root),
    integer()
  )

  expect_equal(
    slcflights:::slc_cached_data_paths("main", root = root),
    data.frame(
      year = integer(),
      source = character(),
      path = character()
    )
  )
})

test_that("cached years require files listed by manifest years", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_equal(
    slcflights:::slc_available_cached_years("main", root = root),
    integer()
  )

  path <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", path)

  expect_equal(
    slcflights:::slc_available_cached_years("main", root = root),
    2024L
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("cached data paths return cache files only", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  path <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", path)

  paths <- slcflights:::slc_cached_data_paths(
    "main",
    years = 2024,
    root = root
  )

  expect_equal(nrow(paths), 1L)
  expect_equal(paths$year, 2024L)
  expect_equal(paths$source, "cache")
  expect_equal(paths$path, path)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("data paths error clearly when requested database year is missing", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_error(
    slcflights:::slc_data_paths("main", years = 3000, root = root),
    "No main Parquet files are available"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("database years are empty without a database manifest", {
  root <- tempfile("slc-data-paths-")

  expect_equal(
    slcflights:::slc_available_db_years("main", root = root),
    integer()
  )

  expect_equal(
    slcflights:::slc_db_data_paths("main", root = root),
    data.frame(
      year = integer(),
      source = character(),
      path = character()
    )
  )
})

test_that("database years require files listed by manifest years", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_equal(
    slcflights:::slc_available_db_years("main", root = root),
    integer()
  )

  path <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", path)

  expect_equal(
    slcflights:::slc_available_db_years("main", root = root),
    1987L
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate path requires database coordinate file", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_error(
    slcflights:::slc_coords_path(root = root),
    "missing its coordinate CSV"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("airline path requires database airline file", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_error(
    slcflights:::slc_airlines_path(root = root),
    "missing its Airline ID lookup CSV"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("database paths are preferred when database is active", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  path <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", path)

  paths <- slcflights:::slc_data_paths(
    "main",
    years = 1987,
    root = root
  )

  expect_equal(nrow(paths), 1L)
  expect_equal(paths$year, 1987L)
  expect_equal(paths$source, "database")
  expect_equal(paths$path, path)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("database coordinate path is preferred when active", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  db_path <- slcflights:::slc_db_coords_path(
    root = root,
    create = TRUE
  )

  writeLines("coords", db_path)

  expect_equal(
    slcflights:::slc_coords_path(root = root),
    db_path
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("database airline path is preferred when active", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  db_path <- slcflights:::slc_db_airlines_path(
    root = root,
    create = TRUE
  )

  writeLines("airlines", db_path)

  expect_equal(
    slcflights:::slc_airlines_path(root = root),
    db_path
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("data year normalization validates years", {
  expect_equal(
    slcflights:::normalize_data_years(c(2025, 2024, 2024)),
    c(2024L, 2025L)
  )

  expect_error(
    slcflights:::normalize_data_years(numeric()),
    "at least one year"
  )

  expect_error(
    slcflights:::normalize_data_years("2024"),
    "numeric vector"
  )

  expect_error(
    slcflights:::normalize_data_years(2024.5),
    "whole-number"
  )

  expect_error(
    slcflights:::normalize_data_years(NA),
    "missing values"
  )
})

test_that("data years require an active database", {
  root <- tempfile("slc-data-paths-")

  expect_error(
    slcflights:::slc_available_data_years("main", root = root),
    "No local slcflights database is active"
  )
})

test_that("data paths require an active database", {
  root <- tempfile("slc-data-paths-")

  expect_error(
    slcflights:::slc_data_paths("main", years = 1987, root = root),
    "No local slcflights database is active"
  )
})

test_that("coordinate path requires an active database", {
  root <- tempfile("slc-data-paths-")

  expect_error(
    slcflights:::slc_coords_path(root = root),
    "No local slcflights database is active"
  )
})

test_that("airline path requires an active database", {
  root <- tempfile("slc-data-paths-")

  expect_error(
    slcflights:::slc_airlines_path(root = root),
    "No local slcflights database is active"
  )
})
