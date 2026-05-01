test_that("installed extdata paths exist", {
  expect_true(dir.exists(slcflights:::slc_installed_extdata_root()))
  expect_true(dir.exists(slcflights:::slc_installed_parquet_root()))
  expect_true(dir.exists(slcflights:::slc_installed_csv_root()))
  expect_true(file.exists(slcflights:::slc_installed_coords_path()))
})

test_that("installed parquet paths follow package annual layout", {
  path <- slcflights:::slc_installed_parquet_path("main", 1987)

  expect_match(path, "Year=1987")
  expect_match(path, "data_0_main[.]parquet$")
})

test_that("installed available years discover main files", {
  years <- slcflights:::slc_available_installed_years("main")

  expect_true(1987L %in% years)
  expect_true(2024L %in% years)
  expect_true(all(years == sort(unique(years))))
})

test_that("installed available years discover diversion files separately", {
  years <- slcflights:::slc_available_installed_years("div")

  expect_type(years, "integer")
  expect_true(all(years == sort(unique(years))))
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

test_that("combined available years include installed and cached years", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_cache_manifest(
    months = data.frame(
      year = c(rep(2024L, 6L), 2025L),
      month = c(7:12, 1L)
    ),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  path_2024 <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  path_2025 <- slcflights:::slc_cache_parquet_path(
    "main",
    2025,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", path_2024)
  writeLines("not real parquet", path_2025)

  years <- slcflights:::slc_available_data_years("main", root = root)

  expect_true(1987L %in% years)
  expect_true(2024L %in% years)
  expect_true(2025L %in% years)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("installed data paths return installed files only", {
  paths <- slcflights:::slc_installed_data_paths("main", years = 1987)

  expect_equal(nrow(paths), 1L)
  expect_equal(paths$year, 1987L)
  expect_equal(paths$source, "installed")
  expect_true(file.exists(paths$path))
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

test_that("combined data paths order installed path before cache overlay", {
  root <- tempfile("slc-data-paths-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  cache_path <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", cache_path)

  paths <- slcflights:::slc_data_paths(
    "main",
    years = 2024,
    root = root
  )

  expect_equal(paths$source, c("installed", "cache"))
  expect_equal(paths$year, c(2024L, 2024L))
  expect_equal(paths$path[[2]], cache_path)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("combined data paths error clearly when no requested year exists", {
  expect_error(
    slcflights:::slc_data_paths("main", years = 3000),
    "No main Parquet files are available"
  )
})

test_that("coordinate path prefers cache only when manifest and file exist", {
  root <- tempfile("slc-data-paths-")

  installed <- slcflights:::slc_installed_coords_path()

  expect_equal(
    slcflights:::slc_coords_path(root = root),
    installed
  )

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_equal(
    slcflights:::slc_coords_path(root = root),
    installed
  )

  cached <- slcflights:::slc_cache_coords_path(
    root = root,
    create = TRUE
  )

  writeLines("coords", cached)

  expect_equal(
    slcflights:::slc_coords_path(root = root),
    cached
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
