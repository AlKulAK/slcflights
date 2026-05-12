test_that("cache months must be a data frame with year and month", {
  expect_error(
    slcflights:::validate_cache_months(list(year = 2024L, month = 7L)),
    "data frame"
  )

  expect_error(
    slcflights:::validate_cache_months(data.frame(year = 2024L)),
    "year` and `month"
  )

  expect_error(
    slcflights:::validate_cache_months(
      data.frame(year = integer(), month = integer())
    ),
    "at least one month"
  )
})

test_that("cache months must begin with July 2024", {
  expect_error(
    slcflights:::validate_cache_months(
      data.frame(year = 2024L, month = 8L)
    ),
    "begin with July 2024"
  )

  expect_error(
    slcflights:::validate_cache_months(
      data.frame(year = 2025L, month = 1L)
    ),
    "begin with July 2024"
  )
})

test_that("cache months must be consecutive", {
  expect_error(
    slcflights:::validate_cache_months(
      data.frame(
        year = c(2024L, 2024L),
        month = c(7L, 9L)
      )
    ),
    "consecutive"
  )

  expect_error(
    slcflights:::validate_cache_months(
      data.frame(
        year = c(2024L, 2024L),
        month = c(7L, 7L)
      )
    ),
    "consecutive"
  )
})

test_that("valid cache months are sorted and normalized", {
  months <- slcflights:::validate_cache_months(
    data.frame(
      year = c(2024, 2024),
      month = c(8, 7)
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

test_that("new cache manifest records expected boundaries", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_equal(manifest$schema_version, slcflights:::slc_schema_version)
  expect_equal(manifest$package_version, "0.0.0.9000")
  expect_equal(manifest$bundled_start, "1987-10")
  expect_equal(manifest$bundled_end, "2024-06")
  expect_equal(manifest$cached_start, "2024-07")
  expect_equal(manifest$cached_end, "2024-07")
  expect_equal(manifest$created_at, "2026-05-01T00:00:00Z")
  expect_equal(length(manifest$months), 1L)
  expect_equal(manifest$months[[1]]$status, "complete")
})

test_that("manifest validation rejects missing fields", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  manifest$cached_end <- NULL

  expect_error(
    slcflights:::validate_cache_manifest(manifest),
    "missing required field"
  )
})

test_that("manifest validation rejects incompatible schema versions", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  manifest$schema_version <- slcflights:::slc_schema_version + 1L

  expect_error(
    slcflights:::validate_cache_manifest(manifest),
    "incompatible schema version"
  )
})

test_that("manifest validation rejects inconsistent cache endpoint", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  manifest$cached_end <- "2024-08"

  expect_error(
    slcflights:::validate_cache_manifest(manifest),
    "inconsistent cached end"
  )
})

test_that("cache months can be extracted from manifest", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(
      year = c(2024L, 2024L, 2024L),
      month = c(7L, 8L, 9L)
    ),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_equal(
    slcflights:::cache_months_from_manifest(manifest),
    data.frame(
      year = c(2024L, 2024L, 2024L),
      month = c(7L, 8L, 9L)
    )
  )
})

test_that("cache endpoint can be extracted from manifest", {
  manifest <- slcflights:::new_cache_manifest(
    months = data.frame(
      year = c(2024L, 2024L, 2024L),
      month = c(7L, 8L, 9L)
    ),
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  endpoint <- slcflights:::cache_endpoint_from_manifest(manifest)

  expect_equal(endpoint$year, 2024L)
  expect_equal(endpoint$month, 9L)
})

test_that("cache manifest round-trips through JSON", {
  root <- tempfile("slc-cache-manifest-")

  manifest <- slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_true(file.exists(file.path(root, "manifest.json")))

  read <- slcflights:::read_cache_manifest(root = root)

  expect_equal(read$schema_version, manifest$schema_version)
  expect_equal(read$package_version, manifest$package_version)
  expect_equal(read$cached_start, manifest$cached_start)
  expect_equal(read$cached_end, manifest$cached_end)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("missing manifest reads as NULL", {
  root <- tempfile("slc-cache-manifest-")

  expect_null(slcflights:::read_cache_manifest(root = root))
})

test_that("cache completeness is false when files are missing", {
  root <- tempfile("slc-cache-manifest-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  expect_false(slcflights:::cache_months_are_complete(root = root))

  expect_error(
    slcflights:::validate_cache_files(root = root),
    "cache appears incomplete"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("cache completeness is true when required files exist", {
  root <- tempfile("slc-cache-manifest-")

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

  writeLines("not real parquet", main)
  writeLines("coords", coords)
  writeLines("airlines", airlines)

  expect_true(slcflights:::cache_months_are_complete(root = root))
  expect_true(slcflights:::validate_cache_files(root = root))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("cache completeness requires reduced Airline ID lookup", {
  root <- tempfile("slc-cache-manifest-")

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

  writeLines("not real parquet", main)
  writeLines("coords", coords)

  expect_false(slcflights:::cache_months_are_complete(root = root))

  expect_error(
    slcflights:::validate_cache_files(root = root),
    "cache appears incomplete"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})
