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

test_that("cache info reports absent cache", {
  root <- tempfile("slc-update-cache-")

  info <- slcflights:::slc_cache_info(root = root)

  expect_equal(info$root, root)
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

  writeLines("main", main)
  writeLines("coords", coords)

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
