test_that("cache root uses R_user_dir", {
  root <- slcflights:::slc_cache_root(create = FALSE)

  expect_match(root, "slcflights")
  expect_false(grepl("inst[/\\\\]extdata", root))
  expect_false(grepl("data-raw[/\\\\]cache", root))
})

test_that("active, staging, and raw cache roots are under cache root", {
  root <- slcflights:::slc_cache_root(create = FALSE)

  expect_true(startsWith(
    slcflights:::slc_cache_active_root(create = FALSE),
    root
  ))

  expect_true(startsWith(
    slcflights:::slc_cache_staging_root(create = FALSE),
    root
  ))

  expect_true(startsWith(
    slcflights:::slc_cache_raw_root(create = FALSE),
    root
  ))
})

test_that("cache extdata structure mirrors package structure", {
  root <- tempfile("slc-cache-test-")

  expect_equal(
    slcflights:::slc_cache_parquet_root(root = root, create = FALSE),
    file.path(root, "extdata", "parquet")
  )

  expect_equal(
    slcflights:::slc_cache_csv_root(root = root, create = FALSE),
    file.path(root, "extdata", "csv")
  )

  expect_equal(
    slcflights:::slc_cache_manifest_path(root = root, create = FALSE),
    file.path(root, "manifest.json")
  )
})

test_that("cache parquet paths preserve annual layout", {
  root <- tempfile("slc-cache-test-")

  expect_equal(
    slcflights:::slc_cache_parquet_path(
      "main",
      2025,
      root = root,
      create = FALSE
    ),
    file.path(
      root,
      "extdata",
      "parquet",
      "Year=2025",
      "data_0_main.parquet"
    )
  )

  expect_equal(
    slcflights:::slc_cache_parquet_path(
      "div",
      2025,
      root = root,
      create = FALSE
    ),
    file.path(
      root,
      "extdata",
      "parquet",
      "Year=2025",
      "data_0_div.parquet"
    )
  )
})

test_that("cache coordinate path preserves package filename", {
  root <- tempfile("slc-cache-test-")

  expect_equal(
    slcflights:::slc_cache_coords_path(root = root, create = FALSE),
    file.path(
      root,
      "extdata",
      "csv",
      "T_MASTER_CORD_reduced.csv"
    )
  )
})

test_that("cache airline path preserves package filename", {
  root <- tempfile("slc-cache-test-")

  expect_equal(
    slcflights:::slc_cache_airlines_path(root = root, create = FALSE),
    file.path(
      root,
      "extdata",
      "csv",
      "L_AIRLINE_ID_reduced.csv"
    )
  )
})

test_that("raw cache paths are separated from active extdata", {
  root <- slcflights:::slc_cache_root(create = FALSE)

  ontime <- slcflights:::slc_cache_raw_ontime_root(create = FALSE)
  coords <- slcflights:::slc_cache_raw_coords_root(create = FALSE)
  airlines <- slcflights:::slc_cache_raw_airlines_root(create = FALSE)

  expect_true(startsWith(ontime, root))
  expect_true(startsWith(coords, root))
  expect_true(startsWith(airlines, root))

  expect_match(ontime, "raw[/\\\\]bts_ontime")
  expect_match(coords, "raw[/\\\\]bts_coords")
  expect_match(airlines, "raw[/\\\\]bts_airlines")
})

test_that("raw monthly on-time directory uses YYYY_MM format", {
  path <- slcflights:::slc_cache_raw_ontime_month_dir(
    2024,
    7,
    create = FALSE
  )

  expect_match(path, "2024_07$")
})

test_that("raw coordinate path uses expected filename", {
  path <- slcflights:::slc_cache_raw_coords_path(create = FALSE)

  expect_match(path, "T_MASTER_CORD[.]csv$")
})

test_that("raw airline path uses expected filename", {
  path <- slcflights:::slc_cache_raw_airlines_path(create = FALSE)

  expect_match(path, "L_AIRLINE_ID[.]csv$")
})

test_that("cache year validation rejects invalid years", {
  expect_error(
    slcflights:::normalize_cache_year(c(2024, 2025)),
    "exactly one year"
  )

  expect_error(
    slcflights:::normalize_cache_year(2024.5),
    "whole-number"
  )

  expect_error(
    slcflights:::normalize_cache_year(NA),
    "non-missing numeric"
  )

  expect_error(
    slcflights:::normalize_cache_year(24),
    "four-digit"
  )
})

test_that("create = TRUE creates requested directories", {
  root <- tempfile("slc-cache-test-")

  path <- slcflights:::slc_cache_parquet_path(
    "main",
    2025,
    root = root,
    create = TRUE
  )

  expect_true(dir.exists(dirname(path)))

  unlink(root, recursive = TRUE, force = TRUE)
})
