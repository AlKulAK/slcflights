test_that("parquet sequence columns are discovered from a Parquet file", {
  root <- tempfile("slc-cache-coords-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(
      OriginAirportSeqID = 1L,
      DestAirportSeqID = 2L,
      Div1AirportSeqID = 3L,
      Other = "x"
    ),
    path
  )

  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_equal(
    slcflights:::cache_build_parquet_seq_cols(con, path),
    c(
      "OriginAirportSeqID",
      "DestAirportSeqID",
      "Div1AirportSeqID"
    )
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("used sequence column discovery requires files", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_used_seq_cols(con, character()),
    "at least one Parquet file"
  )
})

test_that("used sequence column discovery requires AirportSeqID columns", {
  root <- tempfile("slc-cache-coords-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(Other = "x"),
    path
  )

  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_used_seq_cols(con, path),
    "No AirportSeqID columns"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("sequence ID union SQL requires sequence columns", {
  root <- tempfile("slc-cache-coords-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(OriginAirportSeqID = 1L),
    path
  )

  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_seq_id_union_sql(con, path, character()),
    "at least one column name"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate column validation requires AIRPORT_SEQ_ID", {
  expect_error(
    slcflights:::cache_build_vldte_coords_cols("LATITUDE"),
    "AIRPORT_SEQ_ID"
  )

  expect_true(
    slcflights:::cache_build_vldte_coords_cols("AIRPORT_SEQ_ID")
  )
})

test_that("coordinate reduction requires parquet files", {
  coords <- tempfile("coords-", fileext = ".csv")
  writeLines("AIRPORT_SEQ_ID,LATITUDE\n1,40", coords)

  expect_error(
    slcflights:::cache_build_reduce_coords_csv(
      parquet_files = character(),
      coords_in = coords,
      coords_out = tempfile(fileext = ".csv")
    ),
    "at least one Parquet file"
  )

  unlink(coords)
})

test_that("coordinate reduction requires coordinate CSV", {
  root <- tempfile("slc-cache-coords-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(OriginAirportSeqID = 1L),
    path
  )

  expect_error(
    slcflights:::cache_build_reduce_coords_csv(
      parquet_files = path,
      coords_in = file.path(root, "missing.csv"),
      coords_out = file.path(root, "out.csv")
    ),
    "Coordinate CSV not found"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate reduction keeps only used sequence IDs in CSV order", {
  root <- tempfile("slc-cache-coords-")
  dir.create(root, recursive = TRUE)

  main <- file.path(root, "main.parquet")
  div <- file.path(root, "div.parquet")
  coords_in <- file.path(root, "T_MASTER_CORD.csv")
  coords_out <- file.path(root, "out", "T_MASTER_CORD_reduced.csv")

  arrow::write_parquet(
    data.frame(
      OriginAirportSeqID = c(2L, 4L),
      DestAirportSeqID = c(6L, NA_integer_)
    ),
    main
  )

  arrow::write_parquet(
    data.frame(
      Div1AirportSeqID = c(8L, 2L)
    ),
    div
  )

  writeLines(
    c(
      "AIRPORT_SEQ_ID,LATITUDE,LONGITUDE",
      "1,41.0,-111.0",
      "2,42.0,-112.0",
      "4,44.0,-114.0",
      "6,46.0,-116.0",
      "8,48.0,-118.0",
      "9,49.0,-119.0"
    ),
    coords_in
  )

  out <- slcflights:::cache_build_reduce_coords_csv(
    parquet_files = c(main, div),
    coords_in = coords_in,
    coords_out = coords_out
  )

  expect_equal(out, normalizePath(coords_out, mustWork = TRUE))

  reduced <- readr::read_csv(coords_out, show_col_types = FALSE)

  expect_equal(reduced$AIRPORT_SEQ_ID, c(2L, 4L, 6L, 8L))
  expect_equal(reduced$LATITUDE, c(42, 44, 46, 48))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("available cache build parquet files can include installed files", {
  root <- tempfile("slc-cache-coords-")

  slcflights:::write_cache_manifest(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  cache_main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("not real parquet", cache_main)

  with_installed <- slcflights:::cache_build_avail_pq_files(
    years = 2024,
    root = root,
    include_installed = TRUE
  )

  without_installed <- slcflights:::cache_build_avail_pq_files(
    years = 2024,
    root = root,
    include_installed = FALSE
  )

  expect_true(cache_main %in% with_installed)
  expect_true(cache_main %in% without_installed)
  expect_gt(length(with_installed), length(without_installed))

  unlink(root, recursive = TRUE, force = TRUE)
})
