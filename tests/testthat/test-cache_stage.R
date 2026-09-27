test_that("staging prepare clears and recreates directory", {
  root <- tempfile("slc-cache-stage-")
  dir.create(root, recursive = TRUE)
  writeLines("x", file.path(root, "old.txt"))

  out <- slcflights:::cache_stage_prepare(root)

  expect_equal(out, root)
  expect_true(dir.exists(root))
  expect_false(file.exists(file.path(root, "old.txt")))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("staging months split by year", {
  months <- data.frame(
    year = c(rep(2024L, 6L), rep(2025L, 2L)),
    month = c(7:12, 1:2)
  )

  out <- slcflights:::cache_stage_months_by_year(months)

  expect_equal(names(out), c("2024", "2025"))
  expect_equal(out[["2024"]], 7:12)
  expect_equal(out[["2025"]], 1:2)
})

test_that("database staging months split by year", {
  months <- data.frame(
    year = c(rep(1987L, 3L), rep(1988L, 2L)),
    month = c(10:12, 1:2)
  )

  out <- slcflights:::db_stage_months_by_year(months)

  expect_equal(names(out), c("1987", "1988"))
  expect_equal(out[["1987"]], 10:12)
  expect_equal(out[["1988"]], 1:2)
})

test_that("staging parquet files returns existing cache files only", {
  root <- tempfile("slc-cache-stage-")

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  div <- slcflights:::slc_cache_parquet_path(
    "div",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("main", main)

  out <- slcflights:::cache_stage_parquet_files(
    root = root,
    years = 2024
  )

  expect_equal(out, main)

  writeLines("div", div)

  out <- slcflights:::cache_stage_parquet_files(
    root = root,
    years = 2024
  )

  expect_equal(out, c(main, div))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("staging coord files use staged cache files", {
  root <- tempfile("slc-cache-stage-")

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  div <- slcflights:::slc_cache_parquet_path(
    "div",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("main", main)
  writeLines("div", div)

  out <- slcflights:::cache_stage_coord_files(
    root = root,
    years = 2024
  )

  expect_equal(
    sort(out),
    sort(c(main, div))
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("staging build creates annual files, coordinates, and manifest", {
  root <- tempfile("slc-cache-stage-")
  csv <- tempfile("slc-stage-bts-", fileext = ".csv")
  coords <- tempfile("slc-stage-coords-", fileext = ".csv")
  airlines <- tempfile("slc-stage-airlines-", fileext = ".csv")

  make_stage_bts_csv(csv)
  make_stage_coords_csv(coords)
  make_stage_airlines_csv(airlines)

  out <- slcflights:::cache_stage_build(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    coords_in = coords,
    airlines_in = airlines,
    csv_files = list("2024" = csv),
    finalize_schema = FALSE
  )

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = FALSE
  )

  coords_out <- slcflights:::slc_cache_coords_path(
    root = root,
    create = FALSE
  )

  airlines_out <- slcflights:::slc_cache_airlines_path(
    root = root,
    create = FALSE
  )

  manifest <- slcflights:::slc_cache_manifest_path(
    root = root,
    create = FALSE
  )

  expect_true(file.exists(main))
  expect_true(file.exists(coords_out))
  expect_true(file.exists(airlines_out))
  expect_true(file.exists(manifest))
  expect_true(slcflights:::validate_cache_files(root = root))

  read <- arrow::read_parquet(main)

  expect_true("OriginLatitude" %in% names(read))
  expect_true("DestLatitude" %in% names(read))
  expect_true("Reporting_Airline_Name" %in% names(read))
  expect_true("Reporting_Airline_Lookup_Code" %in% names(read))
  expect_equal(nrow(read), 2L)
  expect_equal(
    read$Reporting_Airline_Name,
    c("First Airline Inc.", "Second Airline LLC")
  )
  expect_equal(
    read$Reporting_Airline_Lookup_Code,
    c("FA", "SB")
  )

  expect_equal(out$root, root)
  expect_true(main %in% out$files)
  expect_equal(out$coords, coords_out)
  expect_equal(out$airlines, airlines_out)
  expect_equal(out$manifest$cached_end, "2024-07")

  unlink(c(root, csv, coords, airlines), recursive = TRUE, force = TRUE)
})

test_that("database staging build creates files and manifest", {
  root <- tempfile("slc-db-stage-")
  csv <- tempfile("slc-stage-bts-", fileext = ".csv")
  coords <- tempfile("slc-stage-coords-", fileext = ".csv")
  airlines <- tempfile("slc-stage-airlines-", fileext = ".csv")

  make_stage_bts_csv(
    csv,
    year = 1987L,
    month = 10L
  )
  make_stage_coords_csv(coords)
  make_stage_airlines_csv(airlines)

  out <- slcflights:::db_stage_build(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    coords_in = coords,
    airlines_in = airlines,
    csv_files = list("1987" = csv),
    finalize_schema = FALSE
  )

  main <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = root,
    create = FALSE
  )

  coords_out <- slcflights:::slc_db_coords_path(
    root = root,
    create = FALSE
  )

  airlines_out <- slcflights:::slc_db_airlines_path(
    root = root,
    create = FALSE
  )

  manifest <- slcflights:::slc_db_manifest_path(
    root = root,
    create = FALSE
  )

  expect_true(file.exists(main))
  expect_true(file.exists(coords_out))
  expect_true(file.exists(airlines_out))
  expect_true(file.exists(manifest))
  expect_true(slcflights:::validate_db_files(root = root))

  read <- arrow::read_parquet(main)

  expect_true("OriginLatitude" %in% names(read))
  expect_true("DestLatitude" %in% names(read))
  expect_true("Reporting_Airline_Name" %in% names(read))
  expect_true("Reporting_Airline_Lookup_Code" %in% names(read))
  expect_equal(nrow(read), 2L)

  expect_equal(out$root, root)
  expect_true(main %in% out$files)
  expect_equal(out$coords, coords_out)
  expect_equal(out$airlines, airlines_out)
  expect_equal(out$manifest$db_start, "1987-10")
  expect_equal(out$manifest$db_end, "1987-10")

  unlink(c(root, csv, coords, airlines), recursive = TRUE, force = TRUE)
})

test_that("database staging validation requires database manifest", {
  root <- tempfile("slc-db-stage-")
  dir.create(root, recursive = TRUE)

  expect_error(
    slcflights:::db_stage_validate(root),
    "database appears incomplete|manifest"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("staging promotion requires existing staging directory", {
  root <- tempfile("slc-cache-stage-")
  active <- tempfile("slc-cache-active-")

  expect_error(
    slcflights:::cache_stage_promote(
      staging_root = root,
      active_root = active
    ),
    "Staging cache directory not found"
  )
})

test_that("staging promotion moves validated staging to active", {
  parent <- tempfile("slc-cache-stage-parent-")
  staging <- file.path(parent, "staging")
  active <- file.path(parent, "active")

  csv <- tempfile("slc-stage-bts-", fileext = ".csv")
  coords <- tempfile("slc-stage-coords-", fileext = ".csv")
  airlines <- tempfile("slc-stage-airlines-", fileext = ".csv")

  make_stage_bts_csv(csv)
  make_stage_coords_csv(coords)
  make_stage_airlines_csv(airlines)

  slcflights:::cache_stage_build(
    months = data.frame(year = 2024L, month = 7L),
    root = staging,
    coords_in = coords,
    airlines_in = airlines,
    csv_files = list("2024" = csv),
    finalize_schema = FALSE
  )

  out <- slcflights:::cache_stage_promote(
    staging_root = staging,
    active_root = active
  )

  expect_equal(out, active)
  expect_false(dir.exists(staging))
  expect_true(dir.exists(active))
  expect_true(slcflights:::validate_cache_files(root = active))

  unlink(c(parent, csv, coords, airlines), recursive = TRUE, force = TRUE)
})

test_that("database staging promotion activates staged database", {
  staging <- tempfile("slc-db-staging-")
  active <- tempfile("slc-db-active-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = staging,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  main <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = staging,
    create = TRUE
  )

  coords <- slcflights:::slc_db_coords_path(
    root = staging,
    create = TRUE
  )

  airlines <- slcflights:::slc_db_airlines_path(
    root = staging,
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

  out <- slcflights:::db_stage_promote(
    staging_root = staging,
    active_root = active
  )

  expect_equal(out, active)
  expect_false(dir.exists(staging))
  expect_true(dir.exists(active))
  expect_true(slcflights:::validate_db_files(root = active))

  unlink(active, recursive = TRUE, force = TRUE)
})

test_that("database staging extension rebuilds affected years", {
  active <- tempfile("slc-db-active-")
  staging <- tempfile("slc-db-staging-")
  csv_oct <- tempfile("slc-stage-oct-", fileext = ".csv")
  csv_nov <- tempfile("slc-stage-nov-", fileext = ".csv")
  coords <- tempfile("slc-stage-coords-", fileext = ".csv")
  airlines <- tempfile("slc-stage-airlines-", fileext = ".csv")

  make_stage_bts_csv(
    csv_oct,
    year = 1987L,
    month = 10L
  )

  make_stage_bts_csv(
    csv_nov,
    year = 1987L,
    month = 11L
  )

  make_stage_coords_csv(coords)
  make_stage_airlines_csv(airlines)

  slcflights:::db_stage_build(
    months = data.frame(year = 1987L, month = 10L),
    root = active,
    coords_in = coords,
    airlines_in = airlines,
    csv_files = list("1987" = csv_oct),
    finalize_schema = FALSE
  )

  out <- slcflights:::db_stage_extend(
    months = data.frame(
      year = c(1987L, 1987L),
      month = c(10L, 11L)
    ),
    extend_months = data.frame(year = 1987L, month = 11L),
    active_root = active,
    root = staging,
    coords_in = coords,
    airlines_in = airlines,
    csv_files = list("1987" = c(csv_oct, csv_nov)),
    finalize_schema = FALSE
  )

  main <- slcflights:::slc_db_parquet_path(
    "main",
    1987,
    root = staging,
    create = FALSE
  )

  expect_true(file.exists(main))
  expect_true(slcflights:::validate_db_files(root = staging))
  expect_equal(out$manifest$db_start, "1987-10")
  expect_equal(out$manifest$db_end, "1987-11")

  x <- arrow::read_parquet(main)

  expect_equal(nrow(x), 4L)
  expect_true("OriginLatitude" %in% names(x))
  expect_true("Reporting_Airline_Name" %in% names(x))

  unlink(
    c(active, staging, csv_oct, csv_nov, coords, airlines),
    recursive = TRUE,
    force = TRUE
  )
})
