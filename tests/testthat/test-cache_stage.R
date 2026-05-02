make_stage_coords_csv <- function(path) {
  writeLines(
    c(
      paste(
        "AIRPORT_SEQ_ID",
        "LATITUDE",
        "LONGITUDE",
        "AIRPORT_START_DATE",
        "AIRPORT_THRU_DATE",
        "AIRPORT_IS_CLOSED",
        "AIRPORT_IS_LATEST",
        sep = ","
      ),
      "1,40.1,-111.1,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "2,40.2,-111.2,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "3,40.3,-111.3,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "4,40.4,-111.4,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1"
    ),
    path
  )

  path
}

make_stage_bts_csv <- function(path) {
  rows <- data.frame(
    FlightDate = c("2024-07-02", "2024-07-01", "2024-07-03"),
    CRSDepTime = c(900L, 800L, 700L),
    OriginAirportID = c(14869L, 11111L, 22222L),
    DestAirportID = c(33333L, 14869L, 44444L),
    OriginAirportSeqID = c(1L, 2L, 3L),
    DestAirportSeqID = c(2L, 3L, 4L)
  )

  utils::write.csv(
    rows,
    path,
    row.names = FALSE,
    na = ""
  )

  path
}

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

test_that("staging coord files can exclude installed files", {
  root <- tempfile("slc-cache-stage-")

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  writeLines("main", main)

  out <- slcflights:::cache_stage_coord_files(
    root = root,
    years = 2024,
    include_installed = FALSE
  )

  expect_equal(out, main)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("staging build creates annual files, coordinates, and manifest", {
  root <- tempfile("slc-cache-stage-")
  csv <- tempfile("slc-stage-bts-", fileext = ".csv")
  coords <- tempfile("slc-stage-coords-", fileext = ".csv")

  make_stage_bts_csv(csv)
  make_stage_coords_csv(coords)

  out <- slcflights:::cache_stage_build(
    months = data.frame(year = 2024L, month = 7L),
    root = root,
    coords_in = coords,
    csv_files = list("2024" = csv),
    include_installed = FALSE
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

  manifest <- slcflights:::slc_cache_manifest_path(
    root = root,
    create = FALSE
  )

  expect_true(file.exists(main))
  expect_true(file.exists(coords_out))
  expect_true(file.exists(manifest))
  expect_true(slcflights:::validate_cache_files(root = root))

  read <- arrow::read_parquet(main)

  expect_true("OriginLatitude" %in% names(read))
  expect_true("DestLatitude" %in% names(read))
  expect_equal(nrow(read), 2L)

  expect_equal(out$root, root)
  expect_true(main %in% out$files)
  expect_equal(out$coords, coords_out)
  expect_equal(out$manifest$cached_end, "2024-07")

  unlink(c(root, csv, coords), recursive = TRUE, force = TRUE)
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

  make_stage_bts_csv(csv)
  make_stage_coords_csv(coords)

  slcflights:::cache_stage_build(
    months = data.frame(year = 2024L, month = 7L),
    root = staging,
    coords_in = coords,
    csv_files = list("2024" = csv),
    include_installed = FALSE
  )

  out <- slcflights:::cache_stage_promote(
    staging_root = staging,
    active_root = active
  )

  expect_equal(out, active)
  expect_false(dir.exists(staging))
  expect_true(dir.exists(active))
  expect_true(slcflights:::validate_cache_files(root = active))

  unlink(c(parent, csv, coords), recursive = TRUE, force = TRUE)
})
