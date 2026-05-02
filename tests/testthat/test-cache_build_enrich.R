make_enrich_coords_csv <- function(path) {
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
      "3,40.3,-111.3,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1"
    ),
    path
  )

  path
}

test_that("required coordinate columns are explicit", {
  expect_equal(
    slcflights:::cache_build_req_coord_cols(),
    c(
      "AIRPORT_SEQ_ID",
      "LATITUDE",
      "LONGITUDE",
      "AIRPORT_START_DATE",
      "AIRPORT_THRU_DATE",
      "AIRPORT_IS_CLOSED",
      "AIRPORT_IS_LATEST"
    )
  )
})

test_that("coordinate enrichment column validation catches missing columns", {
  expect_error(
    slcflights:::cache_build_val_enrich_cols(
      c("AIRPORT_SEQ_ID", "LATITUDE")
    ),
    "missing required columns"
  )

  expect_true(
    slcflights:::cache_build_val_enrich_cols(
      slcflights:::cache_build_req_coord_cols()
    )
  )
})

test_that("coordinate extra columns are derived from AirportSeqID roles", {
  out <- slcflights:::cache_build_coord_extra_cols(
    c("OriginAirportSeqID", "DestAirportSeqID", "Div1AirportSeqID")
  )

  expect_true("OriginLatitude" %in% out)
  expect_true("OriginLongitude" %in% out)
  expect_true("DestLatitude" %in% out)
  expect_true("DestLongitude" %in% out)
  expect_true("Div1Latitude" %in% out)
  expect_true("Div1Longitude" %in% out)
})

test_that("coordinate join SQL has one join per sequence column", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  out <- slcflights:::cache_build_coord_join_sql(
    con,
    c("OriginAirportSeqID", "DestAirportSeqID")
  )

  expect_match(out, "LEFT JOIN coords c1", fixed = TRUE)
  expect_match(out, "LEFT JOIN coords c2", fixed = TRUE)
  expect_match(out, "OriginAirportSeqID", fixed = TRUE)
  expect_match(out, "DestAirportSeqID", fixed = TRUE)
})

test_that("coordinate select terms insert metadata after sequence columns", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  terms <- slcflights:::cache_build_coord_sel_terms(
    con,
    cols = c(
      "FlightDate",
      "OriginAirportSeqID",
      "DestAirportSeqID",
      "Other"
    ),
    seq_cols = c("OriginAirportSeqID", "DestAirportSeqID")
  )

  origin_pos <- grep("OriginAirportSeqID", terms, fixed = TRUE)[[1]]
  origin_lat_pos <- grep("OriginLatitude", terms, fixed = TRUE)[[1]]
  dest_pos <- grep("DestAirportSeqID", terms, fixed = TRUE)[[1]]
  dest_lat_pos <- grep("DestLatitude", terms, fixed = TRUE)[[1]]

  expect_equal(origin_lat_pos, origin_pos + 1L)
  expect_equal(dest_lat_pos, dest_pos + 1L)
})

test_that("coordinate enrichment requires existing parquet file", {
  coords <- tempfile("coords-", fileext = ".csv")
  make_enrich_coords_csv(coords)

  expect_error(
    slcflights:::cache_build_enrich_file(
      path = tempfile("missing-", fileext = ".parquet"),
      coords_in = coords
    ),
    "Parquet file not found"
  )

  unlink(coords)
})

test_that("coordinate enrichment requires coordinate CSV", {
  root <- tempfile("slc-cache-enrich-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(OriginAirportSeqID = 1L),
    path
  )

  expect_error(
    slcflights:::cache_build_enrich_file(
      path = path,
      coords_in = file.path(root, "missing.csv")
    ),
    "Reduced coordinate CSV not found"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate enrichment requires AirportSeqID columns", {
  root <- tempfile("slc-cache-enrich-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")
  coords <- file.path(root, "coords.csv")

  arrow::write_parquet(
    data.frame(Other = "x"),
    path
  )

  make_enrich_coords_csv(coords)

  expect_error(
    slcflights:::cache_build_enrich_file(
      path = path,
      coords_in = coords
    ),
    "No AirportSeqID columns"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate enrichment adds origin and destination metadata", {
  root <- tempfile("slc-cache-enrich-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")
  coords <- file.path(root, "coords.csv")

  arrow::write_parquet(
    data.frame(
      FlightDate = as.Date("2025-01-01"),
      OriginAirportSeqID = 1L,
      DestAirportSeqID = 2L,
      Other = "x"
    ),
    path
  )

  make_enrich_coords_csv(coords)

  out <- slcflights:::cache_build_enrich_file(
    path = path,
    coords_in = coords
  )

  expect_equal(out, path)

  read <- arrow::read_parquet(path)

  expect_true("OriginLatitude" %in% names(read))
  expect_true("OriginLongitude" %in% names(read))
  expect_true("OriginAirportStartDate" %in% names(read))
  expect_true("OriginAirportThruDate" %in% names(read))
  expect_true("OriginAirportIsClosed" %in% names(read))
  expect_true("OriginAirportIsLatest" %in% names(read))
  expect_true("DestLatitude" %in% names(read))
  expect_true("DestLongitude" %in% names(read))

  expect_equal(read$OriginLatitude, 40.1)
  expect_equal(read$OriginLongitude, -111.1)
  expect_equal(read$DestLatitude, 40.2)
  expect_equal(read$DestLongitude, -111.2)
  expect_equal(read$OriginAirportIsClosed, 0L)
  expect_equal(read$OriginAirportIsLatest, 1L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate enrichment replaces stale coordinate columns", {
  root <- tempfile("slc-cache-enrich-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")
  coords <- file.path(root, "coords.csv")

  arrow::write_parquet(
    data.frame(
      OriginAirportSeqID = 1L,
      OriginLatitude = 999,
      OriginLongitude = 999,
      OriginAirportStartDate = as.Date("1900-01-01"),
      OriginAirportThruDate = as.Date("1900-01-01"),
      OriginAirportIsClosed = 9L,
      OriginAirportIsLatest = 9L
    ),
    path
  )

  make_enrich_coords_csv(coords)

  slcflights:::cache_build_enrich_file(
    path = path,
    coords_in = coords
  )

  read <- arrow::read_parquet(path)

  expect_equal(read$OriginLatitude, 40.1)
  expect_equal(read$OriginLongitude, -111.1)
  expect_equal(read$OriginAirportIsClosed, 0L)
  expect_equal(read$OriginAirportIsLatest, 1L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate enrichment works for multiple files", {
  root <- tempfile("slc-cache-enrich-")
  dir.create(root, recursive = TRUE)

  path1 <- file.path(root, "x1.parquet")
  path2 <- file.path(root, "x2.parquet")
  coords <- file.path(root, "coords.csv")

  arrow::write_parquet(
    data.frame(OriginAirportSeqID = 1L),
    path1
  )

  arrow::write_parquet(
    data.frame(DestAirportSeqID = 2L),
    path2
  )

  make_enrich_coords_csv(coords)

  out <- slcflights:::cache_build_enrich_files(
    parquet_files = c(path1, path2),
    coords_in = coords
  )

  expect_equal(out, c(path1, path2))

  read1 <- arrow::read_parquet(path1)
  read2 <- arrow::read_parquet(path2)

  expect_equal(read1$OriginLatitude, 40.1)
  expect_equal(read2$DestLatitude, 40.2)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("coordinate enrichment requires at least one file", {
  coords <- tempfile("coords-", fileext = ".csv")
  make_enrich_coords_csv(coords)

  expect_error(
    slcflights:::cache_build_enrich_files(
      parquet_files = character(),
      coords_in = coords
    ),
    "at least one Parquet file"
  )

  unlink(coords)
})
