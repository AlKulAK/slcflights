test_that("readers reject empty years", {
  expect_error(
    read_main(integer()),
    "must contain at least one year"
  )

  expect_error(
    read_div(numeric()),
    "must contain at least one year"
  )
})

test_that("readers reject non-numeric years", {
  expect_error(
    read_main("1987"),
    "must be a numeric vector of whole years"
  )

  expect_error(
    open_main(c("1987", "1988")),
    "must be a numeric vector of whole years"
  )
})

test_that("readers reject missing and non-finite years", {
  expect_error(
    read_main(c(1987, NA_real_)),
    "must not contain missing values"
  )

  expect_error(
    read_main(c(1987, Inf)),
    "must contain only finite values"
  )
})

test_that("readers reject fractional years", {
  expect_error(
    read_main(1987.5),
    "must contain whole-number years"
  )
})

test_that("readers reject non-calendar years", {
  expect_error(
    read_main(99),
    "must contain four-digit calendar years"
  )
})

test_that("available_years requires an active database", {
  expect_error(
    available_years("main"),
    "No local slcflights database is active"
  )

  expect_error(
    available_years("div"),
    "No local slcflights database is active"
  )
})

test_that("available_years validates type", {
  expect_error(available_years("foo"), "arg")
})

test_that("read_coords requires an active database", {
  expect_error(
    read_coords(),
    "No local slcflights database is active"
  )
})

test_that("read_airlines requires an active database", {
  expect_error(
    read_airlines(),
    "No local slcflights database is active"
  )
})

test_that("packaged field dictionary CSV is readable", {
  x <- read_field_dictionary()

  expect_s3_class(x, "spec_tbl_df", exact = FALSE)
  expect_true("field" %in% names(x))
  expect_true("description" %in% names(x))
  expect_true("main_presence" %in% names(x))
  expect_true("div_presence" %in% names(x))
  expect_true("coords_presence" %in% names(x))
  expect_true(nrow(x) > 0)

  expected_presence <- c("always", "sometimes", "never")
  expect_true(all(x$main_presence %in% expected_presence))
  expect_true(all(x$div_presence %in% expected_presence))
  expect_true(all(x$coords_presence %in% expected_presence))

  required_metadata <- c("field", "group", "source", "description")
  expect_true(all(!is.na(x[required_metadata])))
  expect_true(all(x[required_metadata] != ""))
})

test_that("read_main uses active database paths when present", {
  root <- tempfile("slc-reader-db-")

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

  x <- data.frame(
    FlightDate = as.Date("1987-10-01"),
    OriginAirportID = 14869L,
    DestAirportID = 10000L
  )

  arrow::write_parquet(x, main)

  testthat::local_mocked_bindings(
    slc_available_data_years = function(type = c("main", "div"),
                                        root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "main")

      1987L
    },
    slc_data_paths = function(type = c("main", "div"),
                              years = NULL,
                              root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "main")
      expect_equal(years, 1987L)

      data.frame(
        year = 1987L,
        source = "database",
        path = main
      )
    },
    .package = "slcflights"
  )

  out <- slcflights::read_main(years = 1987)

  expect_equal(nrow(out), 1L)
  expect_equal(out$OriginAirportID, 14869L)
  expect_equal(out$DestAirportID, 10000L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("read_year_main uses active database paths when present", {
  root <- tempfile("slc-reader-db-")

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

  x <- data.frame(
    FlightDate = as.Date("1987-10-01"),
    OriginAirportID = 14869L,
    DestAirportID = 10000L
  )

  arrow::write_parquet(x, main)

  testthat::local_mocked_bindings(
    slc_available_data_years = function(type = c("main", "div"),
                                        root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "main")

      1987L
    },
    slc_data_paths = function(type = c("main", "div"),
                              years = NULL,
                              root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "main")
      expect_equal(years, 1987L)

      data.frame(
        year = 1987L,
        source = "database",
        path = main
      )
    },
    .package = "slcflights"
  )

  out <- slcflights::read_year_main(year = 1987)

  expect_equal(nrow(out), 1L)
  expect_equal(out$OriginAirportID, 14869L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("read_div uses active database paths when present", {
  root <- tempfile("slc-reader-db-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  div <- slcflights:::slc_db_parquet_path(
    "div",
    1987,
    root = root,
    create = TRUE
  )

  x <- data.frame(
    FlightDate = as.Date("1987-10-01"),
    OriginAirportID = 10000L,
    DestAirportID = 20000L,
    Div1AirportID = 14869L
  )

  arrow::write_parquet(x, div)

  testthat::local_mocked_bindings(
    slc_available_data_years = function(type = c("main", "div"),
                                        root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "div")

      1987L
    },
    slc_data_paths = function(type = c("main", "div"),
                              years = NULL,
                              root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "div")
      expect_equal(years, 1987L)

      data.frame(
        year = 1987L,
        source = "database",
        path = div
      )
    },
    .package = "slcflights"
  )

  out <- slcflights::read_div(years = 1987)

  expect_equal(nrow(out), 1L)
  expect_equal(out$Div1AirportID, 14869L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("read_year_div uses active database paths when present", {
  root <- tempfile("slc-reader-db-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  div <- slcflights:::slc_db_parquet_path(
    "div",
    1987,
    root = root,
    create = TRUE
  )

  x <- data.frame(
    FlightDate = as.Date("1987-10-01"),
    OriginAirportID = 10000L,
    DestAirportID = 20000L,
    Div1AirportID = 14869L
  )

  arrow::write_parquet(x, div)

  testthat::local_mocked_bindings(
    slc_available_data_years = function(type = c("main", "div"),
                                        root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "div")

      1987L
    },
    slc_data_paths = function(type = c("main", "div"),
                              years = NULL,
                              root = NULL) {
      type <- match.arg(type)
      expect_equal(type, "div")
      expect_equal(years, 1987L)

      data.frame(
        year = 1987L,
        source = "database",
        path = div
      )
    },
    .package = "slcflights"
  )

  out <- slcflights::read_year_div(year = 1987)

  expect_equal(nrow(out), 1L)
  expect_equal(out$Div1AirportID, 14869L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("read_coords uses active database metadata when present", {
  root <- tempfile("slc-reader-db-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  coords <- slcflights:::slc_db_coords_path(
    root = root,
    create = TRUE
  )

  writeLines(
    c(
      "AIRPORT_SEQ_ID,LATITUDE,LONGITUDE",
      "1,40.1,-111.1"
    ),
    coords
  )

  testthat::local_mocked_bindings(
    slc_coords_path = function(root = NULL) {
      coords
    },
    .package = "slcflights"
  )

  out <- slcflights::read_coords()

  expect_equal(nrow(out), 1L)
  expect_equal(out$AIRPORT_SEQ_ID, 1L)
  expect_equal(out$LATITUDE, 40.1)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("read_airlines uses active database metadata when present", {
  root <- tempfile("slc-reader-db-")

  slcflights:::write_db_manifest(
    months = data.frame(year = 1987L, month = 10L),
    root = root,
    package_version = "0.0.0.9000",
    created_at = "2026-05-01T00:00:00Z"
  )

  airlines <- slcflights:::slc_db_airlines_path(
    root = root,
    create = TRUE
  )

  writeLines(
    c(
      "Code,Description",
      "20001,First Airline Inc.: FA"
    ),
    airlines
  )

  testthat::local_mocked_bindings(
    slc_airlines_path = function(root = NULL) {
      airlines
    },
    .package = "slcflights"
  )

  out <- slcflights::read_airlines()

  expect_equal(nrow(out), 1L)
  expect_equal(out$Code, 20001L)
  expect_equal(out$Description, "First Airline Inc.: FA")

  unlink(root, recursive = TRUE, force = TRUE)
})
