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

test_that("available_years returns sorted integer vectors", {
  yrs_main <- available_years("main")
  yrs_div <- available_years("div")

  expect_type(yrs_main, "integer")
  expect_type(yrs_div, "integer")

  expect_true(all(diff(yrs_main) >= 0))
  expect_true(all(diff(yrs_div) >= 0))
})

test_that("available_years validates type", {
  expect_error(available_years("foo"), "arg")
})

test_that("all available main years have readable parquet files", {
  yrs <- available_years("main")

  expect_true(length(yrs) > 0)

  for (yr in yrs) {
    x <- read_year_main(yr)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 0)
  }
})

test_that("all available diversion years have readable parquet files", {
  yrs <- available_years("div")

  for (yr in yrs) {
    x <- read_year_div(yr)
    expect_s3_class(x, "data.frame")
    expect_true(nrow(x) >= 0)
  }
})

test_that("single-year and multi-year readers return data frames", {
  main_yrs <- available_years("main")
  div_yrs <- available_years("div")

  x_main_1 <- read_main(main_yrs[[1]])
  expect_s3_class(x_main_1, "data.frame")

  if (length(main_yrs) >= 2) {
    x_main_2 <- read_main(main_yrs[1:2])
    expect_s3_class(x_main_2, "data.frame")
    expect_true(nrow(x_main_2) >= nrow(x_main_1))
  }

  if (length(div_yrs) >= 1) {
    x_div_1 <- read_div(div_yrs[[1]])
    expect_s3_class(x_div_1, "data.frame")
  }

  if (length(div_yrs) >= 2) {
    x_div_2 <- read_div(div_yrs[1:2])
    expect_s3_class(x_div_2, "data.frame")
  }
})

test_that("open_main and open_div return Arrow datasets", {
  main_yrs <- available_years("main")
  div_yrs <- available_years("div")

  ds_main <- open_main(main_yrs[[1]])
  expect_true(inherits(ds_main, "Dataset"))

  if (length(div_yrs) >= 1) {
    ds_div <- open_div(div_yrs[[1]])
    expect_true(inherits(ds_div, "Dataset"))
  }
})

test_that("requesting a missing main year errors cleanly", {
  main_yrs <- available_years("main")
  missing_year <- setdiff(
    seq.int(min(main_yrs), max(main_yrs) + 1L),
    main_yrs
  )

  if (length(missing_year) == 0) {
    skip("No missing main year is available to test")
  }

  expect_error(
    read_year_main(missing_year[[1]]),
    "No main parquet file found for year"
  )
})

test_that("requesting a missing diversion year errors cleanly", {
  main_yrs <- available_years("main")
  div_yrs <- available_years("div")
  missing_year <- setdiff(main_yrs, div_yrs)

  if (length(missing_year) == 0) {
    skip("No year without a diversion parquet file is available to test")
  }

  expect_error(
    read_year_div(missing_year[[1]]),
    "No diversion parquet file found for year"
  )
})

test_that("packaged coordinates CSV is readable and has expected columns", {
  x <- read_coords()

  expect_s3_class(x, "spec_tbl_df", exact = FALSE)
  expect_true("AIRPORT_SEQ_ID" %in% names(x))
  expect_true("LATITUDE" %in% names(x))
  expect_true("LONGITUDE" %in% names(x))
})

test_that("packaged Airline ID lookup CSV is readable", {
  x <- read_airlines()

  expect_s3_class(x, "spec_tbl_df", exact = FALSE)
  expect_true("DOT_ID_Reporting_Airline" %in% names(x))
  expect_true("Reporting_Airline_Name" %in% names(x))
  expect_true("Reporting_Airline_Lookup_Code" %in% names(x))
  expect_true(nrow(x) > 0)
  expect_true(any(!is.na(x$Reporting_Airline_Name)))

  expect_equal(
    x$Reporting_Airline_Lookup_Code[
      x$DOT_ID_Reporting_Airline == 20374
    ],
    "XE (1)"
  )

  expect_equal(
    x$Reporting_Airline_Lookup_Code[
      x$DOT_ID_Reporting_Airline == 19991
    ],
    "HP"
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
