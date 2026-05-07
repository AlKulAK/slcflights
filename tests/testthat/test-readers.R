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
})
