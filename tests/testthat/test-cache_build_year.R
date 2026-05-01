make_cache_build_csv <- function(path, rows) {
  utils::write.csv(
    rows,
    path,
    row.names = FALSE,
    na = ""
  )

  path
}

test_that("CSV relation requires at least one file", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_csv_relation(con, character()),
    "at least one CSV file"
  )
})

test_that("order clause is empty without required sort columns", {
  expect_equal(
    slcflights:::cache_build_order_clause("FlightDate"),
    ""
  )
})

test_that("order clause uses FlightDate and CRSDepTime when available", {
  out <- slcflights:::cache_build_order_clause(
    c("FlightDate", "CRSDepTime")
  )

  expect_match(out, "ORDER BY", fixed = TRUE)
  expect_match(out, "FlightDate", fixed = TRUE)
  expect_match(out, "CRSDepTime", fixed = TRUE)
})

test_that("any-equals clause requires columns", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_any_equals_clause(con, character(), 14869L),
    "at least one column"
  )
})

test_that("any-equals clause joins airport predicates with OR", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  out <- slcflights:::cache_build_any_equals_clause(
    con,
    c("OriginAirportID", "DestAirportID"),
    14869L
  )

  expect_match(out, "OriginAirportID", fixed = TRUE)
  expect_match(out, "DestAirportID", fixed = TRUE)
  expect_match(out, "14869", fixed = TRUE)
  expect_match(out, " OR ", fixed = TRUE)
})

test_that("year file paths preserve annual cache layout", {
  root <- tempfile("slc-cache-year-")

  out <- slcflights:::cache_build_year_file_paths(
    2025,
    root = root,
    create = FALSE
  )

  expect_equal(
    out$main,
    file.path(
      root,
      "extdata",
      "parquet",
      "Year=2025",
      "data_0_main.parquet"
    )
  )

  expect_equal(
    out$div,
    file.path(
      root,
      "extdata",
      "parquet",
      "Year=2025",
      "data_0_div.parquet"
    )
  )
})

test_that("annual cache builder writes main file", {
  root <- tempfile("slc-cache-year-")
  csv <- tempfile("slc-cache-year-", fileext = ".csv")

  rows <- data.frame(
    FlightDate = c("2025-01-02", "2025-01-01", "2025-01-03"),
    CRSDepTime = c(900L, 800L, 700L),
    OriginAirportID = c(14869L, 11111L, 22222L),
    DestAirportID = c(33333L, 14869L, 44444L),
    OriginAirportSeqID = c(1L, 2L, 3L),
    DestAirportSeqID = c(4L, 5L, 6L)
  )

  make_cache_build_csv(csv, rows)

  out <- slcflights:::cache_build_write_year_files(
    year = 2025,
    months = 1,
    root = root,
    csv_files = csv,
    slc_id = 14869L
  )

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2025,
    root = root,
    create = FALSE
  )

  expect_true(main %in% out)
  expect_true(file.exists(main))

  read <- arrow::read_parquet(main)

  expect_equal(nrow(read), 2L)
  expect_equal(
    read$FlightDate,
    as.Date(c("2025-01-01", "2025-01-02"))
  )

  unlink(c(root, csv), recursive = TRUE, force = TRUE)
})

test_that("annual cache builder omits div file when there are no div rows", {
  root <- tempfile("slc-cache-year-")
  csv <- tempfile("slc-cache-year-", fileext = ".csv")

  rows <- data.frame(
    FlightDate = c("2025-01-01", "2025-01-02"),
    CRSDepTime = c(800L, 900L),
    OriginAirportID = c(14869L, 11111L),
    DestAirportID = c(33333L, 14869L),
    Div1AirportID = c(NA_integer_, NA_integer_),
    OriginAirportSeqID = c(1L, 2L),
    DestAirportSeqID = c(3L, 4L),
    Div1AirportSeqID = c(NA_integer_, NA_integer_)
  )

  make_cache_build_csv(csv, rows)

  out <- slcflights:::cache_build_write_year_files(
    year = 2025,
    months = 1,
    root = root,
    csv_files = csv,
    slc_id = 14869L
  )

  div <- slcflights:::slc_cache_parquet_path(
    "div",
    2025,
    root = root,
    create = FALSE
  )

  expect_false(div %in% out)
  expect_false(file.exists(div))

  unlink(c(root, csv), recursive = TRUE, force = TRUE)
})

test_that("annual cache builder writes diversion-only file when needed", {
  root <- tempfile("slc-cache-year-")
  csv <- tempfile("slc-cache-year-", fileext = ".csv")

  rows <- data.frame(
    FlightDate = c("2025-01-02", "2025-01-01", "2025-01-03"),
    CRSDepTime = c(900L, 800L, 700L),
    OriginAirportID = c(11111L, 14869L, 22222L),
    DestAirportID = c(33333L, 44444L, 55555L),
    Div1AirportID = c(14869L, NA_integer_, 14869L),
    OriginAirportSeqID = c(1L, 2L, 3L),
    DestAirportSeqID = c(4L, 5L, 6L),
    Div1AirportSeqID = c(7L, NA_integer_, 8L)
  )

  make_cache_build_csv(csv, rows)

  out <- slcflights:::cache_build_write_year_files(
    year = 2025,
    months = 1,
    root = root,
    csv_files = csv,
    slc_id = 14869L
  )

  main <- slcflights:::slc_cache_parquet_path(
    "main",
    2025,
    root = root,
    create = FALSE
  )

  div <- slcflights:::slc_cache_parquet_path(
    "div",
    2025,
    root = root,
    create = FALSE
  )

  expect_true(main %in% out)
  expect_true(div %in% out)
  expect_true(file.exists(main))
  expect_true(file.exists(div))

  main_data <- arrow::read_parquet(main)
  div_data <- arrow::read_parquet(div)

  expect_equal(nrow(main_data), 1L)
  expect_equal(nrow(div_data), 2L)
  expect_equal(
    div_data$FlightDate,
    as.Date(c("2025-01-02", "2025-01-03"))
  )

  unlink(c(root, csv), recursive = TRUE, force = TRUE)
})

test_that("annual cache builder errors without Origin/Dest columns", {
  root <- tempfile("slc-cache-year-")
  csv <- tempfile("slc-cache-year-", fileext = ".csv")

  rows <- data.frame(
    FlightDate = "2025-01-01",
    CRSDepTime = 800L,
    Div1AirportID = 14869L
  )

  make_cache_build_csv(csv, rows)

  expect_error(
    slcflights:::cache_build_write_year_files(
      year = 2025,
      months = 1,
      root = root,
      csv_files = csv,
      slc_id = 14869L
    ),
    "Origin/Dest airport ID columns"
  )

  unlink(c(root, csv), recursive = TRUE, force = TRUE)
})
