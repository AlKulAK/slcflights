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

test_that("order clause is empty without FlightDate", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_equal(
    slcflights:::cache_build_order_clause("CRSDepTime", con),
    ""
  )
})

test_that("order clause uses route-first flight ordering", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  out <- slcflights:::cache_build_order_clause(
    c(
      "FlightDate",
      "CRSDepTime",
      "OriginAirportID",
      "DestAirportID",
      "Reporting_Airline",
      "Flight_Number_Reporting_Airline",
      "OriginAirportSeqID",
      "DestAirportSeqID",
      "DOT_ID_Reporting_Airline"
    ),
    con
  )

  expect_match(out, "ORDER BY", fixed = TRUE)
  expect_match(out, "FlightDate", fixed = TRUE)
  expect_match(out, "CRSDepTime IS NULL", fixed = TRUE)
  expect_match(out, "CRSDepTime", fixed = TRUE)
  expect_match(out, "OriginAirportID", fixed = TRUE)
  expect_match(out, "DestAirportID", fixed = TRUE)
  expect_match(out, "Reporting_Airline", fixed = TRUE)
  expect_match(out, "Flight_Number_Reporting_Airline", fixed = TRUE)
  expect_match(out, "OriginAirportSeqID", fixed = TRUE)
  expect_match(out, "DestAirportSeqID", fixed = TRUE)
  expect_match(out, "DOT_ID_Reporting_Airline", fixed = TRUE)

  expect_lt(
    regexpr("OriginAirportID", out, fixed = TRUE)[[1]],
    regexpr("Reporting_Airline", out, fixed = TRUE)[[1]]
  )
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
      "data",
      "parquet",
      "Year=2025",
      "data_0_main.parquet"
    )
  )

  expect_equal(
    out$div,
    file.path(
      root,
      "data",
      "parquet",
      "Year=2025",
      "data_0_div.parquet"
    )
  )
})

test_that("year CSV discovery fails when a requested month is missing", {
  root <- withr::local_tempdir()
  withr::local_envvar(SLCFLIGHTS_TEST_CACHE_ROOT = root)

  for (month in 1:7) {
    slcflights:::slc_cache_raw_ontime_month_dir(
      year = 2026,
      month = month,
      create = TRUE
    )
  }

  for (month in 3:7) {
    month_dir <- slcflights:::slc_cache_raw_ontime_month_dir(
      year = 2026,
      month = month,
      create = FALSE
    )

    csv <- file.path(
      month_dir,
      sprintf("2026_%02d.csv", month)
    )

    writeLines(
      c(
        "Year,Month",
        sprintf("2026,%d", month)
      ),
      csv
    )
  }

  expect_error(
    slcflights:::cache_build_year_csvs(
      year = 2026,
      months = 1:7
    ),
    "Raw BTS on-time CSV data are missing for 2026-01.",
    fixed = TRUE
  )
})

test_that("annual cache builder writes main file", {
  root <- tempfile("slc-cache-year-")
  csv <- tempfile("slc-cache-year-", fileext = ".csv")

  rows <- data.frame(
    FlightDate = c(
      "2025-01-02",
      "2025-01-01",
      "2025-01-01",
      "2025-01-01",
      "2025-01-03"
    ),
    CRSDepTime = c(900L, 700L, 700L, 702L, 700L),
    OriginAirportID = c(14869L, 14869L, 14869L, 11111L, 22222L),
    DestAirportID = c(33333L, 14000L, 13000L, 14869L, 44444L),
    Reporting_Airline = c("ZZ", "AA", "AA", "AA", "AA"),
    Flight_Number_Reporting_Airline = c(9L, 2L, 1L, 3L, 4L),
    OriginAirportSeqID = c(1L, 1L, 1L, 2L, 3L),
    DestAirportSeqID = c(4L, 14L, 13L, 5L, 6L),
    DOT_ID_Reporting_Airline = c(999L, 100L, 100L, 100L, 100L)
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

  expect_equal(nrow(read), 4L)

  expect_equal(
    read$FlightDate,
    as.Date(c("2025-01-01", "2025-01-01", "2025-01-01", "2025-01-02"))
  )

  expect_equal(
    as.integer(read$CRSDepTime),
    c(700L, 700L, 702L, 900L)
  )

  expect_equal(
    read$DestAirportID,
    c(13000L, 14000L, 14869L, 33333L)
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
    FlightDate = c(
      "2025-01-02",
      "2025-01-01",
      "2025-01-03",
      "2025-01-02"
    ),
    CRSDepTime = c(900L, 800L, 700L, 900L),
    OriginAirportID = c(11111L, 14869L, 22222L, 11111L),
    DestAirportID = c(33333L, 44444L, 55555L, 22222L),
    Reporting_Airline = c("AA", "AA", "AA", "AA"),
    Flight_Number_Reporting_Airline = c(2L, 1L, 3L, 4L),
    Div1AirportID = c(14869L, NA_integer_, 14869L, 14869L),
    OriginAirportSeqID = c(1L, 2L, 3L, 1L),
    DestAirportSeqID = c(4L, 5L, 6L, 2L),
    Div1AirportSeqID = c(7L, NA_integer_, 8L, 7L),
    DOT_ID_Reporting_Airline = c(100L, 100L, 100L, 100L)
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
  expect_equal(nrow(div_data), 3L)

  expect_equal(
    div_data$FlightDate,
    as.Date(c("2025-01-02", "2025-01-02", "2025-01-03"))
  )

  expect_equal(
    as.integer(div_data$CRSDepTime),
    c(900L, 900L, 700L)
  )

  expect_equal(
    div_data$DestAirportID,
    c(22222L, 33333L, 55555L)
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

test_that("cache_build_bts_cols drops malformed columns", {
  cols <- c(
    "FlightDate",
    "Origin",
    "Div5TailNum",
    "column109",
    ""
  )

  expect_equal(
    cache_build_bts_cols(cols),
    c("FlightDate", "Origin", "Div5TailNum")
  )
})

test_that("annual builder sanitizes invalid UTF-8 BTS CSVs", {
  root <- withr::local_tempdir()
  csv <- tempfile("slcflights-invalid-utf8-", fileext = ".csv")

  bytes <- as.raw(c(
    charToRaw(paste0(
      "Year,Quarter,Month,DayofMonth,DayOfWeek,FlightDate,",
      "Reporting_Airline,DOT_ID_Reporting_Airline,",
      "IATA_CODE_Reporting_Airline,Tail_Number,",
      "Flight_Number_Reporting_Airline,OriginAirportID,",
      "OriginAirportSeqID,OriginCityMarketID,Origin,OriginCityName,",
      "OriginState,OriginStateFips,OriginStateName,OriginWac,",
      "DestAirportID,DestAirportSeqID,DestCityMarketID,Dest,",
      "DestCityName,DestState,DestStateFips,DestStateName,DestWac,",
      "CRSDepTime\n"
    )),
    charToRaw(paste0(
      "2001,2,5,1,2,2001-05-01,AA,19805,AA,N3CJA1,1,",
      "14869,1486901,34614,SLC,\"Salt Lake City, UT\",UT,49,",
      "Utah,87,12892,1289201,32575,LAX,\"Los Angeles, CA\",",
      "CA,06,California,91,0800\n"
    )),
    charToRaw("2001,2,5,2,3,2001-05-02,AA,19805,AA,"),
    as.raw(0xe4),
    charToRaw("NKNO"),
    as.raw(0xe6),
    charToRaw(paste0(
      ",2,12892,1289201,32575,LAX,\"Los Angeles, CA\",CA,06,",
      "California,91,14869,1486901,34614,SLC,",
      "\"Salt Lake City, UT\",UT,49,Utah,87,0900\n"
    ))
  ))

  writeBin(bytes, csv)
  withr::defer(unlink(csv, force = TRUE))

  con <- slcflights:::cache_build_connect()
  withr::defer(slcflights:::cache_build_disconnect(con))

  out <- slcflights:::cache_build_write_year_files(
    year = 2001,
    months = 5,
    root = root,
    csv_files = csv,
    con = con
  )

  expect_true(file.exists(out[[1]]))

  got <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT Tail_Number FROM read_parquet(%s) ORDER BY FlightDate",
      DBI::dbQuoteString(con, normalizePath(out[[1]], winslash = "/"))
    )
  )

  expect_equal(got$Tail_Number, c("N3CJA1", "@NKNO@"))
})
