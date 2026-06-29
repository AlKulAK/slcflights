test_that("cache build connection opens and closes", {
  con <- slcflights:::cache_build_connect()
  expect_true(DBI::dbIsValid(con))

  slcflights:::cache_build_disconnect(con)
  expect_false(DBI::dbIsValid(con))
})

test_that("cache build path quoting returns SQL string", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  path <- tempfile("cache-build-db-")
  writeLines("x", path)

  quoted <- slcflights:::cache_build_quote_path(con, path)

  expect_s4_class(quoted, "SQL")
  expect_match(as.character(quoted), normalizePath(path, winslash = "/"))

  unlink(path)
})

test_that("cache build quote paths requires at least one path", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_quote_paths(con, character()),
    "at least one path"
  )
})

test_that("cache build quote paths collapses multiple paths", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  path1 <- tempfile("cache-build-db-")
  path2 <- tempfile("cache-build-db-")
  writeLines("x", path1)
  writeLines("x", path2)

  out <- slcflights:::cache_build_quote_paths(con, c(path1, path2))

  expect_match(out, normalizePath(path1, winslash = "/"), fixed = TRUE)
  expect_match(out, normalizePath(path2, winslash = "/"), fixed = TRUE)
  expect_match(out, ",", fixed = TRUE)

  unlink(c(path1, path2))
})

test_that("cache month normalization validates months", {
  expect_equal(
    slcflights:::normalize_cache_months(c(3, 1, 1)),
    c(1L, 3L)
  )

  expect_error(
    slcflights:::normalize_cache_months(integer()),
    "at least one month"
  )

  expect_error(
    slcflights:::normalize_cache_months(NA),
    "missing values"
  )

  expect_error(
    slcflights:::normalize_cache_months("1"),
    "numeric"
  )

  expect_error(
    slcflights:::normalize_cache_months(1.5),
    "whole-number"
  )

  expect_error(
    slcflights:::normalize_cache_months(13),
    "values from 1 to 12"
  )
})

test_that("raw on-time CSV discovery returns empty when raw root is absent", {
  expect_type(
    slcflights:::cache_raw_ontime_csvs(2024, months = 7),
    "character"
  )
})

test_that("airport sequence columns are discovered from column names", {
  cols <- c(
    "OriginAirportSeqID",
    "DestAirportSeqID",
    "Div1AirportSeqID",
    "Div2AirportSeqID",
    "Other"
  )

  expect_equal(
    slcflights:::cache_build_airport_seq_cols(cols),
    c(
      "OriginAirportSeqID",
      "DestAirportSeqID",
      "Div1AirportSeqID",
      "Div2AirportSeqID"
    )
  )
})

test_that("airport ID columns are discovered from column names", {
  cols <- c(
    "OriginAirportID",
    "DestAirportID",
    "Div1AirportID",
    "Div2AirportID",
    "Other"
  )

  expect_equal(
    slcflights:::cache_build_airport_id_cols(cols),
    c(
      "OriginAirportID",
      "DestAirportID",
      "Div1AirportID",
      "Div2AirportID"
    )
  )
})

test_that("main and diversion airport ID column helpers are specific", {
  cols <- c(
    "OriginAirportID",
    "DestAirportID",
    "Div1AirportID",
    "Div2AirportID",
    "Other"
  )

  expect_equal(
    slcflights:::cache_build_main_ap_id_cols(cols),
    c("OriginAirportID", "DestAirportID")
  )

  expect_equal(
    slcflights:::cache_build_div_ap_id_cols(cols),
    c("Div1AirportID", "Div2AirportID")
  )
})

test_that("sort column helper detects required sort columns", {
  expect_true(
    slcflights:::cache_build_has_req_sort_cols(
      c("FlightDate", "CRSDepTime")
    )
  )

  expect_false(
    slcflights:::cache_build_has_req_sort_cols(
      c("FlightDate")
    )
  )
})

test_that("SQL identifier list requires columns", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_sql_ider_list(con, character()),
    "at least one column"
  )
})

test_that("SQL identifier list quotes column names", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  out <- slcflights:::cache_build_sql_ider_list(
    con,
    c("FlightDate", "CRSDepTime")
  )

  expect_match(out, "FlightDate", fixed = TRUE)
  expect_match(out, "CRSDepTime", fixed = TRUE)
  expect_match(out, ",", fixed = TRUE)
})

test_that("invalid UTF-8 bytes are replaced in raw BTS CSVs", {
  csv <- tempfile("slcflights-invalid-utf8-", fileext = ".csv")

  bytes <- as.raw(c(
    charToRaw("Tail_Number,OriginAirportID,DestAirportID\n"),
    charToRaw("N3CJA1,14869,12892\n"),
    as.raw(0xe4),
    charToRaw("NKNO"),
    as.raw(0xe6),
    charToRaw(",12892,14869\n")
  ))

  writeBin(bytes, csv)
  on.exit(unlink(csv, force = TRUE), add = TRUE)

  clean_csv <- slcflights:::cache_build_sanitize_utf8_csv(csv)
  on.exit(unlink(clean_csv, force = TRUE), add = TRUE)

  clean <- readLines(clean_csv, warn = FALSE)

  expect_equal(clean[[1]], "Tail_Number,OriginAirportID,DestAirportID")
  expect_equal(clean[[2]], "N3CJA1,14869,12892")
  expect_equal(clean[[3]], "@NKNO@,12892,14869")
})
