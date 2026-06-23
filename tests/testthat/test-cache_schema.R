test_that("schema type is inferred from cache file name", {
  expect_equal(
    slcflights:::cache_schema_type_for_file(
      file.path("Year=2024", "data_0_main.parquet")
    ),
    "main"
  )

  expect_equal(
    slcflights:::cache_schema_type_for_file(
      file.path("Year=2024", "data_0_div.parquet")
    ),
    "div"
  )

  expect_error(
    slcflights:::cache_schema_type_for_file("x.parquet"),
    "Cannot determine cache data type"
  )
})

test_that("schema year is inferred from cache path", {
  expect_equal(
    slcflights:::cache_schema_year_for_file(
      file.path("extdata", "parquet", "Year=2024", "data_0_main.parquet")
    ),
    2024L
  )

  expect_error(
    slcflights:::cache_schema_year_for_file("data_0_main.parquet"),
    "Cannot determine cache year"
  )
})

test_that("schema alignment requires existing cached file", {
  expect_error(
    slcflights:::cache_schema_align_file(
      tempfile("missing-", fileext = ".parquet")
    ),
    "Cached Parquet file not found"
  )
})

test_that("schema finalization preserves columns", {
  root <- tempfile("slc-cache-schema-")

  cache_main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  x <- data.frame(
    FlightDate = as.Date(c("2024-07-02", "2024-07-01")),
    OriginAirportID = c(14869L, 14869L),
    DestAirportID = c(12000L, 13000L),
    ExtraColumn = c("keep a", "keep b")
  )

  arrow::write_parquet(x, cache_main)

  out <- slcflights:::cache_schema_align_file(cache_main)

  expect_equal(out, cache_main)

  aligned <- arrow::read_parquet(cache_main)

  expect_equal(
    names(aligned),
    c(
      "FlightDate",
      "OriginAirportID",
      "DestAirportID",
      "ExtraColumn",
      "Year"
    )
  )

  expect_equal(
    aligned$FlightDate,
    as.Date(c("2024-07-01", "2024-07-02"))
  )

  expect_equal(aligned$ExtraColumn, c("keep b", "keep a"))

  expect_equal(unique(aligned$Year), 2024L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("schema finalization works for multiple files", {
  root <- tempfile("slc-cache-schema-")

  cache_main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  cache_div <- slcflights:::slc_cache_parquet_path(
    "div",
    2024,
    root = root,
    create = TRUE
  )

  x_main <- data.frame(
    FlightDate = as.Date(c("2024-07-02", "2024-07-01")),
    OriginAirportID = c(14869L, 14869L),
    DestAirportID = c(12000L, 13000L),
    MainExtra = c("keep a", "keep b")
  )

  x_div <- data.frame(
    FlightDate = as.Date(c("2024-07-02", "2024-07-01")),
    Div1AirportID = c(14869L, 14869L),
    DivExtra = c("keep c", "keep d")
  )

  arrow::write_parquet(x_main, cache_main)
  arrow::write_parquet(x_div, cache_div)

  out <- slcflights:::cache_schema_align_files(
    c(cache_main, cache_div)
  )

  expect_equal(out, c(cache_main, cache_div))

  aligned_main <- arrow::read_parquet(cache_main)
  aligned_div <- arrow::read_parquet(cache_div)

  expect_equal(
    names(aligned_main),
    c(names(x_main), "Year")
  )

  expect_equal(
    names(aligned_div),
    c(names(x_div), "Year")
  )

  expect_equal(aligned_main$MainExtra, c("keep b", "keep a"))
  expect_equal(aligned_div$DivExtra, c("keep d", "keep c"))

  expect_equal(unique(aligned_main$Year), 2024L)
  expect_equal(unique(aligned_div$Year), 2024L)

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("schema alignment writes files in route-first flight order", {
  root <- tempfile("slc-cache-schema-")

  cache_main <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  installed <- arrow::read_parquet(
    slcflights:::slc_installed_parquet_path("main", 2024)
  )

  x <- installed[seq_len(min(4L, nrow(installed))), ]

  x <- x[rev(seq_len(nrow(x))), ]
  x$FlightDate <- as.Date(
    c("2024-07-02", "2024-07-01", "2024-07-01", "2024-07-01")
  )
  x$CRSDepTime <- c("0900", NA, "0702", "0700")
  x$OriginAirportID <- c(14869L, 14869L, 14869L, 14869L)
  x$DestAirportID <- c(15000L, 14000L, 13000L, 12000L)
  x$Reporting_Airline <- c("ZZ", "AA", "AA", "AA")
  x$Flight_Number_Reporting_Airline <- c(9L, 2L, 2L, 1L)
  x$OriginAirportSeqID <- c(1486901L, 1486901L, 1486901L, 1486901L)
  x$DestAirportSeqID <- c(1500001L, 1400001L, 1300001L, 1200001L)
  x$DOT_ID_Reporting_Airline <- c(999L, 100L, 100L, 100L)
  x$CacheOnlyColumn <- "keep me"

  arrow::write_parquet(x, cache_main)

  slcflights:::cache_schema_align_file(cache_main)

  aligned <- arrow::read_parquet(cache_main)

  expect_true("CacheOnlyColumn" %in% names(aligned))

  expect_equal(
    aligned$FlightDate,
    as.Date(c("2024-07-01", "2024-07-01", "2024-07-01", "2024-07-02"))
  )

  expect_equal(
    sprintf("%04d", as.integer(aligned$CRSDepTime))[1:2],
    c("0700", "0702")
  )

  expect_true(is.na(aligned$CRSDepTime[[3]]))

  expect_equal(
    aligned$DestAirportID,
    c(12000L, 13000L, 14000L, 15000L)
  )

  unlink(root, recursive = TRUE, force = TRUE)
})
