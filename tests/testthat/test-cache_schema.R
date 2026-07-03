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
      file.path("data", "parquet", "Year=2024", "data_0_main.parquet")
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
  root <- tempfile("slc-schema-")
  path <- slcflights:::slc_cache_parquet_path(
    "main",
    2024,
    root = root,
    create = TRUE
  )

  x <- data.frame(
    OriginAirportID = c(14869L, 12889L, 14869L),
    DestAirportID = c(11292L, 14869L, 11292L),
    FlightDate = as.Date(c("2024-07-03", "2024-07-01", "2024-07-02")),
    CRSDepTime = c(900L, 800L, 700L)
  )

  arrow::write_parquet(x, path)

  slcflights:::cache_schema_align_file(path)

  out <- arrow::read_parquet(path)

  expect_equal(
    out$OriginAirportID,
    c(12889L, 14869L, 14869L)
  )

  expect_equal(
    out$FlightDate,
    as.Date(c("2024-07-01", "2024-07-02", "2024-07-03"))
  )

  unlink(root, recursive = TRUE, force = TRUE)
})
