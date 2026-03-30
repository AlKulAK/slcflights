test_that("path helpers are not exported", {
  exports <- getNamespaceExports("slcflights")

  expect_false(any(c(
    "parquet_dir",
    "coords_csv",
    "year_main_parquet",
    "year_div_parquet",
    "main_years",
    "div_years",
    "main_paths",
    "div_paths",
    ".slcflights_file",
    ".coords_path",
    ".parquet_root",
    ".normalize_years",
    ".available_years_internal",
    ".parquet_paths"
  ) %in% exports))
})

test_that("public API remains sufficient without path access", {
  expect_type(available_years("main"), "integer")
  expect_type(available_years("div"), "integer")

  x <- read_coords()
  expect_s3_class(x, "spec_tbl_df", exact = FALSE)
  expect_true("AIRPORT_SEQ_ID" %in% names(x))
  expect_true("LATITUDE" %in% names(x))
  expect_true("LONGITUDE" %in% names(x))
})
