test_that("BTS on-time ZIP names follow TranStats convention", {
  expect_equal(
    slcflights:::bts_ontime_zip_name(2024, 7),
    paste0(
      "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_",
      "2024_7.zip"
    )
  )

  expect_error(
    slcflights:::bts_ontime_zip_name(2024, 13),
    "integer from 1 to 12"
  )
})

test_that("BTS on-time URLs point to PREZIP files", {
  expect_equal(
    slcflights:::bts_ontime_url(2024, 7),
    paste0(
      "https://transtats.bts.gov/PREZIP/",
      "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_",
      "2024_7.zip"
    )
  )
})

test_that("BTS Master Coordinate URL identifies the expected support table", {
  url <- slcflights:::bts_master_coords_url()

  expect_match(url, "DL_SelectFields[.]aspx", fixed = FALSE)
  expect_match(url, "gnoyr_VQ=FLL", fixed = TRUE)
})

test_that("BTS CSV file discovery is recursive and case-insensitive", {
  root <- tempfile("bts-csv-test-")
  dir.create(file.path(root, "nested"), recursive = TRUE)

  csv_a <- file.path(root, "a.csv")
  csv_b <- file.path(root, "nested", "b.CSV")
  txt <- file.path(root, "nested", "b.txt")

  writeLines("x", csv_a)
  writeLines("x", csv_b)
  writeLines("x", txt)

  out <- basename(slcflights:::bts_csv_files(root))

  expect_equal(sort(out), c("a.csv", "b.CSV"))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("BTS readme file discovery is recursive and case-insensitive", {
  root <- tempfile("bts-readme-test-")
  dir.create(file.path(root, "nested"), recursive = TRUE)

  readme_a <- file.path(root, "readme.html")
  readme_b <- file.path(root, "nested", "README.HTML")
  other <- file.path(root, "nested", "readme.txt")

  writeLines("x", readme_a)
  writeLines("x", readme_b)
  writeLines("x", other)

  out <- basename(slcflights:::bts_readme_files(root))

  expect_equal(sort(out), c("README.HTML", "readme.html"))

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("ZIP raw detection recognizes ZIP signatures", {
  expect_true(
    slcflights:::bts_is_zip_raw(
      as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00))
    )
  )

  expect_false(
    slcflights:::bts_is_zip_raw(charToRaw("not a zip"))
  )
})

test_that("HTML response detection recognizes ordinary HTML", {
  expect_true(
    slcflights:::bts_response_is_html(charToRaw("<!DOCTYPE html><html>"))
  )

  expect_true(
    slcflights:::bts_response_is_html(charToRaw("<html><body>x</body>"))
  )

  expect_false(
    slcflights:::bts_response_is_html(charToRaw("AIRPORT_SEQ_ID,LATITUDE"))
  )
})

test_that("CSV validation rejects missing and empty files", {
  missing <- tempfile("missing-csv-")

  expect_error(
    slcflights:::bts_validate_csv_file(missing),
    "was not found"
  )

  empty <- tempfile("empty-csv-")
  file.create(empty)

  expect_error(
    slcflights:::bts_validate_csv_file(empty),
    "is empty"
  )

  unlink(empty)
})

test_that("CSV validation returns normalized path for non-empty file", {
  path <- tempfile("valid-csv-", fileext = ".csv")
  writeLines("x", path)

  out <- slcflights:::bts_validate_csv_file(path)

  expect_equal(out, normalizePath(path, mustWork = TRUE))

  unlink(path)
})

test_that("coordinate ZIP extraction writes the expected CSV", {
  root <- tempfile("bts-zip-test-")
  dir.create(root, recursive = TRUE)

  src_csv <- file.path(root, "T_MASTER_CORD.csv")
  writeLines("AIRPORT_SEQ_ID,LATITUDE\n1,40.0", src_csv)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  zipfile <- file.path(root, "coords.zip")
  utils::zip(zipfile, files = "T_MASTER_CORD.csv", flags = "-q")

  raw <- readBin(zipfile, what = "raw", n = file.info(zipfile)$size)
  destfile <- file.path(root, "out", "T_MASTER_CORD.csv")

  out <- slcflights:::extract_bts_master_coords_zip(raw, destfile)

  expect_equal(out, normalizePath(destfile, mustWork = TRUE))
  expect_true(file.exists(destfile))
  expect_gt(file.info(destfile)$size, 0)

  unlink(root, recursive = TRUE, force = TRUE)
})
