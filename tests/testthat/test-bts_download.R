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

test_that("BTS Airline ID URL identifies the expected lookup table", {
  url <- slcflights:::bts_airline_id_url()

  expect_match(url, "Download_Lookup[.]asp", fixed = FALSE)
  expect_match(url, "Y11x72=Y_NVeYVaR_VQ", fixed = TRUE)
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

test_that("BTS download timeout is at least five minutes", {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  options(timeout = 60)

  expect_equal(slcflights:::bts_download_timeout(), 300)
})

test_that("BTS download timeout respects larger user timeouts", {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  options(timeout = 900)

  expect_equal(slcflights:::bts_download_timeout(), 900)
})

test_that("BTS download timeout ignores invalid timeout values", {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  options(timeout = NA_real_)
  expect_equal(slcflights:::bts_download_timeout(), 300)

  options(timeout = -1)
  expect_equal(slcflights:::bts_download_timeout(), 300)
})

test_that("ZIP validation rejects missing and empty files", {
  missing <- tempfile("missing-zip-")

  expect_error(
    slcflights:::bts_validate_zip_file(missing),
    "was not downloaded"
  )

  empty <- tempfile("empty-zip-")
  file.create(empty)

  expect_error(
    slcflights:::bts_validate_zip_file(empty),
    "is empty"
  )

  unlink(empty)
})

test_that("ZIP validation rejects HTML and non-ZIP files", {
  html <- tempfile("html-zip-")
  writeLines("<!DOCTYPE html><html></html>", html)

  expect_error(
    slcflights:::bts_validate_zip_file(html),
    "returned HTML"
  )

  text <- tempfile("text-zip-")
  writeLines("not a zip", text)

  expect_error(
    slcflights:::bts_validate_zip_file(text),
    "is not a ZIP file"
  )

  unlink(c(html, text))
})

test_that("ZIP validation rejects incomplete ZIP files", {
  path <- tempfile("partial-zip-")
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04)), path)

  expect_error(
    slcflights:::bts_validate_zip_file(path),
    "is not a readable ZIP archive"
  )

  unlink(path)
})

test_that("ZIP validation requires a CSV file", {
  root <- tempfile("bts-zip-no-csv-")
  dir.create(root, recursive = TRUE)

  txt <- file.path(root, "notes.txt")
  writeLines("x", txt)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  zipfile <- file.path(root, "notes.zip")
  utils::zip(zipfile, files = "notes.txt", flags = "-q")

  expect_error(
    slcflights:::bts_validate_zip_file(zipfile),
    "does not contain a CSV file"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("ZIP validation returns normalized path for ZIP with CSV", {
  root <- tempfile("bts-zip-valid-")
  dir.create(root, recursive = TRUE)

  src_csv <- file.path(root, "data.csv")
  writeLines("x", src_csv)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  zipfile <- file.path(root, "data.zip")
  utils::zip(zipfile, files = "data.csv", flags = "-q")

  out <- slcflights:::bts_validate_zip_file(zipfile)

  expect_equal(out, normalizePath(zipfile, mustWork = TRUE))

  unlink(root, recursive = TRUE, force = TRUE)
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

test_that("BTS Airline ID download reuses existing CSV by default", {
  path <- tempfile("airline-id-", fileext = ".csv")
  writeLines("Code,Description\n1,Example Airline: EX", path)

  out <- slcflights:::download_bts_airline_id(
    destfile = path,
    overwrite = FALSE
  )

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

test_that("selected field names normalize to PREZIP names", {
  cols <- c(
    "ORIGIN_AIRPORT_ID",
    "ORIGIN_AIRPORT_SEQ_ID",
    "DEST_AIRPORT_ID",
    "DEST_AIRPORT_SEQ_ID",
    "FL_DATE",
    "UNKNOWN_FIELD"
  )

  expect_equal(
    slcflights:::bts_normalize_selected_names(cols),
    c(
      "OriginAirportID",
      "OriginAirportSeqID",
      "DestAirportID",
      "DestAirportSeqID",
      "FlightDate",
      "UNKNOWN_FIELD"
    )
  )
})

test_that("selected-fields CSV header is rewritten", {
  path <- tempfile(fileext = ".csv")

  writeLines(
    c(
      paste(
        "ORIGIN_AIRPORT_ID",
        "ORIGIN_AIRPORT_SEQ_ID",
        "DEST_AIRPORT_ID",
        "DEST_AIRPORT_SEQ_ID",
        sep = ","
      ),
      "14869,1486901,11292,1129202"
    ),
    path,
    useBytes = TRUE
  )

  out <- slcflights:::bts_rewrite_csv_header(path)

  expect_equal(normalizePath(path), out)
  expect_equal(
    readLines(path, n = 1),
    paste(
      "OriginAirportID",
      "OriginAirportSeqID",
      "DestAirportID",
      "DestAirportSeqID",
      sep = ","
    )
  )
})
