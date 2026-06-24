# Internal BTS download helpers ----------------------------------------------
#
# These helpers prepare raw BTS source files for cache builds. They are
# intentionally unexported. Users should call build_slcflights_db() or
# update_slcflights_db().

bts_ontime_zip_name <- function(year, month) {
  ym <- as_year_month(year, month, arg = "year/month")

  sprintf(
    "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_%d_%d.zip",
    ym$year,
    ym$month
  )
}

#' Construct a BTS On-Time Performance ZIP URL
#'
#' Constructs the direct BTS PREZIP URL for one Reporting Carrier On-Time
#' Performance monthly ZIP file.
#'
#' @param year Integer-like calendar year.
#' @param month Integer-like calendar month.
#'
#' @returns
#' Character URL for the requested monthly BTS ZIP file.
#'
#' @noRd
bts_ontime_url <- function(year, month) {
  sprintf(
    "https://transtats.bts.gov/PREZIP/%s",
    bts_ontime_zip_name(year, month)
  )
}

#' Return the BTS On-Time Performance selected-fields page URL
#'
#' Returns the TranStats select-fields page used when a static PREZIP monthly
#' ZIP file is not available.
#'
#' @returns
#' Character URL for the BTS On-Time Performance selected-fields form.
#'
#' @noRd
bts_ontime_select_url <- function() {
  paste0(
    "https://www.transtats.bts.gov/DL_SelectFields.aspx?",
    "QO_fu146_anzr=b0-gvzr&gnoyr_VQ=FGJ"
  )
}

#' Return the BTS Master Coordinate download page URL
#'
#' Returns the TranStats select-fields page used to request the Master
#' Coordinate support table.
#'
#' @returns
#' Character URL for the BTS Master Coordinate download form.
#'
#' @noRd
bts_master_coords_url <- function() {
  paste0(
    "https://www.transtats.bts.gov/DL_SelectFields.aspx?",
    "QO_fu146_anzr=N8vn6v10+f722146+gnoyr5&gnoyr_VQ=FLL"
  )
}

#' Return the BTS Airline ID lookup URL
#'
#' Returns the TranStats lookup-table URL for the DOT reporting airline ID
#' support table used by the Reporting Carrier On-Time Performance data.
#'
#' @returns
#' Character URL for the BTS Airline ID lookup CSV.
#'
#' @noRd
bts_airline_id_url <- function() {
  paste0(
    "https://www.transtats.bts.gov/",
    "Download_Lookup.asp?Y11x72=Y_NVeYVaR_VQ"
  )
}

bts_csv_files <- function(path) {
  list.files(
    path,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )
}

bts_selected_name_map <- function() {
  c(
    YEAR = "Year",
    QUARTER = "Quarter",
    MONTH = "Month",
    DAY_OF_MONTH = "DayofMonth",
    DAY_OF_WEEK = "DayOfWeek",
    FL_DATE = "FlightDate",
    FLIGHT_DATE = "FlightDate",
    OP_UNIQUE_CARRIER = "Reporting_Airline",
    OP_CARRIER_AIRLINE_ID = "DOT_ID_Reporting_Airline",
    OP_CARRIER = "IATA_CODE_Reporting_Airline",
    TAIL_NUM = "Tail_Number",
    OP_CARRIER_FL_NUM = "Flight_Number_Reporting_Airline",
    REPORTING_AIRLINE = "Reporting_Airline",
    DOT_ID_REPORTING_AIRLINE = "DOT_ID_Reporting_Airline",
    IATA_CODE_REPORTING_AIRLINE = "IATA_CODE_Reporting_Airline",
    TAIL_NUMBER = "Tail_Number",
    FLIGHT_NUMBER_REPORTING_AIRLINE = "Flight_Number_Reporting_Airline",
    ORIGIN_AIRPORT_ID = "OriginAirportID",
    ORIGIN_AIRPORT_SEQ_ID = "OriginAirportSeqID",
    ORIGIN_CITY_MARKET_ID = "OriginCityMarketID",
    ORIGIN = "Origin",
    ORIGIN_CITY_NAME = "OriginCityName",
    ORIGIN_STATE_ABR = "OriginState",
    ORIGIN_STATE_FIPS = "OriginStateFips",
    ORIGIN_STATE_NM = "OriginStateName",
    ORIGIN_WAC = "OriginWac",
    DEST_AIRPORT_ID = "DestAirportID",
    DEST_AIRPORT_SEQ_ID = "DestAirportSeqID",
    DEST_CITY_MARKET_ID = "DestCityMarketID",
    DEST = "Dest",
    DEST_CITY_NAME = "DestCityName",
    DEST_STATE_ABR = "DestState",
    DEST_STATE_FIPS = "DestStateFips",
    DEST_STATE_NM = "DestStateName",
    DEST_WAC = "DestWac",
    CRS_DEP_TIME = "CRSDepTime",
    DEP_TIME = "DepTime",
    DEP_DELAY = "DepDelay",
    DEP_DELAY_NEW = "DepDelayMinutes",
    DEP_DEL15 = "DepDel15",
    DEP_DELAY_GROUP = "DepDelayGroups",
    DEP_TIME_BLK = "DepTimeBlk",
    TAXI_OUT = "TaxiOut",
    WHEELS_OFF = "WheelsOff",
    WHEELS_ON = "WheelsOn",
    TAXI_IN = "TaxiIn",
    CRS_ARR_TIME = "CRSArrTime",
    ARR_TIME = "ArrTime",
    ARR_DELAY = "ArrDelay",
    ARR_DELAY_NEW = "ArrDelayMinutes",
    ARR_DEL15 = "ArrDel15",
    ARR_DELAY_GROUP = "ArrDelayGroups",
    ARR_TIME_BLK = "ArrTimeBlk",
    CANCELLED = "Cancelled",
    CANCELLATION_CODE = "CancellationCode",
    DIVERTED = "Diverted",
    CRS_ELAPSED_TIME = "CRSElapsedTime",
    ACTUAL_ELAPSED_TIME = "ActualElapsedTime",
    AIR_TIME = "AirTime",
    FLIGHTS = "Flights",
    DISTANCE = "Distance",
    DISTANCE_GROUP = "DistanceGroup",
    CARRIER_DELAY = "CarrierDelay",
    WEATHER_DELAY = "WeatherDelay",
    NAS_DELAY = "NASDelay",
    SECURITY_DELAY = "SecurityDelay",
    LATE_AIRCRAFT_DELAY = "LateAircraftDelay",
    FIRST_DEP_TIME = "FirstDepTime",
    TOTAL_ADD_GTIME = "TotalAddGTime",
    LONGEST_ADD_GTIME = "LongestAddGTime",
    DIV_AIRPORT_LANDINGS = "DivAirportLandings",
    DIV_REACHED_DEST = "DivReachedDest",
    DIV_ACTUAL_ELAPSED_TIME = "DivActualElapsedTime",
    DIV_ARR_DELAY = "DivArrDelay",
    DIV_DISTANCE = "DivDistance",
    DIV1_AIRPORT = "Div1Airport",
    DIV1_AIRPORT_ID = "Div1AirportID",
    DIV1_AIRPORT_SEQ_ID = "Div1AirportSeqID",
    DIV1_WHEELS_ON = "Div1WheelsOn",
    DIV1_TOTAL_GTIME = "Div1TotalGTime",
    DIV1_LONGEST_GTIME = "Div1LongestGTime",
    DIV1_WHEELS_OFF = "Div1WheelsOff",
    DIV1_TAIL_NUM = "Div1TailNum",
    DIV2_AIRPORT = "Div2Airport",
    DIV2_AIRPORT_ID = "Div2AirportID",
    DIV2_AIRPORT_SEQ_ID = "Div2AirportSeqID",
    DIV2_WHEELS_ON = "Div2WheelsOn",
    DIV2_TOTAL_GTIME = "Div2TotalGTime",
    DIV2_LONGEST_GTIME = "Div2LongestGTime",
    DIV2_WHEELS_OFF = "Div2WheelsOff",
    DIV2_TAIL_NUM = "Div2TailNum",
    DIV3_AIRPORT = "Div3Airport",
    DIV3_AIRPORT_ID = "Div3AirportID",
    DIV3_AIRPORT_SEQ_ID = "Div3AirportSeqID",
    DIV3_WHEELS_ON = "Div3WheelsOn",
    DIV3_TOTAL_GTIME = "Div3TotalGTime",
    DIV3_LONGEST_GTIME = "Div3LongestGTime",
    DIV3_WHEELS_OFF = "Div3WheelsOff",
    DIV3_TAIL_NUM = "Div3TailNum",
    DIV4_AIRPORT = "Div4Airport",
    DIV4_AIRPORT_ID = "Div4AirportID",
    DIV4_AIRPORT_SEQ_ID = "Div4AirportSeqID",
    DIV4_WHEELS_ON = "Div4WheelsOn",
    DIV4_TOTAL_GTIME = "Div4TotalGTime",
    DIV4_LONGEST_GTIME = "Div4LongestGTime",
    DIV4_WHEELS_OFF = "Div4WheelsOff",
    DIV4_TAIL_NUM = "Div4TailNum",
    DIV5_AIRPORT = "Div5Airport",
    DIV5_AIRPORT_ID = "Div5AirportID",
    DIV5_AIRPORT_SEQ_ID = "Div5AirportSeqID",
    DIV5_WHEELS_ON = "Div5WheelsOn",
    DIV5_TOTAL_GTIME = "Div5TotalGTime",
    DIV5_LONGEST_GTIME = "Div5LongestGTime",
    DIV5_WHEELS_OFF = "Div5WheelsOff",
    DIV5_TAIL_NUM = "Div5TailNum"
  )
}

bts_normalize_selected_names <- function(cols) {
  cols <- as.character(cols)
  map <- bts_selected_name_map()

  out <- unname(map[cols])
  out[is.na(out)] <- cols[is.na(out)]
  out
}

bts_rewrite_csv_header <- function(path) {
  bts_validate_csv_file(path, label = "BTS selected-fields CSV file")

  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) {
    stop("BTS selected-fields CSV file is empty.", call. = FALSE)
  }

  cols <- strsplit(lines[[1]], ",", fixed = TRUE)[[1]]
  lines[[1]] <- paste(bts_normalize_selected_names(cols), collapse = ",")

  writeLines(lines, path, useBytes = TRUE)

  bts_validate_csv_file(
    path,
    label = "BTS selected-fields CSV file with normalized header"
  )
}

bts_readme_files <- function(path) {
  list.files(
    path,
    pattern = "^readme\\.html$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )
}

bts_is_zip_raw <- function(x) {
  length(x) >= 4L &&
    identical(as.integer(x[1:4]), c(0x50L, 0x4bL, 0x03L, 0x04L))
}

bts_raw_to_text <- function(x, n = 500L) {
  if (!length(x)) {
    return("")
  }

  n <- min(length(x), n)

  tryCatch(
    rawToChar(x[seq_len(n)], multiple = FALSE),
    error = function(e) ""
  )
}

bts_response_is_html <- function(x) {
  first_text <- bts_raw_to_text(x)

  grepl(
    "<html|<!DOCTYPE html",
    first_text,
    ignore.case = TRUE
  )
}

#' Return the BTS download timeout
#'
#' Returns a timeout long enough for monthly BTS ZIP downloads while preserving
#' any larger user-configured timeout.
#'
#' @returns
#' Numeric timeout value, in seconds.
#'
#' @noRd
bts_download_timeout <- function() {
  timeout <- getOption("timeout", 60)
  timeout <- suppressWarnings(as.numeric(timeout[[1]]))

  if (is.na(timeout) || !is.finite(timeout) || timeout <= 0) {
    timeout <- 60
  }

  max(300, timeout)
}

#' Validate a downloaded BTS ZIP file
#'
#' Checks that a downloaded BTS ZIP file exists, is not empty, is not an HTML
#' response, is a readable ZIP archive, and contains at least one CSV file.
#'
#' @param path Path to the ZIP file.
#' @param label Human-readable file label used in error messages.
#'
#' @returns
#' Invisibly, the normalized path to the ZIP file.
#'
#' @noRd
bts_validate_zip_file <- function(path, label = "BTS ZIP file") {
  if (!file.exists(path)) {
    stop(
      sprintf("%s was not downloaded: %s", label, path),
      call. = FALSE
    )
  }

  size <- file.info(path)$size

  if (is.na(size) || size <= 0) {
    stop(
      sprintf("%s is empty: %s", label, path),
      call. = FALSE
    )
  }

  raw <- readBin(path, what = "raw", n = 500L)

  if (bts_response_is_html(raw)) {
    stop(
      sprintf("BTS returned HTML instead of %s.", label),
      call. = FALSE
    )
  }

  if (!bts_is_zip_raw(raw)) {
    stop(
      sprintf("%s is not a ZIP file: %s", label, path),
      call. = FALSE
    )
  }

  listed <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(listed) || !nrow(listed)) {
    stop(
      sprintf("%s is not a readable ZIP archive: %s", label, path),
      call. = FALSE
    )
  }

  has_csv <- any(grepl("\\.csv$", listed$Name, ignore.case = TRUE))

  if (!has_csv) {
    stop(
      sprintf("%s does not contain a CSV file: %s", label, path),
      call. = FALSE
    )
  }

  invisible(normalizePath(path, mustWork = TRUE))
}

#' Download a BTS file with a package-safe timeout
#'
#' Wraps [utils::download.file()] so monthly BTS downloads do not inherit an
#' unrealistically short timeout, and checks the returned status.
#'
#' @param url Source URL.
#' @param destfile Destination file path.
#' @param label Human-readable file label used in error messages.
#'
#' @returns
#' Invisibly, the normalized path to the downloaded file.
#'
#' @noRd
bts_download_file <- function(url, destfile, label = "BTS file") {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  options(timeout = bts_download_timeout())

  status <- tryCatch(
    utils::download.file(
      url = url,
      destfile = destfile,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) {
      attr(e, "bts_url") <- url
      attr(e, "bts_label") <- label
      stop(e)
    }
  )

  if (!identical(status, 0L)) {
    stop(
      sprintf("%s download failed: %s", label, url),
      call. = FALSE
    )
  }

  invisible(normalizePath(destfile, mustWork = TRUE))
}

bts_download_zip_file <- function(url, destfile, label = "BTS ZIP") {
  tryCatch(
    bts_download_file(
      url = url,
      destfile = destfile,
      label = label
    ),
    error = function(e) {
      stop(
        paste(
          sprintf("%s was not available at the static BTS PREZIP URL.", label),
          "The requested month may require the TranStats selected-fields",
          "download workflow instead.",
          sprintf("URL: %s", url)
        ),
        call. = FALSE
      )
    }
  )
}

bts_download_selected_zip <- function(year, month, destfile) {
  ym <- as_year_month(year, month, arg = "year/month")

  page_url <- bts_ontime_select_url()
  cookie_file <- tempfile("bts-cookies-")

  req_base <- function(url) {
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 slcflights") |>
      httr2::req_headers(
        Accept = paste(
          "text/html,application/xhtml+xml,application/xml;q=0.9,",
          "*/*;q=0.8",
          sep = ""
        ),
        `Accept-Language` = "en-US,en;q=0.9",
        Referer = "https://www.transtats.bts.gov/"
      ) |>
      httr2::req_cookie_preserve(cookie_file)
  }

  get_resp <- httr2::req_perform(req_base(page_url))
  doc <- xml2::read_html(httr2::resp_body_string(get_resp))

  form <- xml2::xml_find_first(doc, ".//form[@id='form1']")
  if (inherits(form, "xml_missing")) {
    stop(
      "Could not find the BTS On-Time Performance download form.",
      call. = FALSE
    )
  }

  action <- xml2::xml_attr(form, "action")
  post_url <- xml2::url_absolute(action, page_url)

  body <- bts_form_body(form)
  body$`__EVENTTARGET` <- "chkAllVars"
  body$`__EVENTARGUMENT` <- ""
  body$chkAllVars <- "on"
  body$cboYear <- as.character(ym$year)
  body$cboPeriod <- as.character(ym$month)
  body$btnDownload <- "Download"

  resp <- httr2::req_perform(
    req_base(post_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        Origin = "https://www.transtats.bts.gov",
        Referer = page_url,
        Accept = "application/zip,text/csv,text/plain,*/*"
      ) |>
      httr2::req_body_form(!!!body)
  )

  raw <- httr2::resp_body_raw(resp)

  if (bts_response_is_html(raw)) {
    stop(
      "BTS returned HTML instead of the selected-fields ZIP file.",
      call. = FALSE
    )
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  writeBin(raw, destfile)

  bts_validate_zip_file(
    destfile,
    label = "BTS selected-fields ZIP"
  )
}

#' Validate a downloaded BTS CSV file
#'
#' Checks that a downloaded or previously cached BTS CSV file exists and is not
#' empty.
#'
#' @param path Path to the CSV file.
#' @param label Human-readable file label used in error messages.
#'
#' @returns
#' Invisibly, the normalized path to the CSV file.
#'
#' @noRd
bts_validate_csv_file <- function(path, label = "BTS CSV file") {
  if (!file.exists(path)) {
    stop(
      sprintf("%s was not found: %s", label, path),
      call. = FALSE
    )
  }

  size <- file.info(path)$size

  if (is.na(size) || size <= 0) {
    stop(
      sprintf("%s is empty: %s", label, path),
      call. = FALSE
    )
  }

  invisible(normalizePath(path, mustWork = TRUE))
}

#' Download one BTS On-Time Performance monthly file
#'
#' Downloads and unzips one monthly BTS Reporting Carrier On-Time Performance
#' ZIP file into the raw local cache. If a CSV file is already present and
#' `overwrite` is `FALSE`, the existing CSV is reused.
#'
#' @param year Integer-like calendar year.
#' @param month Integer-like calendar month.
#' @param overwrite If `TRUE`, re-download even when a raw CSV is already
#'   present for the requested month.
#' @param keep_zip If `TRUE`, keep the downloaded ZIP file after extraction.
#'
#' @returns
#' Invisibly, the normalized path to the downloaded or reused CSV file.
#'
#' @noRd
download_bts_ontime_month <- function(
  year,
  month,
  overwrite = FALSE,
  keep_zip = TRUE
) {
  ym <- as_year_month(year, month, arg = "year/month")

  month_dir <- slc_cache_raw_ontime_month_dir(
    ym$year,
    ym$month,
    create = TRUE
  )

  zip_path <- file.path(
    month_dir,
    bts_ontime_zip_name(ym$year, ym$month)
  )

  existing_csv <- bts_csv_files(month_dir)

  if (length(existing_csv) && !isTRUE(overwrite)) {
    return(bts_validate_csv_file(
      existing_csv[[1]],
      label = "Existing BTS on-time CSV file"
    ))
  }

  url <- bts_ontime_url(ym$year, ym$month)

  message("Downloading flight data for ", format_year_month(ym), "...")

  zip_source <- "prezip"

  prezip_ok <- tryCatch(
    {
      bts_download_zip_file(
        url = url,
        destfile = zip_path,
        label = "BTS on-time ZIP"
      )

      TRUE
    },
    error = function(e) FALSE
  )

  if (!prezip_ok) {
    zip_source <- "selected-fields"

    message(
      "Static BTS PREZIP file was not available for ",
      format_year_month(ym),
      "; requesting TranStats selected-fields export..."
    )

    bts_download_selected_zip(
      year = ym$year,
      month = ym$month,
      destfile = zip_path
    )
  }

  bts_validate_zip_file(
    zip_path,
    label = "BTS on-time ZIP"
  )

  utils::unzip(zip_path, exdir = month_dir)

  readme <- bts_readme_files(month_dir)
  if (length(readme)) {
    unlink(readme, force = TRUE)
  }

  csv_files <- bts_csv_files(month_dir)

  if (!length(csv_files)) {
    stop(
      sprintf(
        "No CSV file was found after unzipping BTS data for %s.",
        format_year_month(ym)
      ),
      call. = FALSE
    )
  }

  if (identical(zip_source, "selected-fields")) {
    bts_rewrite_csv_header(csv_files[[1]])
  }

  if (!isTRUE(keep_zip)) {
    unlink(zip_path, force = TRUE)
  }

  bts_validate_csv_file(
    csv_files[[1]],
    label = "Downloaded BTS on-time CSV file"
  )
}

#' Download the BTS Airline ID lookup table
#'
#' Downloads the BTS TranStats Airline ID lookup table. If the file is already
#' present and `overwrite` is `FALSE`, the existing CSV is reused.
#'
#' @param destfile Destination path for the Airline ID lookup CSV.
#' @param overwrite If `TRUE`, re-download even when `destfile` already exists.
#'
#' @returns
#' Invisibly, the normalized path to the downloaded or reused CSV file.
#'
#' @noRd
download_bts_airline_id <- function(
  destfile,
  overwrite = FALSE
) {
  if (file.exists(destfile) && !isTRUE(overwrite)) {
    return(bts_validate_csv_file(
      destfile,
      label = "Existing BTS Airline ID CSV file"
    ))
  }

  url <- bts_airline_id_url()

  message("Downloading required airline lookup table...")

  resp <- httr2::req_perform(
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 slcflights") |>
      httr2::req_headers(
        Accept = "text/csv,text/plain,*/*",
        Referer = paste0(
          "https://www.transtats.bts.gov/",
          "DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr"
        )
      )
  )

  raw <- httr2::resp_body_raw(resp)

  if (bts_response_is_html(raw)) {
    stop(
      "BTS returned HTML instead of the Airline ID lookup CSV file.",
      call. = FALSE
    )
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  writeBin(raw, destfile)

  bts_validate_csv_file(
    destfile,
    label = "Downloaded BTS Airline ID CSV file"
  )
}

#' Download the BTS Master Coordinate table
#'
#' Downloads the BTS TranStats Master Coordinate support table. If the file is
#' already present and `overwrite` is `FALSE`, the existing CSV is reused.
#'
#' @param destfile Destination path for the Master Coordinate CSV.
#' @param overwrite If `TRUE`, re-download even when `destfile` already exists.
#'
#' @returns
#' Invisibly, the normalized path to the downloaded or reused CSV file.
#'
#' @noRd
download_bts_master_coords <- function(
  destfile = slc_cache_raw_coords_path(),
  overwrite = FALSE
) {
  if (file.exists(destfile) && !isTRUE(overwrite)) {
    return(bts_validate_csv_file(
      destfile,
      label = "Existing BTS Master Coordinate CSV file"
    ))
  }

  page_url <- bts_master_coords_url()
  cookie_file <- tempfile("bts-cookies-")

  req_base <- function(url) {
    httr2::request(url) |>
      httr2::req_user_agent("Mozilla/5.0 slcflights") |>
      httr2::req_headers(
        Accept = paste(
          "text/html,application/xhtml+xml,application/xml;q=0.9,",
          "*/*;q=0.8",
          sep = ""
        ),
        `Accept-Language` = "en-US,en;q=0.9",
        Referer = "https://www.transtats.bts.gov/"
      ) |>
      httr2::req_cookie_preserve(cookie_file)
  }

  message("Downloading required airport coordinates...")

  get_resp <- httr2::req_perform(req_base(page_url))
  doc <- xml2::read_html(httr2::resp_body_string(get_resp))

  form <- xml2::xml_find_first(doc, ".//form[@id='form1']")
  if (inherits(form, "xml_missing")) {
    stop(
      "Could not find the BTS Master Coordinate download form.",
      call. = FALSE
    )
  }

  action <- xml2::xml_attr(form, "action")
  post_url <- xml2::url_absolute(action, page_url)

  body <- bts_form_body(form)
  body$btnDownload <- "Download"

  resp <- httr2::req_perform(
    req_base(post_url) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        Origin = "https://www.transtats.bts.gov",
        Referer = page_url,
        Accept = "application/zip,text/csv,text/plain,*/*"
      ) |>
      httr2::req_body_form(!!!body)
  )

  raw <- httr2::resp_body_raw(resp)

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  if (bts_is_zip_raw(raw)) {
    return(extract_bts_master_coords_zip(raw, destfile))
  }

  if (bts_response_is_html(raw)) {
    stop(
      "BTS returned HTML instead of the Master Coordinate data file.",
      call. = FALSE
    )
  }

  writeBin(raw, destfile)

  bts_validate_csv_file(
    destfile,
    label = "Downloaded BTS Master Coordinate CSV file"
  )
}

bts_form_body <- function(form) {
  body <- list()

  inputs <- xml2::xml_find_all(form, ".//input")

  for (input in inputs) {
    nm <- xml2::xml_attr(input, "name")
    if (is.na(nm) || !nzchar(nm)) {
      next
    }

    type <- tolower(xml2::xml_attr(input, "type"))
    val <- xml2::xml_attr(input, "value")

    if (is.na(type)) {
      type <- "text"
    }

    if (identical(type, "checkbox")) {
      checked <- !is.na(xml2::xml_attr(input, "checked"))
      if (!checked) {
        next
      }

      if (is.na(val) || !nzchar(val)) {
        val <- "on"
      }
    }

    if (is.na(val)) {
      val <- ""
    }

    body[[nm]] <- val
  }

  selects <- xml2::xml_find_all(form, ".//select")

  for (select in selects) {
    nm <- xml2::xml_attr(select, "name")
    if (is.na(nm) || !nzchar(nm)) {
      next
    }

    selected <- xml2::xml_find_first(select, ".//option[@selected]")

    if (inherits(selected, "xml_missing")) {
      selected <- xml2::xml_find_first(select, ".//option[1]")
    }

    val <- xml2::xml_attr(selected, "value")

    if (is.na(val)) {
      val <- xml2::xml_text(selected)
    }

    body[[nm]] <- val
  }

  body
}

extract_bts_master_coords_zip <- function(raw, destfile) {
  zipfile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipfile, force = TRUE), add = TRUE)

  writeBin(raw, zipfile)

  unzip_dir <- tempfile("bts-coords-")
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(unzip_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(zipfile, exdir = unzip_dir)

  csv_files <- bts_csv_files(unzip_dir)

  if (!length(csv_files)) {
    stop(
      "BTS returned a coordinate ZIP file, but no CSV was found inside it.",
      call. = FALSE
    )
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

  ok <- file.copy(csv_files[[1]], destfile, overwrite = TRUE)
  if (!ok) {
    stop(
      sprintf("Failed to copy BTS Master Coordinate CSV to: %s", destfile),
      call. = FALSE
    )
  }

  bts_validate_csv_file(
    destfile,
    label = "Downloaded BTS Master Coordinate CSV file"
  )
}
