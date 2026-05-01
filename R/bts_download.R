# Internal BTS download helpers ----------------------------------------------
#
# These helpers prepare raw BTS source files for future cache builds. They are
# intentionally unexported. Users should eventually call a package-level update
# function, not these low-level BTS helpers.

bts_ontime_zip_name <- function(year, month) {
  ym <- as_year_month(year, month, arg = "year/month")

  sprintf(
    "On_Time_Reporting_Carrier_On_Time_Performance_1987_present_%d_%d.zip",
    ym$year,
    ym$month
  )
}

bts_ontime_url <- function(year, month) {
  sprintf(
    "https://transtats.bts.gov/PREZIP/%s",
    bts_ontime_zip_name(year, month)
  )
}

bts_master_coords_url <- function() {
  paste0(
    "https://www.transtats.bts.gov/DL_SelectFields.aspx?",
    "QO_fu146_anzr=N8vn6v10+f722146+gnoyr5&gnoyr_VQ=FLL"
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

  utils::download.file(
    url = url,
    destfile = zip_path,
    mode = "wb",
    quiet = TRUE
  )

  if (!file.exists(zip_path)) {
    stop(
      sprintf("BTS on-time ZIP was not downloaded: %s", zip_path),
      call. = FALSE
    )
  }

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

  if (!isTRUE(keep_zip)) {
    unlink(zip_path, force = TRUE)
  }

  bts_validate_csv_file(
    csv_files[[1]],
    label = "Downloaded BTS on-time CSV file"
  )
}

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

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package `httr2` is required to download BTS coordinates.",
      call. = FALSE
    )
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package `xml2` is required to download BTS coordinates.",
      call. = FALSE
    )
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
  writeBin(raw, zipfile)

  unzip_dir <- tempfile("bts-coords-")
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

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
