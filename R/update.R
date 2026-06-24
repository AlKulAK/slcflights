# Update endpoint and cache management helpers --------------------------------

bts_candidate_months <- function(today = Sys.Date()) {
  today <- as.Date(today)

  current <- as_year_month(
    as.integer(format(today, "%Y")),
    as.integer(format(today, "%m")),
    arg = "today"
  )

  indexes <- seq.int(
    year_month_index(current),
    year_month_index(slc_first_download())
  )

  months <- lapply(indexes, index_to_year_month)

  data.frame(
    year = vapply(months, `[[`, integer(1), "year"),
    month = vapply(months, `[[`, integer(1), "month")
  )
}

bts_probe_month_url <- function(year, month) {
  url <- bts_ontime_url(year, month)
  zip_path <- tempfile("slcflights-bts-probe-", fileext = ".zip")
  on.exit(unlink(zip_path, force = TRUE), add = TRUE)

  ok <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_user_agent("Mozilla/5.0 slcflights") |>
        httr2::req_timeout(60) |>
        httr2::req_perform(path = zip_path)

      status <- httr2::resp_status(resp)

      zip_ok <- status >= 200L &&
        status < 400L &&
        file.exists(zip_path) &&
        !is.na(file.info(zip_path)$size) &&
        file.info(zip_path)$size > 0

      if (!zip_ok) {
        FALSE
      } else {
        listed <- tryCatch(
          utils::unzip(zip_path, list = TRUE),
          error = function(e) NULL,
          warning = function(w) NULL
        )

        !is.null(listed) &&
          nrow(listed) > 0L &&
          any(grepl("\\.csv$", listed$Name, ignore.case = TRUE))
      }
    },
    error = function(e) FALSE
  )

  isTRUE(ok)
}

bts_latest_month <- function(today = Sys.Date()) {
  candidates <- bts_candidate_months(today = today)

  for (i in seq_len(nrow(candidates))) {
    year <- candidates$year[[i]]
    month <- candidates$month[[i]]

    if (bts_probe_month_url(year, month)) {
      return(as_year_month(year, month, arg = "latest"))
    }
  }

  stop(
    paste(
      "Could not determine the latest available BTS month.",
      "Try specifying `until`, for example `until = \"2024-07\"`."
    ),
    call. = FALSE
  )
}

resolve_update_until <- function(until = "latest") {
  until <- normalize_update_until(until)

  if (identical(until, "latest")) {
    return(bts_latest_month())
  }

  validate_update_until(until)
}

update_months_for_until <- function(until = "latest") {
  update_month_sequence(resolve_update_until(until))
}

download_update_months <- function(months, overwrite = FALSE) {
  months <- validate_cache_months(months)

  out <- vector("list", nrow(months))

  for (i in seq_len(nrow(months))) {
    out[[i]] <- download_bts_ontime_month(
      year = months$year[[i]],
      month = months$month[[i]],
      overwrite = overwrite,
      keep_zip = TRUE
    )
  }

  unlist(out, use.names = FALSE)
}

download_update_coords <- function(overwrite = FALSE) {
  download_bts_master_coords(
    destfile = slc_cache_raw_coords_path(create = TRUE),
    overwrite = overwrite
  )
}

download_update_airlines <- function(overwrite = FALSE) {
  download_bts_airline_id(
    destfile = slc_cache_raw_airlines_path(create = TRUE),
    overwrite = overwrite
  )
}

build_update_cache <- function(months) {
  message("Building local slcflights cache...")

  cache_stage_build(
    months = months,
    root = slc_cache_staging_root(create = TRUE),
    coords_in = slc_cache_raw_coords_path(create = FALSE),
    airlines_in = slc_cache_raw_airlines_path(create = FALSE),
    include_installed = TRUE
  )

  message("Activating local slcflights cache...")

  cache_stage_promote(
    staging_root = slc_cache_staging_root(create = FALSE),
    active_root = slc_cache_active_root(create = FALSE)
  )
}

resolve_db_until <- function(until) {
  until <- normalize_db_until(until)

  if (identical(until, "latest")) {
    return(bts_latest_month())
  }

  validate_db_until(until)
}

db_months_for_until <- function(until) {
  db_month_sequence(resolve_db_until(until))
}

validate_download_months <- function(months, arg = "months") {
  if (!is.data.frame(months)) {
    stop(sprintf("`%s` must be a data frame.", arg), call. = FALSE)
  }

  if (!all(c("year", "month") %in% names(months))) {
    stop(
      sprintf("`%s` must contain `year` and `month` columns.", arg),
      call. = FALSE
    )
  }

  if (!nrow(months)) {
    stop(sprintf("`%s` must contain at least one month.", arg), call. = FALSE)
  }

  months <- months[, c("year", "month"), drop = FALSE]
  months$year <- as.integer(months$year)
  months$month <- as.integer(months$month)

  for (i in seq_len(nrow(months))) {
    as_year_month(
      months$year[[i]],
      months$month[[i]],
      arg = sprintf("%s[%s, ]", arg, i)
    )
  }

  months <- months[order(months$year, months$month), , drop = FALSE]
  row.names(months) <- NULL

  if (any(duplicated(months))) {
    stop(
      sprintf("`%s` must not contain duplicate months.", arg),
      call. = FALSE
    )
  }

  months
}

download_db_months <- function(months, overwrite = FALSE) {
  months <- validate_download_months(months)

  out <- vector("list", nrow(months))

  for (i in seq_len(nrow(months))) {
    out[[i]] <- download_bts_ontime_month(
      year = months$year[[i]],
      month = months$month[[i]],
      overwrite = overwrite,
      keep_zip = TRUE
    )
  }

  unlist(out, use.names = FALSE)
}

download_db_coords <- function(overwrite = FALSE) {
  download_bts_master_coords(
    destfile = slc_cache_raw_coords_path(create = TRUE),
    overwrite = overwrite
  )
}

download_db_airlines <- function(overwrite = FALSE) {
  download_bts_airline_id(
    destfile = slc_cache_raw_airlines_path(create = TRUE),
    overwrite = overwrite
  )
}

build_db_cache <- function(months) {
  message("Building local slcflights database...")

  db_stage_build(
    months = months,
    root = slc_db_staging_root(create = TRUE),
    coords_in = slc_cache_raw_coords_path(create = FALSE),
    airlines_in = slc_cache_raw_airlines_path(create = FALSE)
  )

  message("Activating local slcflights database...")

  db_stage_promote(
    staging_root = slc_db_staging_root(create = FALSE),
    active_root = slc_db_root(create = FALSE)
  )
}

extend_db_cache <- function(months, extend_months) {
  message("Extending local slcflights database...")

  db_stage_extend(
    months = months,
    extend_months = extend_months,
    active_root = slc_db_root(create = FALSE),
    root = slc_db_staging_root(create = TRUE),
    coords_in = slc_cache_raw_coords_path(create = FALSE),
    airlines_in = slc_cache_raw_airlines_path(create = FALSE)
  )

  message("Activating extended local slcflights database...")

  db_stage_promote(
    staging_root = slc_db_staging_root(create = FALSE),
    active_root = slc_db_root(create = FALSE)
  )
}

slc_db_info <- function(root = NULL) {
  if (is.null(root)) {
    root <- slc_db_root(create = FALSE)
  }

  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  manifest <- read_db_manifest_if_active(root = root)

  if (is.null(manifest)) {
    return(list(
      root = root,
      exists = FALSE,
      complete = FALSE,
      months = data.frame(year = integer(), month = integer()),
      endpoint = NULL,
      manifest = NULL
    ))
  }

  complete <- db_months_are_complete(root = root)
  months <- db_months_from_manifest(manifest)

  list(
    root = root,
    exists = TRUE,
    complete = complete,
    months = months,
    endpoint = db_endpoint_from_manifest(manifest),
    manifest = manifest
  )
}

print_db_info <- function(info) {
  if (!isTRUE(info$exists)) {
    message("No local slcflights database is currently active.")
    message("Database location: ", info$root)
    return(invisible(info))
  }

  endpoint <- format_year_month(info$endpoint)

  message("Local slcflights database is active.")
  message("Database location: ", info$root)
  message("Database data through: ", endpoint)
  message(
    "Database status: ",
    if (isTRUE(info$complete)) "complete" else "incomplete"
  )

  invisible(info)
}

slc_cache_info <- function(root = NULL) {
  if (is.null(root)) {
    root <- slc_cache_active_root(create = FALSE)
  }

  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  manifest <- read_cache_manifest(root = root)
  complete <- cache_months_are_complete(root = root)

  months <- if (is.null(manifest)) {
    data.frame(year = integer(), month = integer())
  } else {
    cache_months_from_manifest(manifest)
  }

  endpoint <- if (is.null(manifest)) {
    NULL
  } else {
    cache_endpoint_from_manifest(manifest)
  }

  list(
    root = root,
    exists = !is.null(manifest),
    complete = complete,
    months = months,
    endpoint = endpoint,
    manifest = manifest
  )
}

print_cache_info <- function(info) {
  if (!isTRUE(info$exists)) {
    message("No local slcflights cache is currently active.")
    message("Cache location: ", info$root)
    return(invisible(info))
  }

  endpoint <- format_year_month(info$endpoint)

  message("Local slcflights cache is active.")
  message("Cache location: ", info$root)
  message("Cached data through: ", endpoint)
  message(
    "Cache status: ",
    if (isTRUE(info$complete)) "complete" else "incomplete"
  )

  invisible(info)
}

clear_cache_root <- function(root, confirm = interactive()) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  if (isTRUE(confirm)) {
    answer <- readline(
      paste0(
        "Delete the local slcflights cache at ",
        root,
        "? [y/N] "
      )
    )

    if (!tolower(answer) %in% c("y", "yes")) {
      message("Cache was not deleted.")
      return(invisible(FALSE))
    }
  }

  if (dir.exists(root)) {
    unlink(root, recursive = TRUE, force = TRUE)
  }

  invisible(TRUE)
}

#' Build or Extend the Local slcflights Database
#'
#' Downloads BTS monthly on-time performance files and builds a local
#' Salt Lake City-focused database.
#'
#' @param until Database endpoint. Use `"latest"`, a four-digit year, a string
#'   of the form `"YYYY-MM"`, or `c(year, month)`.
#' @param overwrite If `TRUE`, re-downloads raw BTS source files already
#'   present in the local raw-data cache.
#' @param confirm If `TRUE`, allows operations that download and process more
#'   than 24 monthly BTS files.
#'
#' @returns
#' Invisibly, a list describing the active local database.
#'
#' @details
#' The local database always begins with October 1987. If no local database is
#' active, `build_slcflights_db()` builds one from October 1987 through the
#' requested endpoint. If a compatible local database already exists, the
#' function extends it forward to the requested endpoint.
#'
#' Extensions are contiguous. For example, if the active database ends with
#' June 2024, a request through December 2024 downloads and processes July
#' through December 2024.
#'
#' For large initial builds, use `confirm = TRUE`.
#'
#' @seealso [update_slcflights_data()], [slcflights_cache_info()],
#'   [clear_slcflights_cache()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#' build_slcflights_db(until = "latest", confirm = TRUE)
#' }
#'
#' @export
build_slcflights_db <- function(
  until,
  overwrite = FALSE,
  confirm = FALSE
) {
  message("Resolving requested slcflights database endpoint...")

  active <- read_db_manifest(root = slc_db_root(create = FALSE))
  endpoint <- resolve_db_until(until)
  months <- db_month_sequence(endpoint)

  if (is.null(active)) {
    dl_months <- months
  } else {
    current_end <- db_endpoint_from_manifest(active)

    dl_months <- db_extension_months(
      current_end = current_end,
      until = endpoint
    )

    if (!nrow(dl_months)) {
      stop(
        paste(
          "The local slcflights database already covers the requested",
          "endpoint."
        ),
        call. = FALSE
      )
    }
  }

  if (nrow(dl_months) > 24L && !isTRUE(confirm)) {
    stop(
      paste(
        "This operation would download and process",
        nrow(dl_months),
        "monthly BTS files.",
        "Run again with `confirm = TRUE` to proceed."
      ),
      call. = FALSE
    )
  }

  message("Building slcflights through ", format_year_month(endpoint), ".")

  download_db_months(
    months = dl_months,
    overwrite = overwrite
  )

  message("Downloading airport coordinate data...")

  download_db_coords(overwrite = overwrite)

  message("Downloading airline lookup data...")

  download_db_airlines(overwrite = overwrite)

  if (is.null(active)) {
    build_db_cache(months)
  } else {
    extend_db_cache(
      months = months,
      extend_months = dl_months
    )
  }

  info <- slc_db_info()

  message("slcflights database build complete.")

  invisible(info)
}

#' Update Locally Available slcflights Data
#'
#' Builds or extends the local slcflights database.
#'
#' @param until Database endpoint. Use `"latest"`, a four-digit year, a string
#'   of the form `"YYYY-MM"`, or `c(year, month)`.
#' @param overwrite If `TRUE`, re-downloads raw BTS source files already
#'   present in the local raw-data cache.
#'
#' @returns
#' Invisibly, a list describing the active local database.
#'
#' @seealso [build_slcflights_db()], [slcflights_cache_info()],
#'   [clear_slcflights_cache()]
#'
#' @examples
#' \dontrun{
#' update_slcflights_data()
#' update_slcflights_data(until = "2024-12")
#' }
#'
#' @export
update_slcflights_data <- function(until = "latest", overwrite = FALSE) {
  build_slcflights_db(
    until = until,
    overwrite = overwrite,
    confirm = TRUE
  )
}

#' Show slcflights Local Database Information
#'
#' Reports whether a local slcflights database is active and complete.
#'
#' @returns
#' Invisibly, a list describing the active local database. The list contains:
#'
#' - `root`: normalized path to the active local database directory.
#' - `exists`: `TRUE` if an active database manifest exists.
#' - `complete`: `TRUE` if the active database has the expected files.
#' - `months`: data frame of available year-month pairs.
#' - `endpoint`: final available year-month, or `NULL` when no database is
#'   active.
#' - `manifest`: parsed database manifest, or `NULL` when no database is active.
#'
#' @details
#' This function only reports local database state. It does not download, build,
#' or delete data.
#'
#' @seealso [build_slcflights_db()], [update_slcflights_data()],
#'   [clear_slcflights_cache()]
#'
#' @examples
#' \dontrun{
#' slcflights_cache_info()
#' }
#'
#' @export
slcflights_cache_info <- function() {
  info <- slc_db_info()
  print_db_info(info)
}

#' Clear the slcflights Local Database
#'
#' Removes the active local slcflights database.
#'
#' @param confirm If `TRUE`, asks for confirmation before deleting the local
#'   database.
#'
#' @returns
#' Invisibly, `TRUE` if the local database was deleted or did not exist, and
#' `FALSE` if deletion was cancelled.
#'
#' @details
#' This removes the active local slcflights database stored under the user
#' cache directory returned by `tools::R_user_dir("slcflights", "cache")`.
#'
#' @seealso [build_slcflights_db()], [update_slcflights_data()],
#'   [slcflights_cache_info()]
#'
#' @examples
#' \dontrun{
#' clear_slcflights_cache()
#' }
#'
#' @export
clear_slcflights_cache <- function(confirm = interactive()) {
  clear_cache_root(
    root = slc_db_root(create = FALSE),
    confirm = confirm
  )
}
