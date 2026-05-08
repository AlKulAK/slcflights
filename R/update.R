# Public update and cache management API --------------------------------------

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

build_update_cache <- function(months) {
  message("Building local slcflights cache...")

  cache_stage_build(
    months = months,
    root = slc_cache_staging_root(create = TRUE),
    coords_in = slc_cache_raw_coords_path(create = FALSE),
    include_installed = TRUE
  )

  message("Activating local slcflights cache...")

  cache_stage_promote(
    staging_root = slc_cache_staging_root(create = FALSE),
    active_root = slc_cache_active_root(create = FALSE)
  )
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

#' Update Locally Available slcflights Data
#'
#' Downloads newly available BTS flight data and builds a validated local cache.
#'
#' @param until Endpoint through which to update. Use `"latest"` to download
#'   all currently available new data, a string of the form `"YYYY-MM"`, or
#'   `c(year, month)`.
#' @param overwrite If `TRUE`, re-downloads raw BTS source files already present
#'   in the local raw-data cache.
#'
#' @returns
#' Invisibly, a list describing the active local cache. The list has the same
#' structure as the value returned by [slcflights_cache_info()].
#'
#' @details
#' The package ships data through June 2024. Local updates always begin with
#' July 2024 and must form a consecutive extension of the packaged data.
#'
#' By default, `update_slcflights_data()` checks BTS for the latest available
#' monthly file and updates through that month. You may also specify an explicit
#' endpoint, for example `until = "2024-07"`, to perform a smaller update.
#'
#' Updated data are stored in the user cache returned by
#' `tools::R_user_dir("slcflights", "cache")`. The installed package files are
#' never modified.
#'
#' After a compatible cache is active, the ordinary reader functions use it
#' automatically. For example, [read_year_main()] combines installed and cached
#' data for a year that spans both sources.
#'
#' @seealso [slcflights_cache_info()], [clear_slcflights_cache()]
#'
#' @examples
#' \dontrun{
#' update_slcflights_data()
#' update_slcflights_data(until = "2024-07")
#' }
#'
#' @export
update_slcflights_data <- function(until = "latest", overwrite = FALSE) {
  message("Resolving requested slcflights update endpoint...")

  months <- update_months_for_until(until)
  endpoint <- as_year_month(
    months$year[[nrow(months)]],
    months$month[[nrow(months)]]
  )

  message("Updating slcflights through ", format_year_month(endpoint), ".")

  download_update_months(
    months = months,
    overwrite = overwrite
  )

  message("Downloading airport coordinate data...")

  download_update_coords(overwrite = overwrite)

  build_update_cache(months)

  info <- slc_cache_info()

  message("slcflights update complete.")

  invisible(info)
}

#' Show slcflights Cache Information
#'
#' Reports whether a local slcflights data cache is active and complete.
#'
#' @returns
#' Invisibly, a list describing the active local cache. The list contains:
#'
#' - `root`: normalized path to the active cache directory.
#' - `exists`: `TRUE` if an active cache manifest exists.
#' - `complete`: `TRUE` if the active cache has the expected cached files.
#' - `months`: data frame of cached year-month pairs.
#' - `endpoint`: final cached year-month, or `NULL` when no cache is active.
#' - `manifest`: parsed cache manifest, or `NULL` when no cache is active.
#'
#' @details
#' This function only reports cache state. It does not download or
#' build data, modify the cache, or modify the installed package files.
#'
#' @seealso [update_slcflights_data()], [clear_slcflights_cache()]
#'
#' @examples
#' slcflights_cache_info()
#'
#' @export
slcflights_cache_info <- function() {
  info <- slc_cache_info()
  print_cache_info(info)
}

#' Clear the slcflights Local Cache
#'
#' Removes locally downloaded and locally built slcflights update data.
#'
#' @param confirm If `TRUE`, asks for confirmation before deleting the cache.
#'
#' @returns
#' Invisibly, `TRUE` if the cache was deleted or did not exist, and `FALSE` if
#' deletion was cancelled.
#'
#' @details
#' This removes the slcflights user cache returned by
#' `tools::R_user_dir("slcflights", "cache")`. It does not modify the installed
#' package files.
#'
#' After the cache is cleared, the ordinary reader functions use only the
#' installed package data.
#'
#' @seealso [update_slcflights_data()], [slcflights_cache_info()]
#'
#' @examples
#' \dontrun{
#' clear_slcflights_cache()
#' }
#'
#' @export
clear_slcflights_cache <- function(confirm = interactive()) {
  clear_cache_root(
    root = slc_cache_root(create = FALSE),
    confirm = confirm
  )
}
