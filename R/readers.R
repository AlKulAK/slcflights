#' List Available Flight-Data Years
#'
#' Lists the years for which slcflights Parquet files are available.
#'
#' Available years may come from the packaged data installed with the package
#' or, in later update workflows, from a validated local user cache.
#'
#' @param type Which flight-data grouping to inspect: `"main"` for main records
#'   or `"div"` for diversion-only records.
#'
#' @returns
#' An integer vector of available years, sorted in ascending order.
#'
#' @examples
#' available_years("main")
#'
#' \dontrun{
#' available_years("div")
#' }
#'
#' @export
available_years <- function(type = c("main", "div")) {
  type <- match.arg(type)
  slc_available_data_years(type)
}

#' Read One Year's Main Salt Lake City Flight Records
#'
#' Reads one packaged Parquet file of main Salt Lake City flight records into
#' memory.
#'
#' Main records are flights where Salt Lake City appears in the primary origin
#' or destination airport fields.
#'
#' @param year Integer year to read.
#'
#' @returns
#' A data frame containing the selected year's packaged main flight records.
#'
#' @details
#' This function reads the selected year's packaged main Parquet file eagerly
#' into memory.
#'
#' Use [read_main()] to read multiple years at once. Use [open_main()] to work
#' lazily with one or more years as an Arrow dataset.
#'
#' @seealso [read_main()], [open_main()], [read_year_div()]
#'
#' @examples
#' x <- read_year_main(1987)
#' head(x)
#'
#' @export
read_year_main <- function(year) {
  arrow::read_parquet(.parquet_paths("main", year)[[1]])
}

#' Read One Year's Diversion-Only Salt Lake City Flight Records
#'
#' Reads one packaged Parquet file of diversion-only Salt Lake City flight
#' records into memory.
#'
#' Diversion-only records are flights where Salt Lake City appears only in
#' diversion airport fields and not in the primary origin or destination
#' airport fields.
#'
#' @param year Integer year to read.
#'
#' @returns
#' A data frame containing the selected year's packaged diversion-only flight
#' records.
#'
#' @details
#' This function reads the selected year's packaged diversion-only Parquet file
#' eagerly into memory.
#'
#' It errors if the requested year does not have a packaged diversion-only
#' Parquet file.
#'
#' Use [read_div()] to read multiple years at once. Use [open_div()] to work
#' lazily with one or more years as an Arrow dataset.
#'
#' @seealso [read_div()], [open_div()], [read_year_main()]
#'
#' @examples
#' \dontrun{
#' x <- read_year_div(2015)
#' head(x)
#' }
#'
#' @export
read_year_div <- function(year) {
  arrow::read_parquet(.parquet_paths("div", year)[[1]])
}

#' Read Main Salt Lake City Flight Records for One, Many, or All Years
#'
#' Reads one, many, or all packaged main Parquet files into memory and combines
#' them row-wise into a single data frame.
#'
#' Main records are flights where Salt Lake City appears in the primary origin
#' or destination airport fields.
#'
#' @param years Integer vector of years to read. Use `NULL` to read all
#'   available packaged main Parquet files.
#'
#' @returns
#' A data frame containing the row-bound contents of the selected packaged main
#' flight records.
#'
#' @details
#' This function reads all selected files eagerly into memory.
#'
#' For larger workflows, [open_main()] may be more appropriate because it opens
#' the same packaged files lazily as an Arrow dataset, allowing filtering or
#' column selection before materializing results in memory.
#'
#' @seealso [open_main()], [read_year_main()], [read_div()]
#'
#' @examples
#' x1 <- read_main(1987)
#' x2 <- read_main(1987:1988)
#'
#' \dontrun{
#' x_all <- read_main()
#' }
#'
#' @export
read_main <- function(years = NULL) {
  paths <- .parquet_paths("main", years)

  out <- lapply(paths, arrow::read_parquet)
  out <- lapply(out, as.data.frame)

  if (length(out) == 1) {
    return(out[[1]])
  }

  do.call(rbind, out)
}

#' Read Diversion-Only Salt Lake City Flight Records for One, Many, or All Years
#'
#' Reads one, many, or all packaged diversion-only Parquet files into memory
#' and combines them row-wise into a single data frame.
#'
#' Diversion-only records are flights where Salt Lake City appears only in
#' diversion airport fields and not in the primary origin or destination
#' airport fields.
#'
#' @param years Integer vector of years to read. Use `NULL` to read all
#'   available packaged diversion-only Parquet files.
#'
#' @returns
#' A data frame containing the row-bound contents of the selected packaged
#' diversion-only flight records.
#'
#' @details
#' This function reads all selected files eagerly into memory.
#'
#' It errors if any requested year does not have a packaged diversion-only
#' Parquet file.
#'
#' For larger workflows, [open_div()] may be more appropriate because it opens
#' the same packaged files lazily as an Arrow dataset, allowing filtering or
#' column selection before materializing results in memory.
#'
#' @seealso [open_div()], [read_year_div()], [read_main()]
#'
#' @examples
#' \dontrun{
#' x1 <- read_div(2015)
#' head(x1)
#'
#' x2 <- read_div(2015:2016)
#' head(x2)
#'
#' x_all <- read_div()
#' }
#'
#' @export
read_div <- function(years = NULL) {
  paths <- .parquet_paths("div", years)

  out <- lapply(paths, arrow::read_parquet)
  out <- lapply(out, as.data.frame)

  if (length(out) == 1) {
    return(out[[1]])
  }

  do.call(rbind, out)
}

#' Open Main Salt Lake City Flight Records Lazily as an Arrow Dataset
#'
#' Opens one, many, or all packaged main Parquet files as a lazy Arrow dataset.
#'
#' Main records are flights where Salt Lake City appears in the primary origin
#' or destination airport fields.
#'
#' @param years Integer vector of years to open. Use `NULL` to open all
#'   available packaged main Parquet files.
#'
#' @returns
#' An Arrow dataset over the selected packaged main Parquet files.
#'
#' @details
#' This function does not read all rows into memory immediately.
#'
#' It is intended for workflows where you want to filter rows, select columns,
#' or otherwise work lazily before collecting results into memory. Use
#' [read_main()] when you want an in-memory data frame instead.
#'
#' @seealso [read_main()], [read_year_main()], [open_div()]
#'
#' @examples
#' ds <- open_main(1987:1988)
#' ds
#'
#' @export
open_main <- function(years = NULL) {
  arrow::open_dataset(.parquet_paths("main", years), format = "parquet")
}

#' Open Diversion-Only Salt Lake City Flight Records Lazily as an Arrow Dataset
#'
#' Opens one, many, or all packaged diversion-only Parquet files as a lazy
#' Arrow dataset.
#'
#' Diversion-only records are flights where Salt Lake City appears only in
#' diversion airport fields and not in the primary origin or destination
#' airport fields.
#'
#' @param years Integer vector of years to open. Use `NULL` to open all
#'   available packaged diversion-only Parquet files.
#'
#' @returns
#' An Arrow dataset over the selected packaged diversion-only Parquet files.
#'
#' @details
#' This function does not read all rows into memory immediately.
#'
#' It is intended for workflows where you want to filter rows, select columns,
#' or otherwise work lazily before collecting results into memory. Use
#' [read_div()] when you want an in-memory data frame instead.
#'
#' It errors if any requested year does not have a packaged diversion-only
#' Parquet file.
#'
#' @seealso [read_div()], [read_year_div()], [open_main()]
#'
#' @examples
#' \dontrun{
#' ds <- open_div(2015:2016)
#' ds
#' }
#'
#' @export
open_div <- function(years = NULL) {
  arrow::open_dataset(.parquet_paths("div", years), format = "parquet")
}

#' Read the Packaged Airport Coordinate Table
#'
#' Reads the packaged CSV table of airport coordinates used to support the
#' packaged flight records.
#'
#' @returns
#' A data frame containing airport sequence identifiers and associated airport
#' metadata for airports referenced by the packaged flight data.
#'
#' @details
#' The packaged coordinate table is derived from the BTS TranStats Master
#' Coordinate support table.
#'
#' It is intended for joins against airport sequence identifier fields in the
#' flight data, including fields such as `OriginAirportSeqID`,
#' `DestAirportSeqID`, and diversion airport sequence identifier fields.
#'
#' The table contains the airport-level metadata used to enrich the packaged
#' flight records with latitude, longitude, and date-bounded airport
#' information.
#'
#' @seealso [read_main()], [read_div()], [open_main()], [open_div()]
#'
#' @examples
#' x <- read_coords()
#' head(x)
#'
#' @export
read_coords <- function() {
  readr::read_csv(.coords_path(), show_col_types = FALSE)
}
