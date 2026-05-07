#' List Available Flight-Data Years
#'
#' Lists the years for which slcflights Parquet files are available.
#'
#' Available years may come from the packaged data installed with the package
#' or from a validated local user cache.
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
#' Reads one available Parquet file of main Salt Lake City flight records into
#' memory.
#'
#' Main records are flights where Salt Lake City's BTS airport ID appears in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param year Integer year to read.
#'
#' @returns
#' A data frame containing the selected year's main flight records.
#'
#' @details
#' This function reads the selected year's main Parquet file eagerly into
#' memory. The file may come from the packaged data installed with the package
#' or from a validated local user cache.
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
  read_main(year)
}

#' Read One Year's Diversion-Only Salt Lake City Flight Records
#'
#' Reads one available Parquet file of diversion-only Salt Lake City flight
#' records into memory.
#'
#' Diversion-only records are flights where Salt Lake City's BTS airport ID
#' appears in one of `Div1AirportID` through `Div5AirportID`, but not in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param year Integer year to read.
#'
#' @returns
#' A data frame containing the selected year's diversion-only flight records.
#'
#' @details
#' This function reads the selected year's diversion-only Parquet file eagerly
#' into memory. The file may come from the packaged data installed with the
#' package or from a validated local user cache.
#'
#' It errors if the requested year does not have an available diversion-only
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
  read_div(year)
}

#' Read Main Salt Lake City Flight Records for One, Many, or All Years
#'
#' Reads one, many, or all available main Parquet files into memory and combines
#' them row-wise into a single data frame.
#'
#' Main records are flights where Salt Lake City's BTS airport ID appears in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param years Integer vector of years to read. Use `NULL` to read all
#'   available main Parquet files.
#'
#' @returns
#' A data frame containing the row-bound contents of the selected main flight
#' records.
#'
#' @details
#' This function reads all selected files eagerly into memory. Selected files
#' may come from the packaged data installed with the package, from a validated
#' local user cache, or both.
#'
#' For larger workflows, [open_main()] may be more appropriate because it opens
#' the same files lazily as an Arrow dataset, allowing filtering or column
#' selection before materializing results in memory.
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
#' Reads one, many, or all available diversion-only Parquet files into memory
#' and combines them row-wise into a single data frame.
#'
#' Diversion-only records are flights where Salt Lake City's BTS airport ID
#' appears in one of `Div1AirportID` through `Div5AirportID`, but not in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param years Integer vector of years to read. Use `NULL` to read all
#'   available diversion-only Parquet files.
#'
#' @returns
#' A data frame containing the row-bound contents of the selected
#' diversion-only flight records.
#'
#' @details
#' This function reads all selected files eagerly into memory. Selected files
#' may come from the packaged data installed with the package, from a validated
#' local user cache, or both.
#'
#' It errors if any requested year does not have an available diversion-only
#' Parquet file.
#'
#' For larger workflows, [open_div()] may be more appropriate because it opens
#' the same files lazily as an Arrow dataset, allowing filtering or column
#' selection before materializing results in memory.
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
#' Opens one, many, or all available main Parquet files as a lazy Arrow dataset.
#'
#' Main records are flights where Salt Lake City's BTS airport ID appears in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param years Integer vector of years to open. Use `NULL` to open all
#'   available main Parquet files.
#'
#' @returns
#' An Arrow dataset over the selected main Parquet files.
#'
#' @details
#' This function does not read all rows into memory immediately.
#'
#' Selected files may come from the packaged data installed with the package,
#' from a validated local user cache, or both.
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
#' Opens one, many, or all available diversion-only Parquet files as a lazy
#' Arrow dataset.
#'
#' Diversion-only records are flights where Salt Lake City's BTS airport ID
#' appears in one of `Div1AirportID` through `Div5AirportID`, but not in
#' `OriginAirportID` or `DestAirportID`.
#'
#' @param years Integer vector of years to open. Use `NULL` to open all
#'   available diversion-only Parquet files.
#'
#' @returns
#' An Arrow dataset over the selected diversion-only Parquet files.
#'
#' @details
#' This function does not read all rows into memory immediately.
#'
#' Selected files may come from the packaged data installed with the package,
#' from a validated local user cache, or both.
#'
#' It is intended for workflows where you want to filter rows, select columns,
#' or otherwise work lazily before collecting results into memory. Use
#' [read_div()] when you want an in-memory data frame instead.
#'
#' It errors if any requested year does not have an available diversion-only
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

#' Read the Airport Coordinate Table
#'
#' Reads the airport coordinate table supporting the currently available
#' slcflights data.
#'
#' @returns
#' A data frame containing airport sequence identifiers and associated airport
#' metadata for airports referenced by the available flight data.
#'
#' @details
#' Before any local data update, this function reads the coordinate table
#' installed with the package. After a successful local data update, it reads
#' the validated cached coordinate table.
#'
#' The coordinate table is derived from the BTS TranStats Master Coordinate
#' support table.
#'
#' It is intended for joins against airport sequence identifier fields in the
#' flight data, including fields such as `OriginAirportSeqID`,
#' `DestAirportSeqID`, and diversion airport sequence identifier fields.
#'
#' The table contains the airport-level metadata used to enrich the flight
#' records with latitude, longitude, and date-bounded airport information.
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

#' Read the Field Dictionary
#'
#' Reads the field dictionary for the installed slcflights data.
#'
#' @returns
#' A data frame describing fields that appear in the flight-record Parquet
#' files and the airport coordinate table.
#'
#' @details
#' The field dictionary includes the field name, field group, source table,
#' description, presence in main records, presence in diversion-only records,
#' presence in the coordinate table, and additional notes.
#'
#' Values of `main_presence`, `div_presence`, and `coords_presence` use
#' `"always"`, `"sometimes"`, or `"never"` to describe whether a field appears
#' in that data grouping.
#'
#' @seealso [read_main()], [read_div()], [open_main()], [open_div()],
#'   [read_coords()]
#'
#' @examples
#' fields <- read_field_dictionary()
#' head(fields)
#'
#' @export
read_field_dictionary <- function() {
  readr::read_csv(
    system.file(
      "extdata",
      "csv",
      "field_dictionary.csv",
      package = "slcflights",
      mustWork = TRUE
    ),
    show_col_types = FALSE
  )
}
