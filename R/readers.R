#' List Available Flight-Data Years
#'
#' Lists the years for which slcflights Parquet files are available in the
#' active local database.
#'
#' @param type Which flight-data grouping to inspect: `"main"` for main records
#'   or `"div"` for diversion-only records. Defaults to `"main"`.
#'
#' @returns
#' An integer vector of available years for the requested flight-data grouping,
#' sorted in ascending order.
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' available_years()
#' available_years(type = "div")
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
#' memory. The file is read from the active local database.
#'
#' It errors if the requested year does not have an available main Parquet file.
#'
#' Use [read_main()] to read multiple years at once. Use [open_main()] to work
#' lazily with one or more years as an Arrow dataset.
#'
#' @seealso [read_main()], [open_main()], [read_year_div()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' x <- read_year_main(1987)
#' head(x)
#' }
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
#' into memory. The file is read from the active local database.
#'
#' It errors if the requested year does not have an available diversion-only
#' Parquet file.
#'
#' Diversion-only records begin in October 2008.
#'
#' Use [read_div()] to read multiple years at once. Use [open_div()] to work
#' lazily with one or more years as an Arrow dataset.
#'
#' @seealso [read_div()], [open_div()], [read_year_main()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
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
#' This function reads all selected files eagerly into memory. The files are
#' read from the active local database.
#'
#' Calling `read_main()` without a year argument reads all available main years
#' into memory.
#'
#' For larger workflows, [open_main()] may be more appropriate because it opens
#' the same files lazily as an Arrow dataset, allowing filtering or column
#' selection before materializing results in memory.
#'
#' @seealso [open_main()], [read_year_main()], [read_div()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' x1 <- read_main(1987)
#' x2 <- read_main(1987:1988)
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
#' This function reads all selected files eagerly into memory. The files are
#' read from the active local database.
#'
#' Calling `read_div()` without a year argument reads all available
#' diversion-only years into memory.
#'
#' It errors if any requested year does not have an available diversion-only
#' Parquet file.
#'
#' Diversion-only records begin in October 2008.
#'
#' For larger workflows, [open_div()] may be more appropriate because it opens
#' the same files lazily as an Arrow dataset, allowing filtering or column
#' selection before materializing results in memory.
#'
#' @seealso [open_div()], [read_year_div()], [read_main()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
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
#' This function does not read all rows into memory immediately. The files are
#' opened from the active local database.
#'
#' Calling `open_main()` without a year argument opens all available main years.
#'
#' It is intended for workflows where you want to filter rows, select columns,
#' or otherwise work lazily before collecting results into memory. Use
#' [read_main()] when you want an in-memory data frame instead.
#'
#' @seealso [read_main()], [read_year_main()], [open_div()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' ds <- open_main(1987:1988)
#' ds
#' }
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
#' This function does not read all rows into memory immediately. The files are
#' opened from the active local database.
#'
#' Calling `open_div()` without a year argument opens all available
#' diversion-only years.
#'
#' It is intended for workflows where you want to filter rows, select columns,
#' or otherwise work lazily before collecting results into memory. Use
#' [read_div()] when you want an in-memory data frame instead.
#'
#' It errors if any requested year does not have an available diversion-only
#' Parquet file.
#'
#' Diversion-only records begin in October 2008.
#'
#' @seealso [read_div()], [read_year_div()], [open_main()],
#'   [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
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
#' Reads the airport coordinate table for the active local database.
#'
#' @returns
#' A data frame containing airport sequence identifiers and associated airport
#' metadata for airports referenced by the available flight data.
#'
#' @details
#' The coordinate table is derived from the BTS TranStats Master Coordinate
#' support table and is reduced to airports referenced by retained flight
#' records.
#'
#' Most analyses do not require manual joins to this table. During the database
#' build, `slcflights` writes main and diversion Parquet files already enriched
#' with latitude, longitude, and other airport metadata.
#'
#' The standalone coordinate table is mainly useful when you want to inspect
#' the reduced lookup table directly, check airport identifiers, or support
#' specialized analysis.
#'
#' @seealso [read_main()], [read_div()], [open_main()], [open_div()],
#'   [read_airlines()], [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' x <- read_coords()
#' head(x)
#' }
#'
#' @export
read_coords <- function() {
  readr::read_csv(.coords_path(), show_col_types = FALSE)
}

#' Read the Airline ID Lookup Table
#'
#' Reads the Airline ID lookup table for the active local database.
#'
#' @returns
#' A data frame containing DOT reporting airline identifiers, airline names,
#' and airline lookup codes for airlines referenced by the available flight
#' data.
#'
#' @details
#' The Airline ID lookup table is derived from the BTS TranStats
#' `DOT_ID_Reporting_Airline` lookup table and is reduced to airlines
#' referenced by retained flight records.
#'
#' Most analyses do not require manual joins to this table. During the database
#' build, `slcflights` writes main and diversion Parquet files already enriched
#' with airline names.
#'
#' The standalone Airline ID lookup table is mainly useful when you want to
#' inspect the reduced lookup table directly, check airline identifiers, or
#' support specialized analysis.
#'
#' @seealso [read_main()], [read_div()], [open_main()], [open_div()],
#'   [read_coords()], [read_field_dictionary()]
#'
#' @examples
#' \dontrun{
#' build_slcflights_db(until = "2024-12", confirm = TRUE)
#'
#' x <- read_airlines()
#' head(x)
#' }
#'
#' @export
read_airlines <- function() {
  readr::read_csv(.airlines_path(), show_col_types = FALSE)
}

#' Read the Field Dictionary
#'
#' Reads the field dictionary installed with the package.
#'
#' @returns
#' A data frame describing retained fields in the flight-record Parquet files
#' and the airport coordinate table.
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
#' Unlike the flight-record readers, the field dictionary is available without
#' building a local database.
#'
#' @seealso [read_main()], [read_div()], [open_main()], [open_div()],
#'   [read_coords()], [read_airlines()]
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

# Internal reader path helpers -----------------------------------------------

#' Resolve reader Parquet paths
#'
#' Resolves Parquet paths for the exported flight-data readers.
#'
#' @param type Flight-data grouping: `"main"` or `"div"`.
#' @param years Optional integer vector of years.
#'
#' @returns
#' Character vector of Parquet file paths.
#'
#' @noRd
.parquet_paths <- function(type = c("main", "div"), years = NULL) {
  type <- match.arg(type)

  if (is.null(years)) {
    years <- slc_available_data_years(type)
  } else {
    years <- normalize_data_years(years)
  }

  avail <- slc_available_data_years(type)
  missing <- setdiff(years, avail)

  if (length(missing)) {
    label <- switch(type,
      main = "main",
      div = "diversion"
    )

    stop(
      sprintf(
        "No %s parquet file found for year %s",
        label,
        missing[[1]]
      ),
      call. = FALSE
    )
  }

  paths <- slc_data_paths(type, years = years)
  paths <- paths[match(years, paths$year), , drop = FALSE]

  paths$path
}

#' Resolve reader coordinate CSV path
#'
#' Resolves the coordinate CSV path for [read_coords()].
#'
#' @returns
#' Character path to the active coordinate CSV.
#'
#' @noRd
.coords_path <- function() {
  slc_coords_path()
}

#' Resolve reader Airline ID lookup CSV path
#'
#' Resolves the Airline ID lookup CSV path for [read_airlines()].
#'
#' @returns
#' Character path to the active Airline ID lookup CSV.
#'
#' @noRd
.airlines_path <- function() {
  slc_airlines_path()
}
