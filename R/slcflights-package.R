#' slcflights: Salt Lake City-Related Flight Data
#'
#' `slcflights` provides tools for building and reading Salt Lake City-focused
#' subsets of the Bureau of Transportation Statistics (BTS) TranStats On-Time:
#' Reporting Carrier On-Time Performance data, along with airport coordinate
#' and Airline ID metadata for those records.
#'
#' The package builds a local BTS-sourced database from monthly BTS source
#' files. The local database begins with October 1987 and extends through a
#' user-selected endpoint. Use [build_slcflights_db()] or
#' [update_slcflights_data()] to build or extend the database before reading
#' flight records.
#'
#' The package contains two flight-data groupings:
#'
#' - **main records**, where Salt Lake City's BTS airport ID appears in
#'   `OriginAirportID` or `DestAirportID`
#' - **diversion-only records**, where Salt Lake City's BTS airport ID appears
#'   in one of `Div1AirportID` through `Div5AirportID`, but not in
#'   `OriginAirportID` or `DestAirportID`
#'
#' The main user-facing functions are:
#'
#' - [build_slcflights_db()] to build or extend the local BTS-sourced database
#' - [update_slcflights_data()] to build or extend the local BTS-sourced
#'   database
#' - [available_years()] to list years available in the active local database
#'   for main or diversion-only records
#' - [read_main()] to read one, many, or all years of main records into memory
#' - [read_div()] to read one, many, or all years of diversion-only records
#'   into memory
#' - [open_main()] to open one, many, or all years of main records lazily as an
#'   Arrow dataset
#' - [open_div()] to open one, many, or all years of diversion-only records
#'   lazily as an Arrow dataset
#' - [read_year_main()] to read one year's main records into memory
#' - [read_year_div()] to read one year's diversion-only records into memory
#' - [read_coords()] to read the airport coordinate table from the active local
#'   database
#' - [read_airlines()] to read the Airline ID lookup table from the active
#'   local database
#' - [read_field_dictionary()] to read field descriptions and field-presence
#'   metadata
#' - [slcflights_cache_info()] to inspect the active local database
#' - [clear_slcflights_cache()] to remove the active local database
#'
#' @section Fields:
#' Flight-record columns use BTS TranStats field names from the monthly source
#' files. Some BTS fields can be entirely missing within a month, a year, or a
#' record grouping after filtering to Salt Lake City-related records. Those
#' fields are retained as all-null columns when they are valid BTS fields. Use
#' `names(read_year_main(year))`, `names(read_year_div(year))`,
#' `names(open_main(year))`, or `names(open_div(year))` to inspect the fields
#' available for a particular year and record type.
#'
#' The airport coordinate table contains airport sequence identifiers and
#' associated airport metadata used to enrich flight records with latitude,
#' longitude, and date-bounded airport information.
#'
#' The Airline ID lookup table contains DOT reporting airline identifiers,
#' airline names, and airline lookup codes for airlines referenced by the active
#' local database.
#'
#' @keywords internal
"_PACKAGE"
