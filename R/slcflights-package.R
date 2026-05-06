#' slcflights: Salt Lake City-Related Flight Data
#'
#' `slcflights` provides Salt Lake City-focused subsets of the Bureau of
#' Transportation Statistics (BTS) TranStats On-Time: Reporting Carrier
#' On-Time Performance data, along with airport coordinate metadata used to
#' support those records.
#'
#' The installed historical Parquet files cover October 1987 through June 2024
#' and are derived from the 1987--2024 Parquet files distributed for the 2025
#' ASA Data Expo Challenge. The underlying records originate from the BTS
#' TranStats On-Time Performance data, and the coordinate table is derived from
#' the BTS TranStats Master Coordinate support table.
#'
#' The package contains two flight-data groupings:
#'
#' - **main records**, where Salt Lake City's BTS airport ID appears in
#'   `OriginAirportID` or `DestAirportID`
#' - **diversion-only records**, where Salt Lake City's BTS airport ID appears
#'   in one of `Div1AirportID` through `Div5AirportID`, but not in
#'   `OriginAirportID` or `DestAirportID`
#'
#' The main user-facing functions are
#'
#' - [available_years()] to list currently available years for main or
#'   diversion-only records, including compatible local cache years when present
#' - [read_main()] to read one, many, or all years of main records into memory
#' - [read_div()] to read one, many, or all years of diversion-only records
#'   into memory
#' - [open_main()] to open one, many, or all years of main records lazily as an
#'   Arrow dataset
#' - [open_div()] to open one, many, or all years of diversion-only records
#'   lazily as an Arrow dataset
#' - [read_year_main()] to read one year's main records into memory
#' - [read_year_div()] to read one year's diversion-only records into memory
#' - [read_coords()] to read the currently active airport coordinate table
#' - [update_slcflights_data()] to extend the installed data with newer BTS
#'   monthly releases in a local user cache
#' - [slcflights_cache_info()] to inspect the local cache
#' - [clear_slcflights_cache()] to remove the local cache
#'
#' @section Fields:
#' Flight-record columns use BTS TranStats field names where those fields are
#' present in the package data. Not every BTS field is guaranteed to appear in
#' every package Parquet file. The data-build process removes columns that are
#' globally all missing before and after filtering to Salt Lake City-related
#' records. Use `names(read_year_main(year))`, `names(read_year_div(year))`,
#' `names(open_main(year))`, or `names(open_div(year))` to inspect the fields
#' available for a particular year and record type.
#'
#' The currently active coordinate table contains airport sequence identifiers
#' and associated airport metadata used to enrich flight records with latitude,
#' longitude, and date-bounded airport information. If a compatible local cache
#' is active, [read_coords()] reads the cached coordinate table; otherwise it
#' reads the installed package coordinate table.
#'
#' @keywords internal
"_PACKAGE"
