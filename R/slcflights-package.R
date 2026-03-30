#' slcflights: Salt Lake City-Related Flight Data
#'
#' `slcflights` provides packaged Parquet files for Salt Lake City-related U.S.
#' flight records, along with a packaged CSV table of airport coordinates used
#' to support those records.
#'
#' The packaged flight data are derived from the U.S. commercial flight data
#' used in the 2025 ASA Data Expo Challenge. The underlying flight records
#' ultimately originate from the Bureau of Transportation Statistics (BTS)
#' TranStats On-Time Performance data, and the packaged coordinate table is
#' derived from the BTS TranStats Master Coordinate support table.
#'
#' The package contains two flight-data groupings:
#'
#' - **main records**, where Salt Lake City appears in the primary origin or
#'   destination airport fields
#' - **diversion-only records**, where Salt Lake City appears only in diversion
#'   airport fields and not in the primary origin or destination airport fields
#'
#' The main user-facing functions are
#'
#' - [available_years()] to list packaged years for main or diversion-only
#'   records
#' - [read_main()] to read one, many, or all years of main records into memory
#' - [read_div()] to read one, many, or all years of diversion-only records
#'   into memory
#' - [open_main()] to open one, many, or all years of main records lazily as an
#'   Arrow dataset
#' - [open_div()] to open one, many, or all years of diversion-only records
#'   lazily as an Arrow dataset
#' - [read_year_main()] to read one year's main records into memory
#' - [read_year_div()] to read one year's diversion-only records into memory
#' - [read_coords()] to read the packaged airport coordinate table
#'
#' The packaged coordinate table contains airport sequence identifiers and
#' associated airport metadata used to enrich the packaged flight records with
#' latitude, longitude, and date-bounded airport information.
#'
#' @keywords internal
"_PACKAGE"
