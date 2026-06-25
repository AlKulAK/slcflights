
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slcflights

<!-- badges: start -->

[![R-CMD-check](https://github.com/AlKulAK/slcflights/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlKulAK/slcflights/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`slcflights` provides tools for building and reading Salt Lake
City-focused subsets of the Bureau of Transportation Statistics (BTS)
TranStats **On-Time: Reporting Carrier On-Time Performance** data.

The package is designed for analyses where Salt Lake City International
Airport is central to the question. It builds a local BTS-sourced
database that includes flights where Salt Lake City appears as the
scheduled origin or destination, plus a separate diversion-only data
grouping for flights where Salt Lake City appears only as a diversion
airport.

## Installation

You can install the development version of `slcflights` from GitHub:

``` r
# install.packages("pak")
pak::pak("AlKulAK/slcflights")
```

## Data sources and coverage

The flight records come from BTS TranStats **On-Time: Reporting Carrier
On-Time Performance** monthly source files.

A local `slcflights` database begins with October 1987 and extends
through a user-selected endpoint. The database is built from monthly BTS
files, filtered to Salt Lake City-related records, split into main and
diversion-only records, enriched with airport coordinate metadata, and
enriched with Airline ID metadata.

Airport coordinate metadata are derived from the BTS TranStats Master
Coordinate support table. Airline ID metadata are derived from the BTS
TranStats `DOT_ID_Reporting_Airline` lookup table.

## What the database contains

`slcflights` contains two flight-data groupings:

- **main records**: flights where Salt Lake City’s BTS airport ID
  appears in `OriginAirportID` or `DestAirportID`
- **diversion-only records**: flights where Salt Lake City’s BTS airport
  ID appears in one of `Div1AirportID` through `Div5AirportID`, but not
  in `OriginAirportID` or `DestAirportID`

The package also provides:

- an airport coordinate table derived from the BTS TranStats Master
  Coordinate support table
- an Airline ID lookup table derived from the BTS TranStats
  `DOT_ID_Reporting_Airline` lookup table
- a field dictionary describing retained flight-record and
  coordinate-table columns

Flight-record columns use BTS TranStats field names from the monthly
source files. Some BTS fields can be entirely missing within a month, a
year, or a record grouping after filtering to Salt Lake City-related
records. Those fields are retained as all-null columns when they are
valid BTS fields.

## Build or update the local database

Build the local database before reading flight records.

Building or updating the local database can take substantial time and
disk space, even for relatively small date ranges. `slcflights`
downloads and processes monthly national BTS source files before writing
the local Salt Lake City-focused database. When a monthly BTS PREZIP
file is unavailable, the package automatically uses the TranStats
all-fields export for the same BTS table, which can be slower and can
require more temporary storage.

``` r
slcflights::build_slcflights_db(until = "2024-12", confirm = TRUE)
```

The local database always begins with October 1987. The `until` argument
controls the endpoint. It accepts `"latest"`, a four-digit year, a
string of the form `"YYYY-MM"`, or `c(year, month)`.

``` r
slcflights::build_slcflights_db(until = "latest", confirm = TRUE)
slcflights::build_slcflights_db(until = 2024, confirm = TRUE)
slcflights::build_slcflights_db(until = c(2024, 12), confirm = TRUE)
```

Use `update_slcflights_db()` to extend an existing local database.

``` r
slcflights::update_slcflights_db(
  until = "2024-12",
  confirm = TRUE
)
```

Inspect the active local database with:

``` r
slcflights::status_slcflights_db()
```

Remove the active local database with:

``` r
slcflights::delete_slcflights_db()
```

The database is stored under the user cache directory returned by
`tools::R_user_dir("slcflights", "cache")`. Building or updating the
database does not modify installed package files.

## Discover available data

Use `available_years()` to list years available in the active local
database. By default, `available_years()` returns years for main
records. Use `type = "div"` to inspect diversion-only years.

``` r
slcflights::available_years()
slcflights::available_years(type = "div")
```

## Work lazily with Parquet files

Use `open_main()` and `open_div()` when you want an Arrow dataset
instead of an in-memory data frame.

``` r
ds <- slcflights::open_main(1988:1989)
ds
```

This is usually the best entry point for larger workflows where you want
to filter rows or select columns before collecting data into memory.

## Read data into memory

Use `read_year_main()` and `read_year_div()` for a single year.

``` r
x <- slcflights::read_year_main(1988)
head(x)[1:10]
```

Use `read_main()` and `read_div()` for one, many, or all available
years. Calling either function without a year argument reads all
available years for that grouping into memory. For larger workflows,
prefer `open_main()` or `open_div()` and collect only the summarized
result.

``` r
x <- slcflights::read_main(1988:1989)
dim(x)
```

Diversion-only records are stored separately because they answer a
different question: cases where Salt Lake City matters because of a
diversion event, not because it was the scheduled origin or destination.

``` r
div <- slcflights::read_year_div(2015)
head(div)[1:10]
```

## Read metadata tables

Use `read_coords()` when you want the standalone airport coordinate
table.

``` r
coords <- slcflights::read_coords()
head(coords)
```

Use `read_airlines()` when you want the standalone Airline ID lookup
table.

``` r
airlines <- slcflights::read_airlines()
head(airlines)
```

Use `read_field_dictionary()` when you want column descriptions,
source-table information, and field-presence metadata for main records,
diversion-only records, and the coordinate table.

``` r
fields <- slcflights::read_field_dictionary()
head(fields)
```

The flight Parquet files are already enriched with latitude, longitude,
other airport metadata, and `Reporting_Airline_Name`. In most workflows,
you do not need separate coordinate or Airline ID joins.

## Rebuilding data

The standard workflow is to build the local database with
`build_slcflights_db()` or extend it with `update_slcflights_db()`.

The maintainer build scripts under `data-raw/` are source-package
development artifacts. They are not needed for normal package use.

## Learn more

Consult the package vignette for a fuller walkthrough of the package
workflow, including database builds, available-year inspection, lazy
reads, in-memory reads, metadata tables, diversion-only records, and
local database updates.

``` r
vignette("slcflights", package = "slcflights")
```
