
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slcflights

<!-- badges: start -->

<!-- badges: end -->

`slcflights` provides Salt Lake City-focused subsets of the Bureau of
Transportation Statistics (BTS) TranStats **On-Time: Reporting Carrier
On-Time Performance** data.

The package is designed for analyses where Salt Lake City International
Airport is central to the question. It includes flights where Salt Lake
City appears as the scheduled origin or destination, plus a separate
diversion-only data grouping for flights where Salt Lake City appears
only as a diversion airport.

The installed package data cover **October 1, 1987 through June 30,
2024**. Because the endpoint years are partial, the cleanest full-year
span in the installed data is **1988 through 2023**.

## Installation and loading

You can install the development version of `slcflights` from GitHub:

``` r
# install.packages("pak")
pak::pak("AlKulAK/slcflights")
```

Then load it:

``` r
library(slcflights)
```

## Data sources and coverage

The flight records come from BTS TranStats **On-Time: Reporting Carrier
On-Time Performance** data.

The installed historical files are derived from the 1987–2024 Parquet
files released for the 2025 ASA Data Expo Challenge. Those files contain
national flight records. The `slcflights` build workflow filters them to
Salt Lake City-related records, removes globally empty columns, splits
main records from diversion-only records, and enriches airport sequence
identifiers with coordinate metadata.

For months after June 2024, `update_slcflights_data()` can download
monthly BTS files directly from TranStats and build a compatible local
user cache. The installed package files are never modified.

## What the package includes

`slcflights` includes two flight-data groupings:

- **main records**: flights where Salt Lake City’s BTS airport ID
  appears in `OriginAirportID` or `DestAirportID`
- **diversion-only records**: flights where Salt Lake City’s BTS airport
  ID appears in one of `Div1AirportID` through `Div5AirportID`, but not
  in `OriginAirportID` or `DestAirportID`

The package also includes:

- an airport coordinate table derived from the BTS TranStats Master
  Coordinate support table
- a field dictionary describing selected flight-record and
  coordinate-table columns
- cache-aware readers that combine installed data with compatible local
  cached data when a cache is active

## Main function families

### Discover available data

Use `available_years()` to list years available for main records or
diversion-only records.

``` r
available_years("main")
#>  [1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
#> [16] 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
#> [31] 2017 2018 2019 2020 2021 2022 2023 2024
available_years("div")
#>  [1] 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#> [16] 2023 2024
```

The endpoint years in the installed data are partial. Use 1988 through
2023 for analyses that require complete calendar years and do not use a
local cache.

### Work lazily with Parquet files

Use `open_main()` and `open_div()` when you want an Arrow dataset
instead of an in-memory data frame.

``` r
ds <- open_main(1988:1989)
ds
#> FileSystemDataset with 2 Parquet files
#> 121 columns
#> Year: int64
#> Quarter: int64
#> Month: int64
#> DayofMonth: int64
#> DayOfWeek: int64
#> FlightDate: date32[day]
#> Reporting_Airline: string
#> DOT_ID_Reporting_Airline: int64
#> IATA_CODE_Reporting_Airline: string
#> Tail_Number: string
#> Flight_Number_Reporting_Airline: int64
#> OriginAirportID: int64
#> OriginAirportSeqID: int64
#> OriginLatitude: double
#> OriginLongitude: double
#> OriginAirportStartDate: date32[day]
#> OriginAirportThruDate: date32[day]
#> OriginAirportIsClosed: int32
#> OriginAirportIsLatest: int32
#> OriginCityMarketID: int64
#> ...
#> 101 more columns
#> Use `schema()` to see entire schema
```

This is usually the best entry point for larger workflows where you want
to filter rows or select columns before collecting data into memory.

### Read data into memory

Use `read_year_main()` and `read_year_div()` for a single year.

``` r
x <- read_year_main(1988)
head(x)[1:10]
#>   Year Quarter Month DayofMonth DayOfWeek FlightDate Reporting_Airline
#> 1 1988       1     1          1         5 1988-01-01                HP
#> 2 1988       1     1          1         5 1988-01-01                DL
#> 3 1988       1     1          1         5 1988-01-01                HP
#> 4 1988       1     1          1         5 1988-01-01                DL
#> 5 1988       1     1          1         5 1988-01-01                DL
#> 6 1988       1     1          1         5 1988-01-01                DL
#>   DOT_ID_Reporting_Airline IATA_CODE_Reporting_Airline Tail_Number
#> 1                    19991                          HP        <NA>
#> 2                    19790                          DL        <NA>
#> 3                    19991                          HP        <NA>
#> 4                    19790                          DL        <NA>
#> 5                    19790                          DL        <NA>
#> 6                    19790                          DL        <NA>
```

Use `read_main()` and `read_div()` for one, many, or all available
years.

``` r
x <- read_main(1988:1989)
dim(x)
#> [1] 272755    121
```

Diversion-only records are packaged separately because they answer a
different question: cases where Salt Lake City matters because of a
diversion event, not because it was the scheduled origin or destination.

``` r
div <- read_year_div(2015)
head(div)[1:10]
#>   Year Quarter Month DayofMonth DayOfWeek FlightDate Reporting_Airline
#> 1 2015       1     1          4         7 2015-01-04                OO
#> 2 2015       1     1          5         1 2015-01-05                B6
#> 3 2015       1     1          5         1 2015-01-05                B6
#> 4 2015       1     1          5         1 2015-01-05                B6
#> 5 2015       1     1          6         2 2015-01-06                DL
#> 6 2015       1     1          6         2 2015-01-06                AA
#>   DOT_ID_Reporting_Airline IATA_CODE_Reporting_Airline Tail_Number
#> 1                    20304                          OO      N613SK
#> 2                    20409                          B6      N534JB
#> 3                    20409                          B6      N579JB
#> 4                    20409                          B6      N627JB
#> 5                    19790                          DL      N3749D
#> 6                    19805                          AA      N3DDAA
```

### Read metadata tables

Use `read_coords()` when you want the standalone airport coordinate
table.

``` r
coords <- read_coords()
head(coords)
#> # A tibble: 6 × 32
#>   AIRPORT_SEQ_ID AIRPORT_ID AIRPORT DISPLAY_AIRPORT_NAME  DISPLAY_AIRPORT_CITY…¹
#>            <dbl>      <dbl> <chr>   <chr>                 <chr>                 
#> 1        1013505      10135 ABE     Lehigh Valley Intern… Allentown/Bethlehem/E…
#> 2        1013506      10135 ABE     Lehigh Valley Intern… Allentown/Bethlehem/E…
#> 3        1013602      10136 ABI     Abilene Regional      Abilene, TX           
#> 4        1013603      10136 ABI     Abilene Regional      Abilene, TX           
#> 5        1014001      10140 ABQ     Albuquerque Internat… Albuquerque, NM       
#> 6        1014002      10140 ABQ     Albuquerque Internat… Albuquerque, NM       
#> # ℹ abbreviated name: ¹​DISPLAY_AIRPORT_CITY_NAME_FULL
#> # ℹ 27 more variables: AIRPORT_WAC_SEQ_ID2 <dbl>, AIRPORT_WAC <dbl>,
#> #   AIRPORT_COUNTRY_NAME <chr>, AIRPORT_COUNTRY_CODE_ISO <chr>,
#> #   AIRPORT_STATE_NAME <chr>, AIRPORT_STATE_CODE <chr>,
#> #   AIRPORT_STATE_FIPS <chr>, CITY_MARKET_SEQ_ID <dbl>, CITY_MARKET_ID <dbl>,
#> #   DISPLAY_CITY_MARKET_NAME_FULL <chr>, CITY_MARKET_WAC_SEQ_ID2 <dbl>,
#> #   CITY_MARKET_WAC <dbl>, LAT_DEGREES <dbl>, LAT_HEMISPHERE <chr>, …
```

Use `read_field_dictionary()` when you want column descriptions,
source-table information, and field-presence metadata for main records,
diversion-only records, and the coordinate table.

``` r
fields <- read_field_dictionary()
head(fields)
#> # A tibble: 6 × 8
#>   field      group source description main_presence div_presence coords_presence
#>   <chr>      <chr> <chr>  <chr>       <chr>         <chr>        <chr>          
#> 1 ActualEla… elap… BTS O… Elapsed Ti… always        always       never          
#> 2 AIRPORT    coor… BTS M… A three ch… never         never        always         
#> 3 AIRPORT_C… coor… BTS M… Two-charac… never         never        always         
#> 4 AIRPORT_C… coor… BTS M… Country Na… never         never        always         
#> 5 AIRPORT_ID coor… BTS M… An identif… never         never        always         
#> 6 AIRPORT_I… coor… BTS M… Indicates … never         never        always         
#> # ℹ 1 more variable: notes <chr>
```

The flight Parquet files are already enriched with latitude, longitude,
and other date-bounded airport metadata. In most workflows, you do not
need a separate coordinate join.

## Updating local data

The installed package data end on **June 30, 2024**. To extend the
available data with newer BTS monthly releases, build a local cache:

``` r
update_slcflights_data()
```

By default, `update_slcflights_data()` checks BTS for the latest
available monthly data and downloads every available month after June
2024. You can also request an explicit endpoint:

``` r
# One additional month of data
update_slcflights_data(until = "2024-07")

# Six additional months of data
update_slcflights_data(until = c(2024, 12))
```

Local updates always begin with July 2024 and form a consecutive
extension of the installed data. The cache is stored in the user cache
directory returned by `tools::R_user_dir("slcflights", "cache")`.

Inspect the active cache with:

``` r
slcflights_cache_info()
```

Remove the local cache with:

``` r
clear_slcflights_cache()
```

After a compatible cache is active, the ordinary reader functions use it
automatically. For example, after updating through July 2024,
`read_year_main(2024)` reads the installed January-through-June records
and the cached July records as one combined data frame.

## Rebuilding the packaged data

Most users should **not** rebuild the packaged data. They should use the
installed files and, when needed, `update_slcflights_data()` for
post-June-2024 monthly updates.

The maintainer build workflow lives in `data-raw/build_data.R`. From a
source checkout, maintainers can rebuild all bundled data artifacts
with:

``` r
source("data-raw/build_data.R")
build_slc_data()
```

By default, `build_slc_data()` rebuilds years 1987 through 2024, filters
the national source data to Salt Lake City-related records, splits main
and diversion-only files, reduces the BTS Master Coordinate table,
enriches the Parquet files with airport metadata, rebuilds the field
dictionary, and writes final package data under `inst/extdata/`.

No source data files need to be present before running this workflow.
The historical Parquet source files are downloaded into a temporary
build directory, and the BTS Master Coordinate table is downloaded
automatically if `data-raw/T_MASTER_CORD.csv` is absent.

The temporary build directory is a maintainer-side artifact. It is not
part of the installed package and is separate from the local user cache
created by `update_slcflights_data()`.

## Learn more

Consult the package’s vignette for a fuller walkthrough of the package
workflow, including coverage, lazy inspection, in-memory reads, airport
metadata, diversion-only records, and local cache updates. You access it
like so:

``` r
vignette("slcflights", package = "slcflights")
```
