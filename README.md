
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slcflights

<!-- badges: start -->

<!-- badges: end -->

The goal of slcflights is to provide packaged Parquet files for Salt
Lake City-related U.S. flight records, along with a CSV table of airport
coordinates used to support those records.

The packaged flight data are derived from the U.S. commercial flight
data used in the 2025 ASA Data Expo Challenge. The underlying flight
records originate from the Bureau of Transportation Statistics (BTS)
TranStats On-Time Performance data, and the packaged coordinate table is
derived from the BTS TranStats Master Coordinate support table.

The package includes two flight-data groupings:

- **main records**: flights where Salt Lake City appears in the core
  origin or destination airport fields
- **diversion-only records**: flights where Salt Lake City appears only
  in diversion airport fields and not in the core origin or destination
  airport fields

## Installation

You can install the development version of slcflights like so:

``` r
# install.packages("remotes")
remotes::install_github("AlKulAK/slcflights")
```

## Package contents

The main user-facing functions are

- `available_years()` to list packaged years for main or diversion-only
  records
- `read_year_main()` to read one year’s main flight records into memory
- `read_year_div()` to read one year’s diversion-only flight records
  into memory
- `read_main()` to read one, many, or all years of main records into
  memory
- `read_div()` to read one, many, or all years of diversion-only records
  into memory
- `open_main()` to open one, many, or all years of main records lazily
  as an Arrow dataset
- `open_div()` to open one, many, or all years of diversion-only records
  lazily as an Arrow dataset
- `read_coords()` to read the packaged airport coordinate table

## Examples

Load the package:

``` r
library(slcflights)
```

List the packaged years available for main records:

``` r
available_years("main")
#>  [1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
#> [16] 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
#> [31] 2017 2018 2019 2020 2021 2022 2023 2024
```

Read the airport coordinate table:

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

Read one year’s main flight records into memory:

``` r
x_1987 <- read_year_main(1987)
head(x_1987)
#> # A tibble: 6 × 121
#>    Year Quarter Month DayofMonth DayOfWeek FlightDate Reporting_Airline
#>   <int>   <int> <int>      <int>     <int> <date>     <chr>            
#> 1  1987       4    10          1         4 1987-10-01 HP               
#> 2  1987       4    10          1         4 1987-10-01 DL               
#> 3  1987       4    10          1         4 1987-10-01 DL               
#> 4  1987       4    10          1         4 1987-10-01 CO               
#> 5  1987       4    10          1         4 1987-10-01 DL               
#> 6  1987       4    10          1         4 1987-10-01 UA               
#> # ℹ 114 more variables: DOT_ID_Reporting_Airline <int>,
#> #   IATA_CODE_Reporting_Airline <chr>, Tail_Number <chr>,
#> #   Flight_Number_Reporting_Airline <int>, OriginAirportID <int>,
#> #   OriginAirportSeqID <int>, OriginLatitude <dbl>, OriginLongitude <dbl>,
#> #   OriginAirportStartDate <date>, OriginAirportThruDate <date>,
#> #   OriginAirportIsClosed <int>, OriginAirportIsLatest <int>,
#> #   OriginCityMarketID <int>, Origin <chr>, OriginCityName <chr>, …
```

Read multiple years of main records into memory:

``` r
x_main <- read_main(1987:1988)
dim(x_main)
#> [1] 168449    121
```

Open main records lazily with Arrow when you want to defer reading until
later:

``` r
ds_main <- open_main(1987:1988)
ds_main
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

Read diversion-only records for one year:

``` r
x_div <- read_year_div(2015)
head(x_div)
```

## Rebuilding the packaged data

The packaged Parquet files and coordinate CSV can be rebuilt from source
data.

The rebuild workflow is implemented in `data-raw/build_data.R`. To
rebuild, run from a source checkout of the package:

``` r
source("data-raw/build_data.R")
build_slc_data()
```

By default, `build_slc_data()`

- rebuilds data for years 1987 through 2024
- uses Salt Lake City’s BTS airport ID (`14869`)
- downloads source Parquet files into `data-raw/cache/`
- writes the reduced coordinate CSV into the build cache
- copies final packaged outputs into `inst/extdata/`
- deletes the build cache when finished

Note that you can rebuild a subset of years, for example:

``` r
source("data-raw/build_data.R")
build_slc_data(years = 2019:2024)
```

Because rebuilding requires large downloads and substantial disk space,
it is recommended to work with the packaged files directly.

## Data provenance

This package repackages a Salt Lake City-focused subset of the flight
data used in the 2025 ASA Data Expo Challenge. The challenge centered on
analysis and visualization of U.S. commercial flight arrival and
departure records, while the underlying data ultimately come from BTS
TranStats On-Time Performance data.

The packaged coordinate table is derived from the BTS TranStats Master
Coordinate support table and contains the airport sequence identifiers
needed to enrich the flight records with airport latitude, longitude,
and date-bounded airport metadata.
