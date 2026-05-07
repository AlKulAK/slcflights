
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
head(x)
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
#>   Flight_Number_Reporting_Airline OriginAirportID OriginAirportSeqID
#> 1                             315           14869            1486901
#> 2                             625           14869            1486901
#> 3                             182           12889            1288901
#> 4                             448           14057            1405701
#> 5                            1550           12892            1289201
#> 6                            1168           14747            1474702
#>   OriginLatitude OriginLongitude OriginAirportStartDate OriginAirportThruDate
#> 1       40.78417       -111.9669             1950-01-01            1994-08-31
#> 2       40.78417       -111.9669             1950-01-01            1994-08-31
#> 3       36.08000       -115.1522             1950-01-01            1989-12-31
#> 4       45.58917       -122.5950             1950-01-01            2011-06-30
#> 5       33.94250       -118.4081             1950-01-01            2011-06-30
#> 6       47.44917       -122.3081             1983-12-01            2011-06-30
#>   OriginAirportIsClosed OriginAirportIsLatest OriginCityMarketID Origin
#> 1                     0                     0              34614    SLC
#> 2                     0                     0              34614    SLC
#> 3                     0                     0              32211    LAS
#> 4                     0                     0              34057    PDX
#> 5                     0                     0              32575    LAX
#> 6                     0                     0              30559    SEA
#>       OriginCityName OriginState OriginStateFips OriginStateName OriginWac
#> 1 Salt Lake City, UT          UT              49            Utah        87
#> 2 Salt Lake City, UT          UT              49            Utah        87
#> 3      Las Vegas, NV          NV              32          Nevada        85
#> 4       Portland, OR          OR              41          Oregon        92
#> 5    Los Angeles, CA          CA              06      California        91
#> 6        Seattle, WA          WA              53      Washington        93
#>   DestAirportID DestAirportSeqID DestLatitude DestLongitude
#> 1         12889          1288901     36.08000     -115.1522
#> 2         14747          1474702     47.44917     -122.3081
#> 3         14869          1486901     40.78417     -111.9669
#> 4         14869          1486901     40.78417     -111.9669
#> 5         14869          1486901     40.78417     -111.9669
#> 6         14869          1486901     40.78417     -111.9669
#>   DestAirportStartDate DestAirportThruDate DestAirportIsClosed
#> 1           1950-01-01          1989-12-31                   0
#> 2           1983-12-01          2011-06-30                   0
#> 3           1950-01-01          1994-08-31                   0
#> 4           1950-01-01          1994-08-31                   0
#> 5           1950-01-01          1994-08-31                   0
#> 6           1950-01-01          1994-08-31                   0
#>   DestAirportIsLatest DestCityMarketID Dest       DestCityName DestState
#> 1                   0            32211  LAS      Las Vegas, NV        NV
#> 2                   0            30559  SEA        Seattle, WA        WA
#> 3                   0            34614  SLC Salt Lake City, UT        UT
#> 4                   0            34614  SLC Salt Lake City, UT        UT
#> 5                   0            34614  SLC Salt Lake City, UT        UT
#> 6                   0            34614  SLC Salt Lake City, UT        UT
#>   DestStateFips DestStateName DestWac CRSDepTime DepTime DepDelay
#> 1            32        Nevada      85       0045    0045        0
#> 2            53    Washington      93       0055    0055        0
#> 3            49          Utah      87       0140    0143        3
#> 4            49          Utah      87       0550    0550        0
#> 5            49          Utah      87       0600    0606        6
#> 6            49          Utah      87       0600    0600        0
#>   DepDelayMinutes DepDel15 DepartureDelayGroups DepTimeBlk TaxiOut WheelsOff
#> 1               0        0                    0  0001-0559    <NA>      <NA>
#> 2               0        0                    0  0001-0559    <NA>      <NA>
#> 3               3        0                    0  0001-0559    <NA>      <NA>
#> 4               0        0                    0  0001-0559    <NA>      <NA>
#> 5               6        0                    0  0600-0659    <NA>      <NA>
#> 6               0        0                    0  0600-0659    <NA>      <NA>
#>   WheelsOn TaxiIn CRSArrTime ArrTime ArrDelay ArrDelayMinutes ArrDel15
#> 1     <NA>   <NA>       0055    0057        2               2        0
#> 2     <NA>   <NA>       0149    0137      -12               0        0
#> 3     <NA>   <NA>       0350    0353        3               3        0
#> 4     <NA>   <NA>       0827    0825       -2               0        0
#> 5     <NA>   <NA>       0838    0843        5               5        0
#> 6     <NA>   <NA>       0840    0853       13              13        0
#>   ArrivalDelayGroups ArrTimeBlk Cancelled CancellationCode Diverted
#> 1                  0  0001-0559         0             <NA>        0
#> 2                 -1  0001-0559         0             <NA>        0
#> 3                  0  0001-0559         0             <NA>        0
#> 4                 -1  0800-0859         0             <NA>        0
#> 5                  0  0800-0859         0             <NA>        0
#> 6                  0  0800-0859         0             <NA>        0
#>   CRSElapsedTime ActualElapsedTime AirTime Flights Distance DistanceGroup
#> 1             70                72    <NA>       1      368             2
#> 2            114               102    <NA>       1      689             3
#> 3             70                70    <NA>       1      368             2
#> 4             97                95    <NA>       1      630             3
#> 5             98                97    <NA>       1      590             3
#> 6            100               113    <NA>       1      689             3
#>   CarrierDelay WeatherDelay NASDelay SecurityDelay LateAircraftDelay
#> 1         <NA>         <NA>     <NA>          <NA>              <NA>
#> 2         <NA>         <NA>     <NA>          <NA>              <NA>
#> 3         <NA>         <NA>     <NA>          <NA>              <NA>
#> 4         <NA>         <NA>     <NA>          <NA>              <NA>
#> 5         <NA>         <NA>     <NA>          <NA>              <NA>
#> 6         <NA>         <NA>     <NA>          <NA>              <NA>
#>   FirstDepTime TotalAddGTime LongestAddGTime DivAirportLandings DivReachedDest
#> 1         <NA>          <NA>            <NA>               <NA>           <NA>
#> 2         <NA>          <NA>            <NA>               <NA>           <NA>
#> 3         <NA>          <NA>            <NA>               <NA>           <NA>
#> 4         <NA>          <NA>            <NA>               <NA>           <NA>
#> 5         <NA>          <NA>            <NA>               <NA>           <NA>
#> 6         <NA>          <NA>            <NA>               <NA>           <NA>
#>   DivActualElapsedTime DivArrDelay DivDistance Div1Airport Div1AirportID
#> 1                 <NA>        <NA>        <NA>        <NA>          <NA>
#> 2                 <NA>        <NA>        <NA>        <NA>          <NA>
#> 3                 <NA>        <NA>        <NA>        <NA>          <NA>
#> 4                 <NA>        <NA>        <NA>        <NA>          <NA>
#> 5                 <NA>        <NA>        <NA>        <NA>          <NA>
#> 6                 <NA>        <NA>        <NA>        <NA>          <NA>
#>   Div1AirportSeqID Div1Latitude Div1Longitude Div1AirportStartDate
#> 1             <NA>           NA            NA                 <NA>
#> 2             <NA>           NA            NA                 <NA>
#> 3             <NA>           NA            NA                 <NA>
#> 4             <NA>           NA            NA                 <NA>
#> 5             <NA>           NA            NA                 <NA>
#> 6             <NA>           NA            NA                 <NA>
#>   Div1AirportThruDate Div1AirportIsClosed Div1AirportIsLatest Div1WheelsOn
#> 1                <NA>                  NA                  NA         <NA>
#> 2                <NA>                  NA                  NA         <NA>
#> 3                <NA>                  NA                  NA         <NA>
#> 4                <NA>                  NA                  NA         <NA>
#> 5                <NA>                  NA                  NA         <NA>
#> 6                <NA>                  NA                  NA         <NA>
#>   Div1TotalGTime Div1LongestGTime Div1WheelsOff Div1TailNum Div2Airport
#> 1           <NA>             <NA>          <NA>        <NA>        <NA>
#> 2           <NA>             <NA>          <NA>        <NA>        <NA>
#> 3           <NA>             <NA>          <NA>        <NA>        <NA>
#> 4           <NA>             <NA>          <NA>        <NA>        <NA>
#> 5           <NA>             <NA>          <NA>        <NA>        <NA>
#> 6           <NA>             <NA>          <NA>        <NA>        <NA>
#>   Div2AirportID Div2AirportSeqID Div2Latitude Div2Longitude
#> 1          <NA>             <NA>           NA            NA
#> 2          <NA>             <NA>           NA            NA
#> 3          <NA>             <NA>           NA            NA
#> 4          <NA>             <NA>           NA            NA
#> 5          <NA>             <NA>           NA            NA
#> 6          <NA>             <NA>           NA            NA
#>   Div2AirportStartDate Div2AirportThruDate Div2AirportIsClosed
#> 1                 <NA>                <NA>                  NA
#> 2                 <NA>                <NA>                  NA
#> 3                 <NA>                <NA>                  NA
#> 4                 <NA>                <NA>                  NA
#> 5                 <NA>                <NA>                  NA
#> 6                 <NA>                <NA>                  NA
#>   Div2AirportIsLatest Div2WheelsOn Div2TotalGTime Div2LongestGTime
#> 1                  NA         <NA>           <NA>             <NA>
#> 2                  NA         <NA>           <NA>             <NA>
#> 3                  NA         <NA>           <NA>             <NA>
#> 4                  NA         <NA>           <NA>             <NA>
#> 5                  NA         <NA>           <NA>             <NA>
#> 6                  NA         <NA>           <NA>             <NA>
#>   Div2WheelsOff Div2TailNum Div3Airport Div3AirportID Div3AirportSeqID
#> 1          <NA>        <NA>        <NA>          <NA>             <NA>
#> 2          <NA>        <NA>        <NA>          <NA>             <NA>
#> 3          <NA>        <NA>        <NA>          <NA>             <NA>
#> 4          <NA>        <NA>        <NA>          <NA>             <NA>
#> 5          <NA>        <NA>        <NA>          <NA>             <NA>
#> 6          <NA>        <NA>        <NA>          <NA>             <NA>
#>   Div3Latitude Div3Longitude Div3AirportStartDate Div3AirportThruDate
#> 1           NA            NA                 <NA>                <NA>
#> 2           NA            NA                 <NA>                <NA>
#> 3           NA            NA                 <NA>                <NA>
#> 4           NA            NA                 <NA>                <NA>
#> 5           NA            NA                 <NA>                <NA>
#> 6           NA            NA                 <NA>                <NA>
#>   Div3AirportIsClosed Div3AirportIsLatest Div3WheelsOn Div3TotalGTime
#> 1                  NA                  NA         <NA>           <NA>
#> 2                  NA                  NA         <NA>           <NA>
#> 3                  NA                  NA         <NA>           <NA>
#> 4                  NA                  NA         <NA>           <NA>
#> 5                  NA                  NA         <NA>           <NA>
#> 6                  NA                  NA         <NA>           <NA>
#>   Div3LongestGTime
#> 1             <NA>
#> 2             <NA>
#> 3             <NA>
#> 4             <NA>
#> 5             <NA>
#> 6             <NA>
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
head(div)
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

See `vignette("slcflights")` for a fuller walkthrough of the package
workflow, including coverage, lazy inspection, in-memory reads, airport
metadata, diversion-only records, and local cache updates.
