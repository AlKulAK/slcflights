
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slcflights

<!-- badges: start -->

<!-- badges: end -->

The goal of `slcflights` is to provide Salt Lake City-focused subsets of
the Bureau of Transportation Statistics (BTS) TranStats **On-Time:
Reporting Carrier On-Time Performance** data, along with airport
coordinate metadata used to support those records.

The installed historical Parquet files cover **October 1, 1987 through
June 30, 2024** and are derived from the 1987–2024 Parquet files
distributed for the 2025 ASA Data Expo Challenge. The ASA challenge page
links to the BTS TranStats download page and also provides DuckDB-hosted
Parquet files for the national flight records.

The package includes two flight-data groupings:

- **main records**: flights where Salt Lake City’s BTS airport ID
  appears in `OriginAirportID` or `DestAirportID`
- **diversion-only records**: flights where Salt Lake City’s BTS airport
  ID appears in one of `Div1AirportID` through `Div5AirportID`, but not
  in `OriginAirportID` or `DestAirportID`

The installed package data cover **October 1, 1987 through June 30,
2024**. This means that 1987 and 2024 are partial years. For analyses
requiring complete calendar years using only installed data, the
cleanest full-year span is 1988 through 2023.

Users can optionally extend the data beyond June 2024 by building a
local cache with `update_slcflights_data()`. For cache updates, the
package downloads monthly BTS On-Time Performance files directly from
BTS TranStats. The installed package files are never modified.

## Installation

You can install the development version of slcflights like so:

``` r
# install.packages("remotes")
remotes::install_github("AlKulAK/slcflights")
```

## Package contents

The main user-facing functions are

- `available_years()` to list currently available years for main or
  diversion-only records, including compatible local cache years when
  present
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
- `read_coords()` to read the currently active airport coordinate table
- `update_slcflights_data()` to download newer BTS data into a local
  user cache
- `slcflights_cache_info()` to inspect the local user cache
- `clear_slcflights_cache()` to remove the local user cache

The flight-data readers are cache-aware. If no local cache is active,
they read the installed package data. If a local cache is active, they
read the installed data together with compatible cached data.
`read_coords()` is also cache-aware: it reads the cached coordinate
table when a compatible local cache is active and otherwise reads the
installed coordinate table.

## Examples

Load the package:

``` r
library(slcflights)
```

List the available years for main records:

``` r
available_years("main")
#>  [1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
#> [16] 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
#> [31] 2017 2018 2019 2020 2021 2022 2023 2024
```

Read the airport coordinate table:

``` r
coords <- read_coords()
dim(coords)
#> [1] 537  32
names(coords)[1:8]
#> [1] "AIRPORT_SEQ_ID"                 "AIRPORT_ID"                    
#> [3] "AIRPORT"                        "DISPLAY_AIRPORT_NAME"          
#> [5] "DISPLAY_AIRPORT_CITY_NAME_FULL" "AIRPORT_WAC_SEQ_ID2"           
#> [7] "AIRPORT_WAC"                    "AIRPORT_COUNTRY_NAME"
```

Read one year’s main flight records into memory:

``` r
x_1987 <- read_year_main(1987)
head(x_1987)
#>   Year Quarter Month DayofMonth DayOfWeek FlightDate Reporting_Airline
#> 1 1987       4    10          1         4 1987-10-01                HP
#> 2 1987       4    10          1         4 1987-10-01                DL
#> 3 1987       4    10          1         4 1987-10-01                DL
#> 4 1987       4    10          1         4 1987-10-01                CO
#> 5 1987       4    10          1         4 1987-10-01                DL
#> 6 1987       4    10          1         4 1987-10-01                UA
#>   DOT_ID_Reporting_Airline IATA_CODE_Reporting_Airline Tail_Number
#> 1                    19991                          HP        <NA>
#> 2                    19790                          DL        <NA>
#> 3                    19790                          DL        <NA>
#> 4                    19704                          CO        <NA>
#> 5                    19790                          DL        <NA>
#> 6                    19977                          UA        <NA>
#>   Flight_Number_Reporting_Airline OriginAirportID OriginAirportSeqID
#> 1                             336           12889            1288901
#> 2                            1493           10299            1029901
#> 3                             430           14869            1486901
#> 4                            1176           14869            1486901
#> 5                            1489           13930            1393001
#> 6                            1620           12892            1289201
#>   OriginLatitude OriginLongitude OriginAirportStartDate OriginAirportThruDate
#> 1       36.08000      -115.15222             1950-01-01            1989-12-31
#> 2       61.16917      -149.98528             1950-01-01            1999-12-31
#> 3       40.78417      -111.96694             1950-01-01            1994-08-31
#> 4       40.78417      -111.96694             1950-01-01            1994-08-31
#> 5       41.97806       -87.90611             1950-01-01            2011-06-30
#> 6       33.94250      -118.40806             1950-01-01            2011-06-30
#>   OriginAirportIsClosed OriginAirportIsLatest OriginCityMarketID Origin
#> 1                     0                     0              32211    LAS
#> 2                     0                     0              30299    ANC
#> 3                     0                     0              34614    SLC
#> 4                     0                     0              34614    SLC
#> 5                     0                     0              30977    ORD
#> 6                     0                     0              32575    LAX
#>       OriginCityName OriginState OriginStateFips OriginStateName OriginWac
#> 1      Las Vegas, NV          NV              32          Nevada        85
#> 2      Anchorage, AK          AK              02          Alaska         1
#> 3 Salt Lake City, UT          UT              49            Utah        87
#> 4 Salt Lake City, UT          UT              49            Utah        87
#> 5        Chicago, IL          IL              17        Illinois        41
#> 6    Los Angeles, CA          CA              06      California        91
#>   DestAirportID DestAirportSeqID DestLatitude DestLongitude
#> 1         14869          1486901     40.78417    -111.96694
#> 2         14869          1486901     40.78417    -111.96694
#> 3         11298          1129801     32.89444     -97.02972
#> 4         11292          1129201     39.77444    -104.87972
#> 5         14869          1486901     40.78417    -111.96694
#> 6         14869          1486901     40.78417    -111.96694
#>   DestAirportStartDate DestAirportThruDate DestAirportIsClosed
#> 1           1950-01-01          1994-08-31                   0
#> 2           1950-01-01          1994-08-31                   0
#> 3           1950-01-01          1989-12-31                   0
#> 4           1950-01-01          1993-08-31                   0
#> 5           1950-01-01          1994-08-31                   0
#> 6           1950-01-01          1994-08-31                   0
#>   DestAirportIsLatest DestCityMarketID Dest          DestCityName DestState
#> 1                   0            34614  SLC    Salt Lake City, UT        UT
#> 2                   0            34614  SLC    Salt Lake City, UT        UT
#> 3                   0            30194  DFW Dallas/Fort Worth, TX        TX
#> 4                   0            30325  DEN            Denver, CO        CO
#> 5                   0            34614  SLC    Salt Lake City, UT        UT
#> 6                   0            34614  SLC    Salt Lake City, UT        UT
#>   DestStateFips DestStateName DestWac CRSDepTime DepTime DepDelay
#> 1            49          Utah      87       0130    0145       15
#> 2            49          Utah      87       0155    0154       -1
#> 3            48         Texas      74       0600    0600        0
#> 4            08      Colorado      82       0610    0610        0
#> 5            49          Utah      87       0615    0615        0
#> 6            49          Utah      87       0620    0618       -2
#>   DepDelayMinutes DepDel15 DepartureDelayGroups DepTimeBlk TaxiOut WheelsOff
#> 1              15        1                    1  0001-0559    <NA>      <NA>
#> 2               0        0                   -1  0001-0559    <NA>      <NA>
#> 3               0        0                    0  0600-0659    <NA>      <NA>
#> 4               0        0                    0  0600-0659    <NA>      <NA>
#> 5               0        0                    0  0600-0659    <NA>      <NA>
#> 6               0        0                   -1  0600-0659    <NA>      <NA>
#>   WheelsOn TaxiIn CRSArrTime ArrTime ArrDelay ArrDelayMinutes ArrDel15
#> 1     <NA>   <NA>       0340    0353       13              13        0
#> 2     <NA>   <NA>       0814    0832       18              18        1
#> 3     <NA>   <NA>       0918    0924        6               6        0
#> 4     <NA>   <NA>       0728    0719       -9               0        0
#> 5     <NA>   <NA>       0820    0820        0               0        0
#> 6     <NA>   <NA>       0900    0858       -2               0        0
#>   ArrivalDelayGroups ArrTimeBlk Cancelled CancellationCode Diverted
#> 1                  0  0001-0559         0             <NA>        0
#> 2                  1  0800-0859         0             <NA>        0
#> 3                  0  0900-0959         0             <NA>        0
#> 4                 -1  0700-0759         0             <NA>        0
#> 5                  0  0800-0859         0             <NA>        0
#> 6                 -1  0900-0959         0             <NA>        0
#>   CRSElapsedTime ActualElapsedTime AirTime Flights Distance DistanceGroup
#> 1             70                68    <NA>       1      368             2
#> 2            259               278    <NA>       1     2125             9
#> 3            138               144    <NA>       1      988             4
#> 4             78                69    <NA>       1      381             2
#> 5            185               185    <NA>       1     1249             5
#> 6            100               100    <NA>       1      590             3
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

## Updating local data

The installed package includes flight records through June 2024. To
extend the available data beyond the installed endpoint, use:

``` r
update_slcflights_data()
```

By default, `update_slcflights_data()` checks BTS for the latest
available monthly data and downloads every available month after June
2024. You can also request an explicit endpoint:

``` r
update_slcflights_data(until = "2024-07")
update_slcflights_data(until = c(2024, 12))
```

Local updates always begin with July 2024 and form a consecutive
extension of the installed data.

The local update workflow

- downloads monthly BTS On-Time Performance files into the user cache
- downloads the BTS Master Coordinate table into the user cache
- builds annual main and diversion-only Parquet files in a staging
  directory
- reduces the coordinate table to airports used by installed and cached
  records
- enriches cached Parquet files with coordinate metadata
- finalizes cached Parquet schemas so they match the installed schema
  contract
- activates the completed cache atomically

The installed package files are not changed. The local cache is stored
in the user cache directory returned by
`tools::R_user_dir("slcflights", "cache")`.

Inspect the active cache with:

``` r
slcflights_cache_info()
```

After a cache is active, the ordinary readers automatically use it. For
example, after updating through July 2024, `read_year_main(2024)` reads
both the installed January-through-June records and the cached July
records.

Remove the local cache with:

``` r
clear_slcflights_cache()
```

## Rebuilding the packaged data

The packaged Parquet files and coordinate CSV can be rebuilt from source
data.

This is a maintainer workflow implemented in `data-raw/build_data.R`.
The workflow uses `data-raw/T_MASTER_CORD.csv` if it is already present;
otherwise, it downloads the BTS Master Coordinate support table before
reducing it into `inst/extdata/csv/T_MASTER_CORD_reduced.csv`.

To rebuild, run from a source checkout of the package:

``` r
source("data-raw/build_data.R")
build_slc_data()
```

By default, `build_slc_data()`

- rebuilds data for years 1987 through 2024
- uses Salt Lake City’s BTS airport ID (`14869`)
- downloads the annual source Parquet files into a temporary build
  directory
- removes columns that are globally all `NULL`
- filters the data to Salt Lake City-related records
- removes columns that are all `NULL` after Salt Lake City filtering
- sorts rows by flight date and scheduled departure time
- splits the filtered data into main and diversion-only files
- reduces the BTS Master Coordinate table to the airport sequence IDs
  used by the packaged flight data
- enriches the final Parquet files with airport coordinate metadata
- copies final packaged outputs into `inst/extdata/`
- removes the temporary build directory when finished

The temporary build directory is a maintainer-side build artifact. It is
not part of the installed package and should not be confused with the
user cache created by `update_slcflights_data()`.

You can also rebuild a subset of years, for example:

``` r
source("data-raw/build_data.R")
build_slc_data(years = 2019:2024)
```

Because rebuilding requires large downloads and substantial disk space,
most users should work with the installed files and, when needed, use
`update_slcflights_data()` for post-June-2024 updates.

## Data provenance

The flight records in `slcflights` are Salt Lake City-focused subsets of
the Bureau of Transportation Statistics (BTS) TranStats **On-Time:
Reporting Carrier On-Time Performance** data. BTS TranStats is the
authoritative source for the underlying flight records and field
definitions.

The installed historical Parquet files are derived from the 1987–2024
Parquet files distributed for the 2025 ASA Data Expo Challenge. The ASA
challenge page links to the BTS TranStats download page and provides
DuckDB-hosted Parquet files for the national flight records. The
maintainer build workflow filters those national files to Salt Lake
City-related records and writes the installed `main` and `div` package
files.

For months after June 2024, `update_slcflights_data()` downloads monthly
On-Time Performance ZIP files directly from BTS TranStats and writes
compatible Parquet files to a local user cache.

The coordinate table is derived from the BTS TranStats Master Coordinate
support table and contains the airport sequence identifiers needed to
enrich the flight records with airport latitude, longitude, and
date-bounded airport metadata.
