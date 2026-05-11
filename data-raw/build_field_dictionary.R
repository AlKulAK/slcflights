# Maintainer-only field dictionary build script.
#
# This script creates inst/extdata/csv/field_dictionary.csv from the rebuilt
# package data outputs through the slcflights public readers. It records where
# each field appears: main flight records, diversion-only flight records, and
# the coordinate table.

build_field_dictionary <- function(
  output = file.path(
    "inst",
    "extdata",
    "csv",
    "field_dictionary.csv"
  )
) {
  main_years <- slc_available_installed_years("main")
  div_years <- slc_available_installed_years("div")

  fields_for <- function(type, year) {
    path <- slc_installed_parquet_path(type, year)

    names(arrow::open_dataset(path, format = "parquet"))
  }

  installed_csv_fields <- function(path) {
    names(readr::read_csv(
      path,
      n_max = 0,
      show_col_types = FALSE
    ))
  }

  presence_status <- function(field, type, years) {
    present_years <- years[vapply(
      years,
      function(year) field %in% fields_for(type, year),
      logical(1)
    )]

    missing_years <- setdiff(years, present_years)

    if (length(present_years) == 0) {
      "never"
    } else if (length(missing_years) == 0) {
      "always"
    } else {
      "sometimes"
    }
  }

  txt <- function(...) {
    paste(..., sep = " ")
  }

  main_fields <- sort(unique(unlist(
    lapply(main_years, function(year) fields_for("main", year)),
    use.names = FALSE
  )))

  div_fields <- sort(unique(unlist(
    lapply(div_years, function(year) fields_for("div", year)),
    use.names = FALSE
  )))

  coords_fields <- installed_csv_fields(slc_installed_coords_path())

  all_fields <- sort(unique(c(main_fields, div_fields, coords_fields)))

  field_dictionary <- data.frame(
    field = all_fields,
    group = "",
    source = ifelse(
      all_fields %in% coords_fields,
      "BTS Master Coordinate",
      "BTS On-Time"
    ),
    description = "",
    main_presence = vapply(
      all_fields,
      presence_status,
      character(1),
      type = "main",
      years = main_years
    ),
    div_presence = vapply(
      all_fields,
      presence_status,
      character(1),
      type = "div",
      years = div_years
    ),
    coords_presence = ifelse(
      all_fields %in% coords_fields,
      "always",
      "never"
    ),
    notes = "",
    stringsAsFactors = FALSE
  )

  coordinate_metadata <- data.frame(
    field = c(
      "AIRPORT_SEQ_ID",
      "AIRPORT_ID",
      "AIRPORT",
      "DISPLAY_AIRPORT_NAME",
      "DISPLAY_AIRPORT_CITY_NAME_FULL",
      "AIRPORT_WAC_SEQ_ID2",
      "AIRPORT_WAC",
      "AIRPORT_COUNTRY_NAME",
      "AIRPORT_COUNTRY_CODE_ISO",
      "AIRPORT_STATE_NAME",
      "AIRPORT_STATE_CODE",
      "AIRPORT_STATE_FIPS",
      "CITY_MARKET_SEQ_ID",
      "CITY_MARKET_ID",
      "DISPLAY_CITY_MARKET_NAME_FULL",
      "CITY_MARKET_WAC_SEQ_ID2",
      "CITY_MARKET_WAC",
      "LAT_DEGREES",
      "LAT_HEMISPHERE",
      "LAT_MINUTES",
      "LAT_SECONDS",
      "LATITUDE",
      "LON_DEGREES",
      "LON_HEMISPHERE",
      "LON_MINUTES",
      "LON_SECONDS",
      "LONGITUDE",
      "UTC_LOCAL_TIME_VARIATION",
      "AIRPORT_START_DATE",
      "AIRPORT_THRU_DATE",
      "AIRPORT_IS_CLOSED",
      "AIRPORT_IS_LATEST"
    ),
    group = "coordinate",
    description = c(
      paste(
        "An identification number assigned by US DOT to identify a unique",
        "airport at a given point of time. Airport attributes, such as",
        "airport name or coordinates, may change over time."
      ),
      paste(
        "An identification number assigned by US DOT to identify a unique",
        "airport. Use this field for airport analysis across a range of",
        "years because an airport can change its airport code and airport",
        "codes can be reused."
      ),
      paste(
        "A three character alpha-numeric code issued by the U.S.",
        "Department of Transportation which is the official designation of",
        "the airport. The airport code is not always unique to a specific",
        "airport because airport codes can change or can be reused."
      ),
      "Airport Name.",
      "Airport City Name with either U.S. State or Country.",
      paste(
        "Unique Identifier for a World Area Code (WAC) at a given point",
        "of time for the Physical Location of the Airport. See World Area",
        "Codes support table."
      ),
      "World Area Code for the Physical Location of the Airport.",
      "Country Name for the Physical Location of the Airport.",
      paste(
        "Two-character ISO Country Code for the Physical Location of the",
        "Airport."
      ),
      "State Name for the Physical Location of the Airport.",
      "State Abbreviation for the Physical Location of the Airport.",
      paste(
        "FIPS (Federal Information Processing Standard) State Code for",
        "the Physical Location of the Airport."
      ),
      paste(
        "An identification number assigned by US DOT to identify a city",
        "market at a given point of time. City Market attributes may",
        "change over time. For example the country associated with the",
        "city market can change over time due to geopolitical changes."
      ),
      paste(
        "An identification number assigned by US DOT to identify a city",
        "market. Use this field to consolidate airports serving the same",
        "city market."
      ),
      "City Market Name with either U.S. State or Country.",
      paste(
        "Unique Identifier for a World Area Code (WAC) at a given point",
        "of time for the City Market. See World Area Codes support table."
      ),
      "World Area Code for the City Market.",
      "Latitude, Degrees.",
      "Latitude, Hemisphere.",
      "Latitude, Minutes.",
      "Latitude, Seconds.",
      "Latitude.",
      "Longitude, Degrees.",
      "Longitude, Hemisphere.",
      "Longitude, Minutes.",
      "Longitude, Seconds.",
      "Longitude.",
      "Time Zone at the Airport.",
      "Start Date of Airport Attributes.",
      "End Date of Airport Attributes (Active = NULL).",
      paste(
        "Indicates if the airport is closed (1 = Yes). If yes, the",
        "airport is closed on the AirportEndDate."
      ),
      paste(
        "Indicates if this row contains the latest attributes for the",
        "Airport (1 = Yes)."
      )
    ),
    notes = c(
      "Original BTS display name: AirportSeqID.",
      "Original BTS display name: AirportID.",
      "Original BTS display name: Airport.",
      "Original BTS display name: AirportName.",
      "Original BTS display name: AirportCityName.",
      "Original BTS display name: AirportWacSeqID2.",
      "Original BTS display name: AirportWac.",
      "Original BTS display name: AirportCountryName.",
      "Original BTS display name: AirportCountryCodeISO.",
      "Original BTS display name: AirportStateName.",
      "Original BTS display name: AirportStateCode.",
      "Original BTS display name: AirportStateFips.",
      "Original BTS display name: CityMarketSeqID.",
      "Original BTS display name: CityMarketID.",
      "Original BTS display name: CityMarketName.",
      "Original BTS display name: CityMarketWacSeqID2.",
      "Original BTS display name: CityMarketWac.",
      "Original BTS display name: LatDegrees.",
      "Original BTS display name: LatHemisphere.",
      "Original BTS display name: LatMinutes.",
      "Original BTS display name: LatSeconds.",
      "Original BTS display name: Latitude.",
      "Original BTS display name: LonDegrees.",
      "Original BTS display name: LonHemisphere.",
      "Original BTS display name: LonMinutes.",
      "Original BTS display name: LonSeconds.",
      "Original BTS display name: Longitude.",
      "Original BTS display name: UTCLocalTimeVariation.",
      "Original BTS display name: AirportStartDate.",
      "Original BTS display name: AirportEndDate.",
      "Original BTS display name: AirportIsClosed.",
      "Original BTS display name: AirportIsLatest."
    ),
    stringsAsFactors = FALSE
  )

  coordinate_idx <- match(coordinate_metadata$field, field_dictionary$field)

  if (anyNA(coordinate_idx)) {
    missing_fields <- coordinate_metadata$field[is.na(coordinate_idx)]

    stop(
      sprintf(
        paste(
          "Coordinate metadata contains fields not found in the",
          "dictionary scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[coordinate_idx] <- coordinate_metadata$group
  field_dictionary$description[coordinate_idx] <-
    coordinate_metadata$description
  field_dictionary$notes[coordinate_idx] <- coordinate_metadata$notes

  flight_metadata <- data.frame(
    field = c(
      "Year",
      "Quarter",
      "Month",
      "DayofMonth",
      "DayOfWeek",
      "FlightDate",
      "Reporting_Airline",
      "DOT_ID_Reporting_Airline",
      "IATA_CODE_Reporting_Airline",
      "Tail_Number",
      "Flight_Number_Reporting_Airline"
    ),
    group = c(
      "date",
      "date",
      "date",
      "date",
      "date",
      "date",
      "carrier",
      "carrier",
      "carrier",
      "flight",
      "flight"
    ),
    description = c(
      "Year.",
      "Quarter, coded 1 through 4.",
      "Month.",
      "Day of month.",
      "Day of week.",
      "Flight date, in yyyymmdd format.",
      txt(
        "Unique Carrier Code. When the same code has been used by multiple",
        "carriers, a numeric suffix is used for earlier users, for example",
        "PA, PA(1), PA(2). Use this field for analysis across a range of",
        "years."
      ),
      txt(
        "An identification number assigned by US DOT to identify a unique",
        "airline (carrier). A unique airline (carrier) is defined as one",
        "holding and reporting under the same DOT certificate regardless of",
        "its Code, Name, or holding company/corporation."
      ),
      txt(
        "Code assigned by IATA and commonly used to identify a carrier. As",
        "the same code may have been assigned to different carriers over",
        "time, the code is not always unique. For analysis, use the Unique",
        "Carrier Code."
      ),
      "Tail Number.",
      "Flight Number."
    ),
    notes = c(
      "",
      "",
      "",
      "",
      "",
      "",
      "BTS recommends this field for carrier analysis across years.",
      "",
      "BTS recommends Reporting_Airline for cross-year carrier analysis.",
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )

  flight_idx <- match(flight_metadata$field, field_dictionary$field)

  if (anyNA(flight_idx)) {
    missing_fields <- flight_metadata$field[is.na(flight_idx)]

    stop(
      sprintf(
        paste(
          "Flight metadata contains fields not found in the dictionary",
          "scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[flight_idx] <- flight_metadata$group
  field_dictionary$description[flight_idx] <- flight_metadata$description
  field_dictionary$notes[flight_idx] <- flight_metadata$notes

  airport_metadata <- data.frame(
    field = c(
      "OriginAirportID",
      "OriginAirportSeqID",
      "OriginCityMarketID",
      "Origin",
      "OriginCityName",
      "OriginState",
      "OriginStateFips",
      "OriginStateName",
      "OriginWac",
      "OriginLatitude",
      "OriginLongitude",
      "OriginAirportStartDate",
      "OriginAirportThruDate",
      "OriginAirportIsClosed",
      "OriginAirportIsLatest",
      "DestAirportID",
      "DestAirportSeqID",
      "DestCityMarketID",
      "Dest",
      "DestCityName",
      "DestState",
      "DestStateFips",
      "DestStateName",
      "DestWac",
      "DestLatitude",
      "DestLongitude",
      "DestAirportStartDate",
      "DestAirportThruDate",
      "DestAirportIsClosed",
      "DestAirportIsLatest"
    ),
    group = "airport",
    description = c(
      txt(
        "Origin Airport, Airport ID. An identification number assigned by",
        "US DOT to identify a unique airport. Use this field for airport",
        "analysis across a range of years because an airport can change",
        "its airport code and airport codes can be reused."
      ),
      txt(
        "Origin Airport, Airport Sequence ID. An identification number",
        "assigned by US DOT to identify a unique airport at a given point",
        "of time. Airport attributes, such as airport name or coordinates,",
        "may change over time."
      ),
      txt(
        "Origin Airport, City Market ID. City Market ID is an",
        "identification number assigned by US DOT to identify a city",
        "market. Use this field to consolidate airports serving the same",
        "city market."
      ),
      "Origin Airport.",
      "Origin Airport, City Name.",
      "Origin Airport, State Code.",
      "Origin Airport, State Fips.",
      "Origin Airport, State Name.",
      "Origin Airport, World Area Code.",
      "Origin airport latitude from the BTS Master Coordinate table.",
      "Origin airport longitude from the BTS Master Coordinate table.",
      txt(
        "Start date of origin airport attributes from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "End date of origin airport attributes from the BTS Master",
        "Coordinate table. Active airports have a missing end date."
      ),
      txt(
        "Origin airport closed indicator from the BTS Master Coordinate",
        "table."
      ),
      txt(
        "Origin airport latest-row indicator from the BTS Master Coordinate",
        "table."
      ),
      txt(
        "Destination Airport, Airport ID. An identification number assigned",
        "by US DOT to identify a unique airport. Use this field for airport",
        "analysis across a range of years because an airport can change",
        "its airport code and airport codes can be reused."
      ),
      txt(
        "Destination Airport, Airport Sequence ID. An identification",
        "number assigned by US DOT to identify a unique airport at a given",
        "point of time. Airport attributes, such as airport name or",
        "coordinates, may change over time."
      ),
      txt(
        "Destination Airport, City Market ID. City Market ID is an",
        "identification number assigned by US DOT to identify a city",
        "market. Use this field to consolidate airports serving the same",
        "city market."
      ),
      "Destination Airport.",
      "Destination Airport, City Name.",
      "Destination Airport, State Code.",
      "Destination Airport, State Fips.",
      "Destination Airport, State Name.",
      "Destination Airport, World Area Code.",
      "Destination airport latitude from the BTS Master Coordinate table.",
      "Destination airport longitude from the BTS Master Coordinate table.",
      txt(
        "Start date of destination airport attributes from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "End date of destination airport attributes from the BTS Master",
        "Coordinate table. Active airports have a missing end date."
      ),
      txt(
        "Destination airport closed indicator from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "Destination airport latest-row indicator from the BTS Master",
        "Coordinate table."
      )
    ),
    notes = c(
      "BTS recommends AirportID for airport analysis across years.",
      "",
      "Useful for consolidating airports serving the same city market.",
      "",
      "",
      "",
      "",
      "",
      "",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "BTS recommends AirportID for airport analysis across years.",
      "",
      "Useful for consolidating airports serving the same city market.",
      "",
      "",
      "",
      "",
      "",
      "",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field.",
      "Package-added coordinate enrichment field."
    ),
    stringsAsFactors = FALSE
  )

  airport_idx <- match(airport_metadata$field, field_dictionary$field)

  if (anyNA(airport_idx)) {
    missing_fields <- airport_metadata$field[is.na(airport_idx)]

    stop(
      sprintf(
        paste(
          "Airport metadata contains fields not found in the dictionary",
          "scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[airport_idx] <- airport_metadata$group
  field_dictionary$description[airport_idx] <- airport_metadata$description
  field_dictionary$notes[airport_idx] <- airport_metadata$notes

  timing_metadata <- data.frame(
    field = c(
      "CRSDepTime",
      "DepTime",
      "DepDelay",
      "DepDelayMinutes",
      "DepDel15",
      "DepartureDelayGroups",
      "DepTimeBlk",
      "TaxiOut",
      "WheelsOff",
      "WheelsOn",
      "TaxiIn",
      "CRSArrTime",
      "ArrTime",
      "ArrDelay",
      "ArrDelayMinutes",
      "ArrDel15",
      "ArrivalDelayGroups",
      "ArrTimeBlk",
      "Cancelled",
      "CancellationCode",
      "Diverted",
      "CRSElapsedTime",
      "ActualElapsedTime",
      "AirTime",
      "Flights",
      "Distance",
      "DistanceGroup"
    ),
    group = c(
      rep("departure", 9),
      rep("arrival", 9),
      "status",
      "status",
      "status",
      "elapsed",
      "elapsed",
      "elapsed",
      "flight",
      "distance",
      "distance"
    ),
    description = c(
      "CRS Departure Time, local time in hhmm format.",
      "Actual Departure Time, local time in hhmm format.",
      txt(
        "Difference in minutes between scheduled and actual departure",
        "time. Early departures show negative numbers."
      ),
      txt(
        "Difference in minutes between scheduled and actual departure",
        "time. Early departures are set to 0."
      ),
      "Departure Delay Indicator, 15 Minutes or More (1 = Yes).",
      "Departure Delay intervals, every 15 minutes from <-15 to >180.",
      "CRS Departure Time Block, hourly intervals.",
      "Taxi Out Time, in minutes.",
      "Wheels Off Time, local time in hhmm format.",
      "Wheels On Time, local time in hhmm format.",
      "Taxi In Time, in minutes.",
      "CRS Arrival Time, local time in hhmm format.",
      "Actual Arrival Time, local time in hhmm format.",
      txt(
        "Difference in minutes between scheduled and actual arrival",
        "time. Early arrivals show negative numbers."
      ),
      txt(
        "Difference in minutes between scheduled and actual arrival",
        "time. Early arrivals are set to 0."
      ),
      "Arrival Delay Indicator, 15 Minutes or More (1 = Yes).",
      "Arrival Delay intervals, every 15 minutes from <-15 to >180.",
      "CRS Arrival Time Block, hourly intervals.",
      "Cancelled Flight Indicator (1 = Yes).",
      "Specifies the reason for cancellation.",
      "Diverted Flight Indicator (1 = Yes).",
      "CRS Elapsed Time of Flight, in minutes.",
      "Elapsed Time of Flight, in minutes.",
      "Flight Time, in minutes.",
      "Number of Flights.",
      "Distance between airports, in miles.",
      "Distance intervals, every 250 miles, for flight segment."
    ),
    notes = "",
    stringsAsFactors = FALSE
  )

  timing_idx <- match(timing_metadata$field, field_dictionary$field)

  if (anyNA(timing_idx)) {
    missing_fields <- timing_metadata$field[is.na(timing_idx)]

    stop(
      sprintf(
        paste(
          "Timing metadata contains fields not found in the dictionary",
          "scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[timing_idx] <- timing_metadata$group
  field_dictionary$description[timing_idx] <- timing_metadata$description
  field_dictionary$notes[timing_idx] <- timing_metadata$notes

  cause_metadata <- data.frame(
    field = c(
      "CarrierDelay",
      "WeatherDelay",
      "NASDelay",
      "SecurityDelay",
      "LateAircraftDelay",
      "FirstDepTime",
      "TotalAddGTime",
      "LongestAddGTime"
    ),
    group = c(
      rep("delay_cause", 5),
      rep("gate_return", 3)
    ),
    description = c(
      "Carrier Delay, in minutes.",
      "Weather Delay, in minutes.",
      "National Air System Delay, in minutes.",
      "Security Delay, in minutes.",
      "Late Aircraft Delay, in minutes.",
      "First Gate Departure Time at Origin Airport.",
      txt(
        "Total Ground Time Away from Gate for Gate Return or Cancelled",
        "Flight."
      ),
      txt(
        "Longest Time Away from Gate for Gate Return or Cancelled",
        "Flight."
      )
    ),
    notes = "",
    stringsAsFactors = FALSE
  )

  cause_idx <- match(cause_metadata$field, field_dictionary$field)

  if (anyNA(cause_idx)) {
    missing_fields <- cause_metadata$field[is.na(cause_idx)]

    stop(
      sprintf(
        paste(
          "Delay-cause metadata contains fields not found in the",
          "dictionary scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[cause_idx] <- cause_metadata$group
  field_dictionary$description[cause_idx] <- cause_metadata$description
  field_dictionary$notes[cause_idx] <- cause_metadata$notes

  diversion_summary_metadata <- data.frame(
    field = c(
      "DivAirportLandings",
      "DivReachedDest",
      "DivActualElapsedTime",
      "DivArrDelay",
      "DivDistance"
    ),
    group = "diversion_summary",
    description = c(
      "Number of Diverted Airport Landings.",
      "Diverted Flight Reaching Scheduled Destination Indicator (1 = Yes).",
      txt(
        "Elapsed Time of Diverted Flight Reaching Scheduled Destination,",
        "in minutes. The ActualElapsedTime column remains NULL for all",
        "diverted flights."
      ),
      txt(
        "Difference in minutes between scheduled and actual arrival time",
        "for a diverted flight reaching scheduled destination. The ArrDelay",
        "column remains NULL for all diverted flights."
      ),
      txt(
        "Distance between scheduled destination and final diverted airport,",
        "in miles. Value will be 0 for diverted flight reaching scheduled",
        "destination."
      )
    ),
    notes = "",
    stringsAsFactors = FALSE
  )

  diversion_summary_idx <- match(
    diversion_summary_metadata$field,
    field_dictionary$field
  )

  if (anyNA(diversion_summary_idx)) {
    missing_fields <-
      diversion_summary_metadata$field[is.na(diversion_summary_idx)]

    stop(
      sprintf(
        paste(
          "Diversion-summary metadata contains fields not found in the",
          "dictionary scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[diversion_summary_idx] <-
    diversion_summary_metadata$group
  field_dictionary$description[diversion_summary_idx] <-
    diversion_summary_metadata$description
  field_dictionary$notes[diversion_summary_idx] <-
    diversion_summary_metadata$notes

  diversion_airport_12_metadata <- data.frame(
    field = c(
      "Div1Airport",
      "Div1AirportID",
      "Div1AirportSeqID",
      "Div1Latitude",
      "Div1Longitude",
      "Div1AirportStartDate",
      "Div1AirportThruDate",
      "Div1AirportIsClosed",
      "Div1AirportIsLatest",
      "Div1WheelsOn",
      "Div1TotalGTime",
      "Div1LongestGTime",
      "Div1WheelsOff",
      "Div1TailNum",
      "Div2Airport",
      "Div2AirportID",
      "Div2AirportSeqID",
      "Div2Latitude",
      "Div2Longitude",
      "Div2AirportStartDate",
      "Div2AirportThruDate",
      "Div2AirportIsClosed",
      "Div2AirportIsLatest",
      "Div2WheelsOn",
      "Div2TotalGTime",
      "Div2LongestGTime",
      "Div2WheelsOff",
      "Div2TailNum"
    ),
    group = "diversion_airport",
    description = c(
      "Diverted Airport Code 1.",
      txt(
        "Airport ID of Diverted Airport 1. Airport ID is a unique key for",
        "an airport."
      ),
      txt(
        "Airport Sequence ID of Diverted Airport 1. Unique key for",
        "time-specific information for an airport."
      ),
      "Diverted Airport 1 latitude from the BTS Master Coordinate table.",
      "Diverted Airport 1 longitude from the BTS Master Coordinate table.",
      txt(
        "Start date of Diverted Airport 1 attributes from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "End date of Diverted Airport 1 attributes from the BTS Master",
        "Coordinate table. Active airports have a missing end date."
      ),
      txt(
        "Diverted Airport 1 closed indicator from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "Diverted Airport 1 latest-row indicator from the BTS Master",
        "Coordinate table."
      ),
      "Wheels On Time, local time in hhmm format, at Diverted Airport 1.",
      "Total Ground Time Away from Gate at Diverted Airport 1.",
      "Longest Ground Time Away from Gate at Diverted Airport 1.",
      "Wheels Off Time, local time in hhmm format, at Diverted Airport 1.",
      "Aircraft Tail Number for Diverted Airport 1.",
      "Diverted Airport Code 2.",
      txt(
        "Airport ID of Diverted Airport 2. Airport ID is a unique key for",
        "an airport."
      ),
      txt(
        "Airport Sequence ID of Diverted Airport 2. Unique key for",
        "time-specific information for an airport."
      ),
      "Diverted Airport 2 latitude from the BTS Master Coordinate table.",
      "Diverted Airport 2 longitude from the BTS Master Coordinate table.",
      txt(
        "Start date of Diverted Airport 2 attributes from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "End date of Diverted Airport 2 attributes from the BTS Master",
        "Coordinate table. Active airports have a missing end date."
      ),
      txt(
        "Diverted Airport 2 closed indicator from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "Diverted Airport 2 latest-row indicator from the BTS Master",
        "Coordinate table."
      ),
      "Wheels On Time, local time in hhmm format, at Diverted Airport 2.",
      "Total Ground Time Away from Gate at Diverted Airport 2.",
      "Longest Ground Time Away from Gate at Diverted Airport 2.",
      "Wheels Off Time, local time in hhmm format, at Diverted Airport 2.",
      "Aircraft Tail Number for Diverted Airport 2."
    ),
    notes = c(
      "",
      "",
      "",
      rep("Package-added coordinate enrichment field.", 6),
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      rep("Package-added coordinate enrichment field.", 6),
      "",
      "",
      "",
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )

  diversion_airport_12_idx <- match(
    diversion_airport_12_metadata$field,
    field_dictionary$field
  )

  if (anyNA(diversion_airport_12_idx)) {
    missing_fields <-
      diversion_airport_12_metadata$field[is.na(diversion_airport_12_idx)]

    stop(
      sprintf(
        paste(
          "Diversion-airport 1-2 metadata contains fields not found in",
          "the dictionary scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[diversion_airport_12_idx] <-
    diversion_airport_12_metadata$group
  field_dictionary$description[diversion_airport_12_idx] <-
    diversion_airport_12_metadata$description
  field_dictionary$notes[diversion_airport_12_idx] <-
    diversion_airport_12_metadata$notes

  diversion_airport_3_metadata <- data.frame(
    field = c(
      "Div3Airport",
      "Div3AirportID",
      "Div3AirportSeqID",
      "Div3Latitude",
      "Div3Longitude",
      "Div3AirportStartDate",
      "Div3AirportThruDate",
      "Div3AirportIsClosed",
      "Div3AirportIsLatest",
      "Div3WheelsOn",
      "Div3TotalGTime",
      "Div3LongestGTime"
    ),
    group = "diversion_airport",
    description = c(
      "Diverted Airport Code 3.",
      txt(
        "Airport ID of Diverted Airport 3. Airport ID is a unique key for",
        "an airport."
      ),
      txt(
        "Airport Sequence ID of Diverted Airport 3. Unique key for",
        "time-specific information for an airport."
      ),
      "Diverted Airport 3 latitude from the BTS Master Coordinate table.",
      "Diverted Airport 3 longitude from the BTS Master Coordinate table.",
      txt(
        "Start date of Diverted Airport 3 attributes from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "End date of Diverted Airport 3 attributes from the BTS Master",
        "Coordinate table. Active airports have a missing end date."
      ),
      txt(
        "Diverted Airport 3 closed indicator from the BTS Master",
        "Coordinate table."
      ),
      txt(
        "Diverted Airport 3 latest-row indicator from the BTS Master",
        "Coordinate table."
      ),
      "Wheels On Time, local time in hhmm format, at Diverted Airport 3.",
      "Total Ground Time Away from Gate at Diverted Airport 3.",
      "Longest Ground Time Away from Gate at Diverted Airport 3."
    ),
    notes = c(
      "",
      "",
      "",
      rep("Package-added coordinate enrichment field.", 6),
      "",
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )

  diversion_airport_3_idx <- match(
    diversion_airport_3_metadata$field,
    field_dictionary$field
  )

  if (anyNA(diversion_airport_3_idx)) {
    missing_fields <-
      diversion_airport_3_metadata$field[is.na(diversion_airport_3_idx)]

    stop(
      sprintf(
        paste(
          "Diversion-airport 3 metadata contains fields not found in",
          "the dictionary scaffold: %s"
        ),
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  field_dictionary$group[diversion_airport_3_idx] <-
    diversion_airport_3_metadata$group
  field_dictionary$description[diversion_airport_3_idx] <-
    diversion_airport_3_metadata$description
  field_dictionary$notes[diversion_airport_3_idx] <-
    diversion_airport_3_metadata$notes

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    field_dictionary,
    output
  )

  invisible(field_dictionary)
}
