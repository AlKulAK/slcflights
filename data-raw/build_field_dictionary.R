# Maintainer-only field dictionary build script.
#
# This script creates inst/extdata/csv/field_dictionary.csv from the currently
# available slcflights public API. It records where each field appears:
# main flight records, diversion-only flight records, and the coordinate table.

build_field_dictionary <- function(
    output = file.path(
      "inst",
      "extdata",
      "csv",
      "field_dictionary.csv"
    )
) {
  main_years <- available_years("main")
  div_years <- available_years("div")

  fields_for <- function(type, year) {
    if (identical(type, "main")) {
      names(open_main(year))
    } else {
      names(open_div(year))
    }
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

  coords_fields <- names(read_coords())

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

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    field_dictionary,
    output
  )

  invisible(field_dictionary)
}
