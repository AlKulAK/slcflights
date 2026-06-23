make_stage_coords_csv <- function(path) {
  writeLines(
    c(
      paste(
        "AIRPORT_SEQ_ID",
        "LATITUDE",
        "LONGITUDE",
        "AIRPORT_START_DATE",
        "AIRPORT_THRU_DATE",
        "AIRPORT_IS_CLOSED",
        "AIRPORT_IS_LATEST",
        sep = ","
      ),
      "1,40.1,-111.1,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "2,40.2,-111.2,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "3,40.3,-111.3,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1",
      "4,40.4,-111.4,01/01/2020 12:00:00 AM,12/31/2099 12:00:00 AM,0,1"
    ),
    path
  )

  path
}

make_stage_airlines_csv <- function(path) {
  writeLines(
    c(
      paste(
        "Code",
        "Description",
        sep = ","
      ),
      "20001,First Airline Inc.: FA",
      "20002,Second Airline LLC: SB",
      "20003,Third Airline: TC"
    ),
    path
  )

  path
}

make_stage_bts_csv <- function(path) {
  rows <- data.frame(
    FlightDate = c("2024-07-02", "2024-07-01", "2024-07-03"),
    CRSDepTime = c(900L, 800L, 700L),
    DOT_ID_Reporting_Airline = c(20001L, 20002L, 20003L),
    OriginAirportID = c(14869L, 11111L, 22222L),
    DestAirportID = c(33333L, 14869L, 44444L),
    OriginAirportSeqID = c(1L, 2L, 3L),
    DestAirportSeqID = c(2L, 3L, 4L)
  )

  utils::write.csv(
    rows,
    path,
    row.names = FALSE,
    na = ""
  )

  path
}
