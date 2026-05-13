test_that("parquet Airline ID columns are discovered from a Parquet file", {
  root <- tempfile("slc-cache-airlines-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(
      DOT_ID_Reporting_Airline = 20001L,
      Other = "x"
    ),
    path
  )

  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_equal(
    slcflights:::cache_build_pq_airline_cols(con, path),
    "DOT_ID_Reporting_Airline"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("used Airline ID column discovery requires files", {
  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_used_airline_cols(con, character()),
    "at least one Parquet file"
  )
})

test_that(
  "used Airline ID discovery requires DOT ID columns",
  {
    root <- tempfile("slc-cache-airlines-")
    dir.create(root, recursive = TRUE)

    path <- file.path(root, "x.parquet")

    arrow::write_parquet(
      data.frame(Other = "x"),
      path
    )

    con <- slcflights:::cache_build_connect()
    on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

    expect_error(
      slcflights:::cache_build_used_airline_cols(con, path),
      "No DOT_ID_Reporting_Airline columns"
    )

    unlink(root, recursive = TRUE, force = TRUE)
  }
)

test_that("Airline ID union SQL requires Airline ID columns", {
  root <- tempfile("slc-cache-airlines-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(DOT_ID_Reporting_Airline = 20001L),
    path
  )

  con <- slcflights:::cache_build_connect()
  on.exit(slcflights:::cache_build_disconnect(con), add = TRUE)

  expect_error(
    slcflights:::cache_build_airline_union_sql(con, path, character()),
    "at least one column name"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("Airline ID column validation requires Code and Description", {
  expect_error(
    slcflights:::cache_build_vldte_airline_cols("Code"),
    "Description"
  )

  expect_error(
    slcflights:::cache_build_vldte_airline_cols("Description"),
    "Code"
  )

  expect_true(
    slcflights:::cache_build_vldte_airline_cols(
      c("Code", "Description")
    )
  )
})

test_that("Airline ID reduction requires parquet files", {
  airlines <- tempfile("airlines-", fileext = ".csv")
  writeLines("Code,Description\n20001,First Airline: FA", airlines)

  expect_error(
    slcflights:::cache_build_reduce_air_csv(
      parquet_files = character(),
      airlines_in = airlines,
      airlines_out = tempfile(fileext = ".csv")
    ),
    "at least one Parquet file"
  )

  unlink(airlines)
})

test_that("Airline ID reduction requires Airline ID CSV", {
  root <- tempfile("slc-cache-airlines-")
  dir.create(root, recursive = TRUE)

  path <- file.path(root, "x.parquet")

  arrow::write_parquet(
    data.frame(DOT_ID_Reporting_Airline = 20001L),
    path
  )

  expect_error(
    slcflights:::cache_build_reduce_air_csv(
      parquet_files = path,
      airlines_in = file.path(root, "missing.csv"),
      airlines_out = file.path(root, "out.csv")
    ),
    "Airline ID CSV not found"
  )

  unlink(root, recursive = TRUE, force = TRUE)
})

test_that("Airline ID reduction keeps only used IDs in CSV order", {
  root <- tempfile("slc-cache-airlines-")
  dir.create(root, recursive = TRUE)

  main <- file.path(root, "main.parquet")
  div <- file.path(root, "div.parquet")
  airlines_in <- file.path(root, "L_AIRLINE_ID.csv")
  airlines_out <- file.path(root, "out", "L_AIRLINE_ID_reduced.csv")

  arrow::write_parquet(
    data.frame(
      DOT_ID_Reporting_Airline = c(20003L, 20001L, NA_integer_),
      Other = c("a", "b", "c")
    ),
    main
  )

  arrow::write_parquet(
    data.frame(
      DOT_ID_Reporting_Airline = c(20002L, 20001L)
    ),
    div
  )

  writeLines(
    c(
      "Code,Description",
      "20000,Unused Airline: ZZ",
      "20001,First Airline Inc.: FA (Merged with Example 1/99.)",
      "20002,Second Airline LLC: SB (1)",
      "20003,Third Airline: TC",
      "20004,Unused Later Airline: UL"
    ),
    airlines_in
  )

  out <- slcflights:::cache_build_reduce_air_csv(
    parquet_files = c(main, div),
    airlines_in = airlines_in,
    airlines_out = airlines_out
  )

  expect_equal(out, normalizePath(airlines_out, mustWork = TRUE))

  reduced <- readr::read_csv(airlines_out, show_col_types = FALSE)

  expect_equal(
    names(reduced),
    c(
      "DOT_ID_Reporting_Airline",
      "Reporting_Airline_Name",
      "Reporting_Airline_Lookup_Code"
    )
  )

  expect_equal(
    reduced$DOT_ID_Reporting_Airline,
    c(20001L, 20002L, 20003L)
  )

  expect_equal(
    reduced$Reporting_Airline_Name,
    c(
      "First Airline Inc.",
      "Second Airline LLC",
      "Third Airline"
    )
  )

  expect_equal(
    reduced$Reporting_Airline_Lookup_Code,
    c("FA", "SB (1)", "TC")
  )

  unlink(root, recursive = TRUE, force = TRUE)
})
