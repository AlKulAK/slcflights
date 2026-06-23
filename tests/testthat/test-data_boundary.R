test_that("packaged and downloadable data boundaries are explicit", {
  expect_equal(
    format_year_month(slcflights:::slc_bundled_start()),
    "1987-10"
  )

  expect_equal(
    format_year_month(slcflights:::slc_bundled_end()),
    "2024-06"
  )

  expect_equal(
    format_year_month(slcflights:::slc_first_download()),
    "2024-07"
  )

  expect_equal(
    format_year_month(slcflights:::slc_db_start()),
    "1987-10"
  )
})

test_that("year-month constructor and validator accept valid values", {
  x <- slcflights:::new_year_month(2025, 3)

  expect_s3_class(x, "slc_year_month")
  expect_equal(x$year, 2025L)
  expect_equal(x$month, 3L)

  expect_identical(
    slcflights:::validate_year_month(x),
    x
  )
})

test_that("year-month validator rejects malformed objects", {
  expect_error(
    slcflights:::validate_year_month(list(year = 2025L, month = 3L)),
    "slcflights year-month object"
  )

  expect_error(
    slcflights:::validate_year_month(
      structure(list(year = 2025L), class = "slc_year_month")
    ),
    "must contain `year` and `month`"
  )

  expect_error(
    slcflights:::validate_year_month(
      slcflights:::new_year_month(2025, 13)
    ),
    "integer from 1 to 12"
  )
})

test_that("numeric year-month coercion validates inputs", {
  x <- slcflights:::as_year_month(2025, 3)

  expect_s3_class(x, "slc_year_month")
  expect_equal(x$year, 2025L)
  expect_equal(x$month, 3L)

  expect_error(
    slcflights:::as_year_month(2025, 3.5),
    "whole-number"
  )

  expect_error(
    slcflights:::as_year_month(2025, NA),
    "missing or non-finite"
  )

  expect_error(
    slcflights:::as_year_month(2025, 13),
    "integer from 1 to 12"
  )
})

test_that("string year-month coercion validates format", {
  x <- slcflights:::as_year_month_string("2025-03")

  expect_s3_class(x, "slc_year_month")
  expect_equal(x$year, 2025L)
  expect_equal(x$month, 3L)

  expect_error(
    slcflights:::as_year_month_string("2025"),
    "YYYY-MM"
  )

  expect_error(
    slcflights:::as_year_month_string("2025-3"),
    "YYYY-MM"
  )

  expect_error(
    slcflights:::as_year_month_string("2025-13"),
    "integer from 1 to 12"
  )
})

test_that("year-month formatting and indexing are inverse operations", {
  x <- slcflights:::as_year_month_string("2025-03")
  index <- slcflights:::year_month_index(x)
  y <- slcflights:::index_to_year_month(index)

  expect_equal(
    slcflights:::format_year_month(y),
    "2025-03"
  )
})

test_that("update endpoint normalization supports intended inputs", {
  expect_identical(
    slcflights:::normalize_update_until(),
    "latest"
  )

  x <- slcflights:::normalize_update_until("2025-03")
  expect_equal(x$year, 2025L)
  expect_equal(x$month, 3L)

  y <- slcflights:::normalize_update_until(c(2025, 3))
  expect_equal(y$year, 2025L)
  expect_equal(y$month, 3L)
})

test_that("update endpoint normalization rejects invalid inputs", {
  expect_error(
    slcflights:::normalize_update_until("2025"),
    "YYYY-MM"
  )

  expect_error(
    slcflights:::normalize_update_until(c(2025, 3, 1)),
    "latest"
  )

  expect_error(
    slcflights:::normalize_update_until(TRUE),
    "latest"
  )
})

test_that("update endpoints before July 2024 are illegal", {
  expect_error(
    slcflights:::validate_update_until(
      slcflights:::as_year_month_string("2024-06")
    ),
    "bundled slcflights data end in June 2024"
  )

  expect_error(
    slcflights:::validate_update_until(
      slcflights:::as_year_month_string("2023-12")
    ),
    "bundled slcflights data end in June 2024"
  )
})

test_that("update month sequence is consecutive from July 2024", {
  x <- slcflights:::update_month_sequence(
    slcflights:::as_year_month_string("2024-07")
  )

  expect_equal(nrow(x), 1L)
  expect_equal(x$year, 2024L)
  expect_equal(x$month, 7L)

  y <- slcflights:::update_month_sequence(
    slcflights:::as_year_month_string("2024-12")
  )

  expect_equal(y$year, rep(2024L, 6L))
  expect_equal(y$month, 7:12)

  z <- slcflights:::update_month_sequence(
    slcflights:::as_year_month_string("2025-03")
  )

  expect_equal(
    z,
    data.frame(
      year = c(rep(2024L, 6L), rep(2025L, 3L)),
      month = c(7:12, 1:3)
    )
  )
})

test_that("database endpoint normalization supports intended inputs", {
  expect_identical(
    slcflights:::normalize_db_until("latest"),
    "latest"
  )

  x <- slcflights:::normalize_db_until(2025)
  expect_equal(x$year, 2025L)
  expect_equal(x$month, 12L)

  y <- slcflights:::normalize_db_until("2025-03")
  expect_equal(y$year, 2025L)
  expect_equal(y$month, 3L)

  z <- slcflights:::normalize_db_until(c(2025, 3))
  expect_equal(z$year, 2025L)
  expect_equal(z$month, 3L)
})

test_that("database endpoint normalization rejects invalid inputs", {
  expect_error(
    slcflights:::normalize_db_until("2025"),
    "YYYY-MM"
  )

  expect_error(
    slcflights:::normalize_db_until(c(2025, 3, 1)),
    "four-digit year"
  )

  expect_error(
    slcflights:::normalize_db_until(TRUE),
    "four-digit year"
  )
})

test_that("database endpoints before October 1987 are illegal", {
  expect_error(
    slcflights:::validate_db_until(
      slcflights:::as_year_month_string("1987-09")
    ),
    "October 1987"
  )

  expect_error(
    slcflights:::validate_db_until(
      slcflights:::as_year_month_string("1986-12")
    ),
    "October 1987"
  )
})

test_that("database month sequence is consecutive from October 1987", {
  x <- slcflights:::db_month_sequence(
    slcflights:::as_year_month_string("1987-10")
  )

  expect_equal(nrow(x), 1L)
  expect_equal(x$year, 1987L)
  expect_equal(x$month, 10L)

  y <- slcflights:::db_month_sequence(
    slcflights:::as_year_month_string("1987-12")
  )

  expect_equal(
    y,
    data.frame(
      year = rep(1987L, 3L),
      month = 10:12
    )
  )

  z <- slcflights:::db_month_sequence(
    slcflights:::as_year_month_string("1988-03")
  )

  expect_equal(
    z,
    data.frame(
      year = c(rep(1987L, 3L), rep(1988L, 3L)),
      month = c(10:12, 1:3)
    )
  )
})

test_that("database extension months are consecutive after current endpoint", {
  x <- slcflights:::db_extension_months(
    current_end = slcflights:::as_year_month_string("2024-06"),
    until = slcflights:::as_year_month_string("2024-07")
  )

  expect_equal(
    x,
    data.frame(year = 2024L, month = 7L)
  )

  y <- slcflights:::db_extension_months(
    current_end = slcflights:::as_year_month_string("2024-06"),
    until = slcflights:::as_year_month_string("2024-12")
  )

  expect_equal(
    y,
    data.frame(
      year = rep(2024L, 6L),
      month = 7:12
    )
  )
})

test_that("database extension months are empty when endpoint already exists", {
  x <- slcflights:::db_extension_months(
    current_end = slcflights:::as_year_month_string("2024-06"),
    until = slcflights:::as_year_month_string("2024-06")
  )

  expect_equal(
    x,
    data.frame(year = integer(), month = integer())
  )

  y <- slcflights:::db_extension_months(
    current_end = slcflights:::as_year_month_string("2024-06"),
    until = slcflights:::as_year_month_string("2024-05")
  )

  expect_equal(
    y,
    data.frame(year = integer(), month = integer())
  )
})
