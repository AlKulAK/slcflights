# slcflights 0.1.2

## User-facing changes

* Updated `available_years()` so that calling it without a `type` reports both
  main and diversion-only year coverage. Calling it with `"main"` or `"div"`
  returns the corresponding integer vector.

* Made `read_field_dictionary()` independent of installed package external
  files. The field dictionary is now built into the package code and is
  available without an active local database.

## Internal cleanup

* Removed remaining installed-data path helpers from the reader path layer.
  Reader-facing paths now resolve only through the active local database.

* Removed obsolete installed-data tests and stale path tests.

* Renamed the local database payload directory from `extdata/` to `data/` to
  avoid confusion with package-installed external data.

* Removed obsolete `data-raw/` scripts that rebuilt old `inst/extdata`
  artifacts.

* Updated tests and documentation for the cleaned local-database architecture.

# slcflights 0.1.1

## Bug fixes

* Made the cached database builder robust to raw BTS CSV files that contain
  invalid UTF-8 byte sequences. The builder now preserves the original
  downloaded CSV files, retries affected annual builds with temporary
  sanitized CSV copies, and replaces only invalid UTF-8 byte sequences with
  `@`.

* Added regression tests for the UTF-8 sanitizing path. The tests verify that
  valid ASCII values are preserved exactly and that invalid UTF-8 bytes are
  replaced with `@` in cached annual Parquet output.

# slcflights 0.1.0

Initial public GitHub release.

* Added a NEWS.md file to track changes to the package.
