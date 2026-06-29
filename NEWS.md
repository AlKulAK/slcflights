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
