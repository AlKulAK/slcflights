slc_test_cache_root <- file.path(
  tempdir(),
  "slcflights-test-cache"
)

unlink(slc_test_cache_root, recursive = TRUE, force = TRUE)
dir.create(slc_test_cache_root, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(SLCFLIGHTS_TEST_CACHE_ROOT = slc_test_cache_root)
