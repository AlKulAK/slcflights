.slcflights_file <- function(...) {
  rel_path <- file.path(...)
  path <- system.file(..., package = "slcflights")

  if (!nzchar(path)) {
    stop(
      sprintf("Installed slcflights file not found: %s", rel_path),
      call. = FALSE
    )
  }

  path
}

.coords_path <- function() {
  .slcflights_file(
    "extdata",
    "csv",
    "T_MASTER_CORD_reduced.csv"
  )
}

.parquet_root <- function() {
  path <- .slcflights_file("extdata", "parquet")

  if (!dir.exists(path)) {
    stop("Installed slcflights parquet directory not found", call. = FALSE)
  }

  path
}

.normalize_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }

  if (length(years) == 0L) {
    stop(
      "`years` must contain at least one year or be NULL.",
      call. = FALSE
    )
  }

  if (!is.numeric(years)) {
    stop(
      paste(
        "`years` must be a numeric vector of whole years,",
        "such as 1987 or c(1987, 1988)."
      ),
      call. = FALSE
    )
  }

  if (anyNA(years)) {
    stop("`years` must not contain missing values.", call. = FALSE)
  }

  if (any(!is.finite(years))) {
    stop("`years` must contain only finite values.", call. = FALSE)
  }

  if (any(years != trunc(years))) {
    stop("`years` must contain whole-number years.", call. = FALSE)
  }

  if (any(years < 1000 | years > 9999)) {
    stop("`years` must contain four-digit calendar years.", call. = FALSE)
  }

  sort(unique(as.integer(years)))
}

.available_years_internal <- function(type = c("main", "div")) {
  type <- match.arg(type)

  root <- .parquet_root()
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  dirs <- dirs[grepl("^Year=[0-9]{4}$", dirs)]

  if (!length(dirs)) {
    return(integer())
  }

  filename <- switch(type,
    main = "data_0_main.parquet",
    div = "data_0_div.parquet"
  )

  keep <- file.exists(file.path(root, dirs, filename))
  years <- as.integer(sub("^Year=", "", dirs[keep]))

  sort(unique(years))
}

.parquet_paths <- function(type = c("main", "div"), years = NULL) {
  type <- match.arg(type)

  yrs <- .normalize_years(years)
  if (is.null(yrs)) {
    yrs <- .available_years_internal(type)
  }

  label <- switch(type,
    main = "main",
    div = "diversion"
  )

  if (!length(yrs)) {
    stop(
      sprintf("No %s parquet files are available", label),
      call. = FALSE
    )
  }

  filename <- switch(type,
    main = "data_0_main.parquet",
    div = "data_0_div.parquet"
  )

  root <- .parquet_root()
  paths <- file.path(root, sprintf("Year=%s", yrs), filename)
  keep <- file.exists(paths)

  if (!all(keep)) {
    missing <- yrs[!keep]
    stop(
      sprintf(
        "No %s parquet file found for year%s %s",
        label,
        if (length(missing) > 1) "s" else "",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unname(paths)
}
