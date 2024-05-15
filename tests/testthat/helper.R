#' Helper function that gets csv test data quietly
#'
#' @family helper functions for testthat
#'
#' @return A dataframe
read_test_data <- function(file, path = testthat::test_path("test_data", file)) {
  readr::read_csv(path, col_types = list())
}

#' Helper function that sets up temporary target directory, if it does not exist
#'
#' @param path Character. Main path to the directory the output file should be
#'   written to.
#' @param add_level Character. String with an investor or portfolio name that is
#'   added to the initial path as an extra layer to the output directory.
#'   This is where the file will end up being written to.
#' @family helper functions for testthat
#'
#' @return A dataframe
test_create_target_directory <- function(path = NULL,
                                         add_level = NULL) {
  path <- path %||% stop("Must provide 'path'")
  add_level <- add_level %||% stop("Must provide 'add_level'")

  if (!dir.exists(file.path(path, add_level))) {
    target_dir <- dir.create(file.path(path, add_level))
  } else {
    target_dir <- file.path(path, add_level)
  }
}


#' Get a valid value of an argument of `run_trisk()`
#'
#' @examples
#' get_st_argument("risk_free_rate")
#' # Same
#' get_st_argument("risk_free_rate", "default")
#'
#' get_st_argument("risk_free_rate", "min")
#'
#' typeof(get_st_argument("asset_type"))
#' @noRd
get_st_argument <- function(name,
                            value = c("default", "allowed", "min", "max")) {
  stopifnot(is.character(name), length(name) == 1)

  value <- match.arg(value)
  out <- stress_test_arguments

  out <- out[out$name == name, c("type", value)]
  as_type <- get(paste0("as.", out$type))
  out <- as_type(out[[value]])
  out
}

#' conveniently access stress test related files in the directory set via envvar
#' or options
#'
#' @param ... Character vectors, if any values are `NA`, the result will also be
#'   `NA`.
#' @param data_store String. Directory that contains the relevant data for the stress test analysis
#'
#' @family miscellaneous utility functions
#'
#' @return Character
#'
#' @examples
#' r2dii.climate.stress.test:::data_path()
data_path <- function(..., data_store = "data-raw") {
  fs::path(data_store, ...)
}

expect_no_error <- function(object, ...) {
  testthat::expect_error(object, regexp = NA, ...)
}

#' Help skip tests where the developer has not opted in running snapshots
#'
#' To opt in set the environment variable `ST_OPT_IN_SNAPSHOTS = TRUE`, maybe
#' via `usethis::edit_r_environ("project")`.
#'
#' @examples
#' default <- list(ST_OPT_IN_SNAPSHOTS = FALSE)
#' withr::with_envvar(default, testthat::skip_if_not(opt_in_snapshots()))
#'
#' opt_in <- list(ST_OPT_IN_SNAPSHOTS = TRUE)
#' withr::with_envvar(opt_in, testthat::skip_if_not(opt_in_snapshots()))
#' @noRd
opt_in_snapshots <- function() {
  out <- Sys.getenv("ST_OPT_IN_SNAPSHOTS", unset = "FALSE")
  as.logical(out)
}

skip_slow_tests <- function() {
  skipping_slow_tests <- as.logical(
    Sys.getenv("ST_SKIP_SLOW_TESTS", unset = "TRUE")
  )
  testthat::skip_if(skipping_slow_tests)
}
