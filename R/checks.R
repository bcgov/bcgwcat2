#' Check samples format - Expect or return minimal
#'
#' @returns Formatted samples data frame
#'
#' @noRd
#' @examples
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' check_samples(samples)

check_samples <- function(samples, check_fmt = TRUE) {
  need <- "`samples` must be output of either `fetch_renmods()` or `fmt_minimal()`"

  if (!inherits(samples, "data.frame")) {
    abort(need)
  }

  if (check_fmt) {
    if ("Location_ID" %in% names(samples)) {
      inform("Formatting raw samples with `fmt_minimal()`...")
      samples <- fmt_minimal(samples)
    } else if (!"obs_well" %in% names(samples)) {
      abort(need)
    }
  }

  samples
}

#' Check samples format - Expect or return wide
#'
#' @returns Formatted samples data frame
#'
#' @noRd
#' @examples
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' check_samples_wide(s)
#'
#' ss <- fmt_minimal(s)
#' check_samples_wide(ss)

check_samples_wide <- function(samples, check_fmt = TRUE) {
  need <- "`samples` must be output of either `fetch_renmods()`, `fmt_minimal()` or `fmt_wide()`"

  if (!inherits(samples, "data.frame")) {
    abort(need)
  }

  if (check_fmt) {
    if ("Location_ID" %in% names(samples)) {
      samples <- fmt_minimal(samples)
    }
    if (all(c("obs_well", "result_value") %in% names(samples))) {
      inform("Formatting raw samples with `fmt_wide()`...")
      samples <- fmt_wide(samples)
    } else if (!"obs_well" %in% names(samples)) {
      abort(need)
    }
  }

  samples
}
