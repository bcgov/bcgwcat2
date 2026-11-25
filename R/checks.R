# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Check samples are formatted
#'
#' Expects output of `fmt_minimal()` (default output of `fetch_renmods()`).
#'
#' @returns Invisible TRUE if check passes
#'
#' @noRd
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' check_samples(s)

check_samples <- function(samples) {
  need <- "`samples` must be output of `fetch_renmods()` or `fmt_minimal()`"

  if (!inherits(samples, "data.frame")) {
    abort(need)
  }

  if (is_raw(samples)) {
    abort(c(
      "`samples` must be formatted, not raw EnMoDS data",
      "i" = "Do not use `raw = TRUE` with `fetch_renmods()` or format raw data with `fmt_minimal()`"
    ))
  }

  if (!all(c("obs_well", "bcgwcat_param") %in% names(samples))) {
    abort(need)
  }

  invisible(TRUE)
}

#' Check samples are raw EnMoDS data
#'
#' Expects the output of `fetch_renmods(raw = TRUE)`.
#'
#' @returns Invisible TRUE if check passes
#'
#' @noRd

check_samples_raw <- function(samples) {
  if (!inherits(samples, "data.frame") || !is_raw(samples)) {
    abort(c(
      "`samples` must be raw EnMoDS data from `fetch_renmods(raw = TRUE)`",
      "i" = "The default for `fetch_renmods()` is to format the data"
    ))
  }

  invisible(TRUE)
}

#' Check samples format - Expect or return wide
#'
#' Accepts formatted samples in long (`fmt_minimal()`) or wide (`fmt_wide()`)
#' format. Long samples are converted to wide.
#'
#' @returns Wide samples data frame
#'
#' @noRd
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' check_samples_wide(s)
#' check_samples_wide(fmt_wide(s))

check_samples_wide <- function(samples) {
  is_wide <- inherits(samples, "data.frame") &&
    "obs_well" %in% names(samples) &&
    !"result_value" %in% names(samples)

  if (!is_wide) {
    abort("`samples` must be in wide format, output of `fmt_wide()`")
  }

  invisible(TRUE)
}

is_raw <- function(samples) {
  "Location_ID" %in% names(samples)
}

check_plot_ids <- function(
  samples_orig,
  samples_filtered,
  group = "location_id"
) {
  if (!nrow(samples_filtered)) {
    if (shiny::isRunning()) {
      validate(need(
        nrow(samples_filtered) > 0,
        "Missing too many samples: nothing to plot."
      ))
    }
    inform(
      "Not enough {dplyr::if_else(qc_valid_only, 'good quality', '')} data."
    )
    return(invisible(FALSE))
  }

  ids_dropped <- setdiff(samples_orig[[group]], samples_filtered[[group]])

  if (length(ids_dropped)) {
    inform(
      "Some groups have too few samples and will be omitted from the plot: {ids_dropped}"
    )
  }

  return(invisible(TRUE))
}
