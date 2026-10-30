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

renmods_data_types <- function() {
  c("this_yr", "yr_2_5", "yr_5_10", "historic")
}

#' Title
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
ion_details <- function() {
  read_data("ion_details.csv")
}

#' Helper to return metadata columns for different data
#'
#' @param which Which data to fetch metadata columns for?
#' @param param_vars Logical. Include parameter variables?
#'
#' @returns
#'
#' @noRd
#' @examples
#' meta_vars()
#' meta_vars("enmods")
#' meta_vars(param_vars = TRUE)

meta_vars <- function(which = "bcgwcat", param_vars = FALSE) {
  v <- index_meta()[[paste0(which, "_col")]]

  if (param_vars) {
    v <- c(v, index_meta_params()[[paste0(which, "_col")]])
  }

  v[!is.na(v)]
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_meta <- function() {
  read_data("index_meta.csv")
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_params <- function() {
  read_data("index_params.csv")
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_meta_params <- function() {
  read_data("index_meta_params.csv")
}

#' Water Quality Data
#'
#' A filtered and formatted subset of the BC Gov water quality guidelines
#' fetched with [bcdata::bcdc_get_data()], from record
#' [`85d3990a-ec0a-4436-8ebd-150de3ba0747`](https://catalogue.data.gov.bc.ca/dataset/water-quality-guidelines-of-b-c-),
#' and resource `6f32a85b-a3d9-44c3-9a14-15175eba25b6`.
#'
#' This data is formatted for easier use in R (e.g., lower case columns names),
#' and filtered to include only parameters for Drinking Water an looking at only
#' the Upper Limits of the Maximum Acceptable Concentration. It is further
#' filtered to only include parameters of interest to groundwater analyses.
#'
#' Three columns, `enmods_param`, `bcgwcat_param` and `n` have been added to the
#' data to reflect the corresponding parameter names in EnMoDS and bcgwcat, as
#' well as to reflect how many different guidelines there are for a paraticular
#' parameters (`n`).
#'
#' @returns tibble of formatted and filtered water quality guidelines
#'
#' @export
#' @examples
#' wq_std()

wq_std <- function() {
  read_data("wq_std.csv")
}

read_data <- function(file) {
  readr::read_csv(
    system.file(
      "extdata",
      file,
      package = "bcgwcat2"
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
}
