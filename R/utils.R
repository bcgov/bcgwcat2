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

#' Make column names pretty
#'
#' @param x Character vector of names to make pretty.
#' @param units Whether or not these include units
#'
#' @returns Character vector of pretty names
#'
#' @noRd
#' @examples
#' pretty_names("observed_date_time")
#' pretty_names(c("observed_date_time", "Cation sum meq/l"))

pretty_names <- function(x) {
  stringr::str_replace_all(x, c("-|_" = " ")) |>
    stringr::str_to_title() |>
    stringr::str_replace_all(c(
      "(m|M)eq\\/(l|L)" = "(mEq/L)",
      "(m|M)g\\/(l|L)" = "(mg/L)"
    )) |>
    stringr::str_replace_all(c("enmods" = "EnMoDS", "(I|i)d" = "ID"))
}

pretty_renmods <- function(x) {
  dplyr::recode_values(
    x,
    "this_yr" ~ "This year",
    "yr_2_5" ~ "Last 2-5 years",
    "yr_5_10" ~ "Last 5-10 years",
    "historic" ~ "Historic data"
  )
}

units_convert <- function(x, from, to) {
  dplyr::case_when(
    from == "mg/L" & to == "\u00B5g/L" ~ x * 1000,
    from == "mg/L" & to == "ug/L" ~ x * 1000,
    from == "\u00B5g/L" & to == "mg/L" ~ x / 1000,
    from == "ug/L" & to == "mg/L" ~ x / 1000,
    from == to | (is.na(from) & is.na(to)) | from == "meq/L" ~ x,
    TRUE ~ NA_real_
  )
}
