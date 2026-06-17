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

#' Format EnMoDS data
#'
#' This family of functions formats and filters data returned by [fetch_renmods()].
#' All functions use [fmt_minimal()] as their first step.
#'
#' **`fmt_minimal()`**
#'
#' - keeps only columns listed in `index_meta` and `index_meta_params`
#' - filters to only parameters (observed properties) listed in `index_param`
#' - all column names converted to lower case (snake_case)
#' - creates `obs_well` filled with Observation Well listed in `location_name`
#'
#' **`fmt_wide()`** builds on `fmt_minimal()`
#'
#' - transform the parameters/observed properties from long into wide format
#' - if any parameters for a specific observation (identified by `location_id` and `observation_date_time`) have been measured multiple times with different methods, a single measurement will be selected
#'
#' **`fmt_aquachem()`** - Builds on `fmt_wide()`
#'
#' - create a data set which can be imported into AquaChem
#' - rename columns to those required by AquaChem
#' - adds units as second row of data (very awkward in R, so best if only used
#'   with AquaChem)
#'
#' @param samples Data frame. Output from [fetch_renmods()]
#' @param save Logical. Whether to automatically save the formatted data to
#' `out_path`.
#' @param out_path Character. Location and file name to save data to if `save =
#' TRUE`.
#'
#' @returns Filtered and formatted data frame of EnMoDS data.
#'
#' @name Formatting functions
#' @aliases fmt
#'
#' @export
#' @examples
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' ) |>
#' fmt_minimal()
#'
#' s

#' @rdname fmt
fmt_minimal <- function(
  samples,
  save = FALSE,
  out_path = paste0("bcgwcat_", Sys.Date(), ".csv")
) {
  # TODO: Check path

  s <- samples |>
    # Get only columns of interest
    dplyr::rename_with(tolower) |>
    dplyr::select(any_of(c(
      index_meta_params$enmods_name,
      index_meta$enmods_name,
      "obs_well" # In case all ready done
    ))) |>
    # Get only parameters of interest
    dplyr::filter(observed_property_id %in% index_params$enmods_name) |>
    add_obs_wells() |>
    dplyr::select(-dplyr::any_of("location_name")) |>
    dplyr::relocate(
      obs_well,
      location_id,
      location_latitude,
      location_longitude,
      observed_date_time,
      medium
    )
}

#' @rdname fmt
fmt_wide <- function(
  samples,
  include_units = TRUE,
fix_multi_method = c("first", "min", "max"),
  save = FALSE,
  out_path = paste0("wide_", Sys.Date(), ".csv")
) {
  #TODO add saving
  samples |>
    fmt_minimal() |>
    fix_multiple_measures(fix_multi_method) |>
    dplyr::left_join(
      dplyr::select(index_params, "enmods_name", "bcgwcat_wide"),
      by = c("observed_property_id" = "enmods_name")
    ) |>
    add_units("bcgwcat_wide", "result_unit", include_units) |>
    dplyr::select(
-"observed_property_id",
      -"observed_property_name",
      -"analysis_method",
      -"result_unit"
    ) |>
        tidyr::pivot_wider(
      names_from = "bcgwcat_wide",
      values_from = "result_value"
    )
}

#' @rdname fmt
fmt_aquachem <- function(
  samples,
  save = FALSE,
  out_path = paste0("aquachem_", Sys.Date(), ".csv")
) {

add_obs_wells <- function(df) {
  if ("obs_well" %in% names(df) && !"location_name" %in% names(df)) {
    return(df)
  }
  # Extract Observation Wells
  df |>
    dplyr::mutate(
      obs_well = stringr::str_extract(
        .data$location_name,
        "(?<=OBS WELL )\\d{1,4}"
      ) |>
        as.numeric()
    )
}

add_units <- function(df, param_col, units_col, include_units) {
  if (include_units) {
    df <- dplyr::mutate(
      df,
      !!units_col := stringr::str_replace(.data[[units_col]], " ", "_"),
      !!param_col := glue("{.data[[param_col]]}_{.data[[units_col]]}")
    )
  }
  df
}
