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
#' - keeps only columns listed in [index_meta()] and [index_meta_params()]
#' - filters to only parameters (observed properties) listed in [index_params()]
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
#' @param include_units Logical. Whether or not to append units to parameter
#'   column names.
#' @param fix_multi_method Character. How should multiple measurements be fixed?
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
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' fmt_minimal(s)
#' fmt_wide(s)
#'
#' s

#' @rdname fmt
fmt_minimal <- function(
  samples,
  save = FALSE,
  out_path = paste0("bcgwcat_", Sys.Date(), ".csv")
) {
  # TODO: Check path

  s <- fmt_base(samples)

  # Add calculated values
  s <- s |>
    add_meq() |>
    add_charge_balance() |>
    add_water_type()

  s
}

#' @rdname fmt
fmt_wide <- function(
  samples,
  params = NULL,
  include_units = TRUE,
  fix_multi_method = "first",
  save = FALSE,
  out_path = paste0("wide_", Sys.Date(), ".csv")
) {
  # TODO add saving
  samples <- check_samples(samples)

  if (!is.null(params)) {
    samples <- dplyr::filter(samples, .data$bcgwcat_param %in% .env$params)
  }

  prep <- samples |>
    add_units_to_col("bcgwcat_param", "result_unit", include_units) |>
    # Multiple samples of the same type
    fix_multiple_measures(
      method = fix_multi_method,
      type = "repeated_measure"
    ) |>
    # Multiple samples, different methods
    fix_multiple_measures(method = fix_multi_method, type = "multi_measure") |>
    fix_multiple_measures(method = fix_multi_method, type = "meq") |>
    dplyr::select(
      -"observed_property_id",
      -"observed_property_name",
      -"analysis_method",
      -"result_unit"
    )

  #TODO: Consider adding placeholders incase missing?

  tidyr::pivot_wider(
    prep,
    names_from = "bcgwcat_param",
    values_from = "result_value"
  )
}

fmt_base <- function(samples) {
  check_samples(samples, check_fmt = FALSE)

  # Filtering and Fixing
  s <- samples |>
    dplyr::filter(
      # Only parameters of interest
      .data$Observed_Property_ID %in% index_params()$enmods_param
    ) |>
    fix_replicates() |>
    fix_ph()

  # Format and return only columns of interest
  s <- fmt_cols(s)

  s
}

fmt_plot <- function(
  samples,
  plot_type,
  group = NULL,
  valid_only = TRUE,
  omit_outliers = TRUE,
  with_alkalinity = TRUE
) {
  p <- dplyr::filter(ion_details(), .data[[plot_type]])

  ss <- check_samples(samples, check_fmt = TRUE) |>
    dplyr::filter(
      bcgwcat_param %in% c(p$bcgwcat_param, "charge_balance"),
      result_unit %in% c("meq/l", "%")
    ) |>
    fmt_wide(include_units = FALSE) |>
    dplyr::arrange(.data$location_id, .data$observed_date_time) |>
    dplyr::mutate(
      sample_id = paste0(.data$location_id, "-", seq_len(dplyr::n())),
      .by = "location_id"
    )

  # Ions not measured at all are missing (NA)

  for (ion in setdiff(p$bcgwcat_param, names(ss))) {
    ss[[ion]] <- NA_real_
  }

  # If no group use a placeholder
  if (!is.null(group)) {
    ss$group <- ss[[group]]
  } else {
    ss$group <- "group"
  }

  if (with_alkalinity) {
    # Use alkalinity if missing bicarbonate
    ss <- dplyr::mutate(
      ss,
      `bicarbonate_alkalinity_as_caco3` = dplyr::if_else(
        is.na(.data$`bicarbonate_alkalinity_as_caco3`),
        .data$`alkalinity_total_4.5_as_caco3`,
        .data$`bicarbonate_alkalinity_as_caco3`
      )
    )
  }

  # Select and rename
  ss <- ss |>
    dplyr::select(
      "location_id",
      "observed_date_time",
      "sample_id",
      "group",
      "charge_balance",
      "water_type",
      dplyr::any_of(rlang::set_names(p$bcgwcat_param, p$water_code))
    )

  if (valid_only) {
    # Only keep samples with valid charge balances of non-NA and -10 <= x <= 10
    ss <- dplyr::filter(
      ss,
      !is.na(.data$charge_balance),
      abs(.data$charge_balance) <= 10
    )
  }

  if (omit_outliers) {
    # Omit outliers by water type (not within the dominant set)
    ss <- ss |>
      tidyr::nest(.by = group) |>
      dplyr::mutate(data = purrr::map(.data$data, add_dominant_water_types)) |>
      tidyr::unnest("data") |>
      dplyr::filter(.data$dominant)
  }

  ss
}

fmt_stiff <- function(
  samples,
  group = NULL,
  valid_only = TRUE,
  with_alkalinity = TRUE
) {
  s <- samples |>
    tidyr::pivot_longer(
      cols = -c(
        "location_id",
        "observed_date_time",
        "sample_id",
        "group",
        "charge_balance",
        "water_type"
      ),
      names_to = "water_code",
      values_to = "result_value"
    )

  ion_positions <- c(
    "Ca" = 0,
    "Mg" = 1,
    "Na" = 2,
    "Cl" = 0,
    "HCO3" = 2,
    "SO4" = 1
  )

  s <- s |>
    dplyr::mutate(
      y = ion_positions[.data$water_code],
      result_value = dplyr::if_else(
        .data$water_code %in% c("Ca", "Mg", "Na"),
        -.data$result_value,
        .data$result_value
      ),
      water_code = factor(
        .data$water_code,
        levels = c("Ca", "Mg", "Na", "HCO3", "SO4", "Cl")
      ),
      sample = factor(paste(.data$sample_id, .data$y))
    ) |>
    dplyr::filter(!is.na(water_code)) |>
    dplyr::arrange(.data$sample_id, .data$water_code) |>
    # Remove samples with any NAs
    dplyr::filter(!anyNA(.data$result_value), .by = "sample_id")

  s
}

fmt_cols <- function(samples) {
  s <- samples |>
    dplyr::select(any_of(meta_vars("enmods", param_vars = TRUE))) |>
    dplyr::rename_with(tolower)

  # Add and arrange additional columns
  s <- s |>
    add_obs_wells() |>
    add_bcgwcat_params() |>
    dplyr::relocate(
      "obs_well",
      "location_id",
      "location_latitude",
      "location_longitude",
      "location_elevation",
      "observed_date_time",
      "medium"
    )

  s
}


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
    ) |>
    dplyr::select(-dplyr::any_of("location_name"))
}

add_units_to_col <- function(df, param_col, units_col, include_units) {
  if (include_units) {
    df <- dplyr::mutate(
      df,
      !!units_col := stringr::str_replace(.data[[units_col]], " ", "_"),
      !!param_col := glue("{.data[[param_col]]}_{tolower(.data[[units_col]])}")
    )
  } else {
    # Always add meq as units
    df <- dplyr::mutate(
      df,
      !!param_col := dplyr::if_else(
        .data[[units_col]] == "meq",
        glue("{.data[[param_col]]}_meq"),
        .data[[param_col]]
      )
    )
  }
  df
}

add_bcgwcat_params <- function(s) {
  dplyr::left_join(
    dplyr::select(s, -dplyr::any_of("bcgwcat_param")), # Remove previously added columns
    dplyr::select(index_params(), "enmods_param", "bcgwcat_param"),
    by = c("observed_property_id" = "enmods_param")
  ) |>
    dplyr::relocate("bcgwcat_param", .before = "observed_property_id")
}

add_meq <- function(samples) {
  dplyr::bind_rows(samples, meq(samples))
}

add_charge_balance <- function(samples) {
  samples |>
    add_placeholders() |>
    dplyr::filter(
      !bcgwcat_param %in% c("charge_balance", "anion_sum", "cation_sum")
    ) |>
    dplyr::bind_rows(charge_balance(samples))
}

add_water_type <- function(samples) {
  dplyr::left_join(samples, water_type(samples), by = meta_vars())
}


add_placeholders <- function(
  samples,
  add = c("charge_balance_enmods", "anion_sum_enmods", "cation_sum_enmods")
) {
  placeholders <- dplyr::tribble(
    ~bcgwcat_param          , ~result_unit ,
    "charge_balance_enmods" , "%"          ,
    "anion_sum_enmods"      , "meq/l"      ,
    "cation_sum_enmods"     , "meq/l"
  ) |>
    dplyr::mutate(result_value = NA) |>
    dplyr::filter(
      .data$bcgwcat_param %in%
        .env$add &
        !.data$bcgwcat_param %in% unique(samples$bcgwcat_param)
    )

  if (nrow(placeholders) > 0) {
    placeholders <- cbind(
      dplyr::select(samples[1, ], -dplyr::all_of(names(placeholders))),
      placeholders
    )
    samples <- dplyr::bind_rows(samples, placeholders)
  }

  samples
}
