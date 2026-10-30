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

#' Calculate MEQ values
#'
#' Calculates MEQ values for 'long' data in mg/L. Expects output of either
#' [fetch_renmods()] or [fmt_minimal()].
#'
#' For conversion details and a description of the data see [ion_details()].
#'
#' MEQs are calculated by dividing the parameter concentration in mg/L by the
#' conversion factor. "result_unit" is required to ensure the units are in mg/L
#' before calculating MEQs
#'
#' **Note: This is an internal function, exported for clarity in calculations**
#'
#' @param samples Data frame. Output from [fmt_minimal()] or [fetch_renmods()].
#' If [fetch_renmods()] samples will be first formatted with [fmt_minimal()].
#'
#' @noRd
#'
#' @examplesIf interactive()
#'
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#' fmt_base(s) |>
#'   meq() |>
#'   dplyr::select(bcgwcat_param, observed_property_id, result_value, result_unit)

meq <- function(samples) {
  # TODO: CHECK UNITS!

  # Values all in mg/L which is the standard reported by EMS

  meq_values <- samples |>
    dplyr::inner_join(
      dplyr::select(ion_details(), "bcgwcat_param", "conversion"),
      by = "bcgwcat_param",
      multiple = "all"
    ) |>
    dplyr::mutate(
      result_value = dplyr::if_else(
        .data$bcgwcat_param == "ph",
        10^(-.data$result_value) * 1000,
        .data$result_value / .data$conversion
      ),
      result_unit = "meq/l",
    ) |>
    dplyr::select(-"conversion")

  # if (drop_na) {
  #   meq_values <- tidyr::drop_na(meq_values)
  # }

  meq_values
}


#' Calculate charge balance
#'
#' Calculates charge balances based on ALS formula. **Note:** Original EMS
#' charge balances, anion sums and cation sums will be renamed to
#' `charge_balance_enmods`, `anion_sum_enmods`, `cation_sum_enmods`,
#' respectively, when these are added to samples data (see
#' [fmt_minimal()]).
#'
#' Potential changes in workflows over the years have made it difficult to
#' ascertain exactly how charge balances were calculated in older samples. This
#' resulted in discrepancies between EMS and locally calculated charge balances.
#' Therefore for consistency, we calculate charge balances for all samples using
#' the ALS formula below.
#'
#' One difference between this calculation and that of ALS, is that we use more
#' significant digits when calculating MEQ.
#'
#' anion sum = chloride_dissolved_meq/l + sulfate_dissolved_meq/l +
#'             fluoride_dissolved_meq/l + nitrate_no3_dissolved_meq/l +
#'             nitrogen_nitrite_dissolved_no2_meq/l +
#'             alkalinity_total_4.5_as_caco3_meq/l
#'
#' cation sum = calcium_dissolved_meq/l + magnesium_dissolved_meq/l + sodium_dissolved_meq/l +
#'              potassium_dissolved_meq/l + aluminum_dissolved_meq/l +
#'              copper_dissolved_meq/l + iron_dissolved_meq/l +
#'              manganese_dissolved_meq/l + zinc_dissolved_meq/l +
#'              nitrogen_ammonia_dissolved_meq/l +
#'              (10 ^ (-ph)) * 1000
#'
#' Charge balance = 100 x (Cation Sum - Anion sum) / (Cation Sum + Anion Sum)
#'
#' Missing values are ignored (ie. generally treated as 0). However, if all
#' values for cations or anions are missing the charge balance is `NA`.
#'
#' If there are multiple measures for the same observation (i.e. repeated
#' measures), we use the mean MEQ value for that parameter.
#'
#' @inheritParams common_docs
#'
#' @return Data frame with only cation and anion sums, and charge balance
#'   values.
#' @noRd
#'
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#' fmt_base(s) |>
#'   meq() |>
#'   charge_balance() |>
#'   dplyr::select(bcgwcat_param, result_value, result_unit)
#'
#' s <- fetch_renmods(
#'   ems_ids = "E102388",
#'   date_range = c("2026-01-01", "2027-01-01")
#' )
#' fmt_base(s) |>
#'   meq() |>
#'   charge_balance() |>
#'   dplyr::select(bcgwcat_param, result_value, result_unit)

charge_balance <- function(samples) {
  pre_calc <- samples |>
    # take mean of multiple measures
    dplyr::left_join(
      dplyr::select(
        ion_details(),
        "bcgwcat_param",
        "type",
        "charge_balance"
      ),
      by = "bcgwcat_param"
    ) |>
    dplyr::filter(.data$charge_balance, .data$result_unit == "meq/l") |>
    fix_multiple_measures(type = "meq") |>
    dplyr::summarize(
      sum = dplyr::if_else(
        all(is.na(.data$result_value)),
        NA_real_,
        sum(.data$result_value, na.rm = TRUE)
      ), # Ignore missing ions
      .by = c(meta_vars(), "type")
    ) |>
    dplyr::mutate(type = paste0(.data$type, "_sum")) |>
    tidyr::pivot_wider(names_from = "type", values_from = "sum")

  if (!"anion_sum" %in% names(pre_calc)) {
    pre_calc$anion_sum <- NA
  }
  if (!"cation_sum" %in% names(pre_calc)) {
    pre_calc$cation_sum <- NA
  }

  pre_calc |>
    dplyr::mutate(
      charge_balance = 100 *
        ((.data$cation_sum - .data$anion_sum) /
          (.data$cation_sum + .data$anion_sum)),
      anion_sum = round(.data$anion_sum, 2),
      cation_sum = round(.data$cation_sum, 2),
      charge_balance = round(.data$charge_balance, 1)
    ) |>
    tidyr::pivot_longer(
      cols = c("cation_sum", "anion_sum", "charge_balance"),
      names_to = "bcgwcat_param",
      values_to = "result_value"
    ) |>
    dplyr::mutate(
      result_unit = dplyr::if_else(
        bcgwcat_param %in% c("cation_sum", "anion_sum"),
        "meq/l",
        "%"
      )
    )
}


is_valid <- function(charge_balance) {
  !is.na(charge_balance) & abs(charge_balance) <= 10
}

#' Calculate water type
#'
#' Water type based on anions Cl, SO4, HCO3 and cations Ca, Mg, Na and K. Ions
#' are ranked by proportion MEQ, all greater than 10% are listed in descending
#' order of presence, cations first. Water type is only calculated for samples
#' with valid charge balances. If HCO3 (`bicarbonate_alkalinity_as_caco3_meq/l`)
#' is missing, we use Alkalinity (`alkalinity_total_4.5_as_caco3_meq/l`) as a
#' replacement (indicated by an '*' on HCO3 in the water type). Otherwise,
#' missing ions are ignored (i.e. treated as 0). The column `missing_ion`
#' indicates whether there is a problem with the water type, such that it is
#' missing a cation or anion (i.e. is all cations or all anions). Water type is
#' designed to compliment piper plots by using the same ions so they are
#' comparable.
#'
#' @param samples Data frame. Output from [fmt_minimal()] or [fetch_renmods()].
#' If [fetch_renmods()] samples will be first formatted with [fmt_minimal()].
#'
#' @return Data frame with added columns `water_type` and `missing_ion`.
#'
#' @noRd
#'
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = "E102388",
#'   date_range = c("2026-01-01", "2027-01-01")
#' )
#'
#' fmt_base(s) |>
#'   add_meq() |>
#'   add_charge_balance() |>
#'   water_type() |>
#'   dplyr::select(water_type)

water_type <- function(samples) {
  samples |>
    dplyr::full_join(ion_details(), by = "bcgwcat_param") |>
    dplyr::filter(
      (.data$water_type & result_unit == "meq/l") |
        .data$bcgwcat_param == "charge_balance"
    ) |>
    tidyr::nest(.by = meta_vars(), .key = "wt") |>
    dplyr::mutate(wt = purrr::map(.data$wt, water_type_single)) |>
    tidyr::unnest("wt")
}

#' Calculate water type for a single sample
#'
#' @param df Data frame containing sample, must have columns 'bcgwcat_param' and
#' 'result_value', as well as 'type' (i.e. anion/cation), and 'water_code'.
#'
#' @returns Data frame with two values, missing_ion and water_type
#'
#' @noRd

water_type_single <- function(df) {
  vals <- rlang::set_names(df$result_value, df$water_code)
  vals <- vals[!is.na(names(vals))]

  # Only if valid charge balance
  if (!is_valid(df$result_value[df$bcgwcat_param == "charge_balance"])) {
    return(data.frame(missing_ion = FALSE, water_type = NA))
  }

  if ("HCO3" %in% names(vals) && is.na(vals[["HCO3"]])) {
    drop <- "HCO3"
  } else {
    drop <- "HCO3*"
  }
  vals <- vals[names(vals) != drop]
  total <- if (all(is.na(vals))) NA_real_ else sum(vals, na.rm = TRUE) # Ignore missing ions

  df |>
    dplyr::filter(water_code %in% names(vals)) |>
    dplyr::mutate(
      total = .env$total,
      prop = round(.data$result_value / .data$total, 3)
    ) |>
    dplyr::filter(.data$prop >= 0.1) |>
    dplyr::arrange(
      dplyr::desc(.data$type),
      dplyr::desc(.data$prop)
    ) |>
    dplyr::summarize(
      missing_ion = !all(c("anion", "cation") %in% .data$type),
      water_type = paste0(.data$water_code, collapse = "-")
    )
}
