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

fix_ph <- function(s) {
  cols <- c("Location_ID", "Observed_Date_Time", "Result_Value")

  problems <- dplyr::filter(
    s,
    .data$Observed_Property_ID == "pH (acidity)",
    .data$Data_Classification == "FIELD_RESULT"
  )

  if (nrow(problems) > 0) {
    msg <- c(
      "!" = "Correcting {nrow(problems)} case(s) of `pH (acidity)` being recorded as a Field Result.",
      "*" = "Reclassifying as `pH-Field (acidity)`"
    )
    s <- s |>
      dplyr::mutate(
        Observed_Property_ID = dplyr::if_else(
          .data$Observed_Property_ID == "pH (acidity)" &
            .data$Data_Classification == "FIELD_RESULT",
          "pH-Field (acidity)",
          .data$Observed_Property_ID
        )
      )

    # Check if exact duplicate of recorded field result (pick the transformed result if so)
    dups <- s |>
      dplyr::filter(.data$Observed_Property_ID == "pH-Field (acidity)") |>
      dplyr::filter(dplyr::n() > 1, .by = cols) |>
      dplyr::filter(.data$Observed_Property_Name == "0004")

    # Remove exact dups created
    if (nrow(dups) > 0) {
      msg <- c(
        msg,
        "x" = "Reclassification created {nrow(dups)} duplicate(s), omit instead"
      )
      s <- dplyr::anti_join(s, dups, by = c(cols, "Observed_Property_Name"))
    }

    inform(msg)
  }
  s
}

fix_replicates <- function(s) {
  # Only "Normal" samples (not replicates, blanks, spikes, or ...
  # TODO: Do something with replicate samples?
  dplyr::filter(s, .data$QC_Type == "NORMAL")
}

fix_multiple_measures <- function(
  s,
  method = c("first", "min", "max", "mean"),
  type = c("repeated_measure", "multi_measure", "meq")
) {
  # Find multiple measures
  # (i.e. same location, same date, same parameter, and 'by')

  obs <- c(meta_vars(), "bcgwcat_param")

  if (type == "multi_measure") {
    desc <- "Multiple measurement types"
    obs <- c(obs, "analysis_method")
    s_prep <- dplyr::filter(s, .data$result_unit != "meq/l")
  } else if (type == "repeated_measure") {
    desc <- "Repeated measurements"
    s_prep <- dplyr::filter(s, .data$result_unit != "meq/l")
  } else if (type == "meq") {
    desc <- "Multiple MEQ values"
    s_prep <- dplyr::filter(s, .data$result_unit == "meq/l")
  } else {
    abort("`type` must be 'multi_measure' or 'repeated_measure' or 'meq'")
  }

  method <- match.arg(method)

  s_prep <- dplyr::select(s_prep, dplyr::all_of(obs))
  keys <- purrr::pmap_chr(s_prep[obs], paste)
  n <- table(keys)
  multi <- s_prep[keys %in% names(n)[n > 1], ]

  if (type == "multi_measure") {
    multi <- dplyr::summarize(
      multi,
      analysis_methods = glue_collapse(unique(.data$analysis_method), "; "),
      .by = obs
    )
  } else {
    multi <- dplyr::summarize(multi, n_duplicates = dplyr::n(), .by = obs)
  }

  multi <- dplyr::arrange(multi, dplyr::pick(dplyr::all_of(obs)))

  if (nrow(multi) > 0) {
    inform(
      c(
        "!" = "{desc} for single parameter observations ({nrow(multi)} occurrences), keeping {method} values.",
        "*" = "Parameters affected: {paste0(unique(multi$bcgwcat_param),  collapse = ', ')}"
      )
    )

    if (method == "first") {
      s <- dplyr::slice(s, 1, .by = obs)
    } else if (method == "min") {
      s <- dplyr::slice_min(s, order_by = "results_value", by = obs)
    } else if (method == "max") {
      s <- dplyr::slice_max(
        s,
        order_by = "results_value",
        by = obs
      )
    }
  }

  s
}
