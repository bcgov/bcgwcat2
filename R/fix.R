fix_multiple_measures <- function(
  s,
  method = c("first", "min", "max")
) {
  method <- match.arg(method)

  # Find multiple measures
  # (i.e. same location, same date, same parameter, but different method)
  obs <- c("location_id", "observed_date_time", "observed_property_id")

  multi <- dplyr::summarize(
    s,
    analysis_methods = glue_collapse(unique(.data$analysis_method), "; "),
    n = dplyr::n(),
    .by = obs
  ) |>
    dplyr::filter(.data$n > 1)

  if (nrow(multi) > 0) {
    inform(
      "Multiple measures for a single observation detected, keeping {method} value",
      df = dplyr::select(multi, -"n")
    )

    if (method == "first") {
      s <- dplyr::slice(s, 1, .by = obs)
    }
    if (method == "min") {
      s <- dplyr::slice_min(s, order_by = "results_value", by = obs)
    }
    if (method == "max") {
      s <- dplyr::slice_max(
        s,
        order_by = "results_value",
        by = obs
      )
    }
  }
  s
}
