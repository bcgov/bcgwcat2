fix_multiple_measures <- function(
  s,
  method = c("first", "min", "max")
) {
  method <- match.arg(method)

  # Find multiple measures
  # (i.e. same location, same date, same parameter, but different method)
  obs <- c(meta_vars(), "bcgwcat_param")
  no_meq <- s |>
    dplyr::select(dplyr::all_of(obs), "analysis_method") |>
    dplyr::distinct()

  keys <- purrr::pmap_chr(no_meq[obs], paste)
  n <- table(keys)
  multi <- no_meq[keys %in% names(n)[n > 1], ] |>
    dplyr::summarize(
      analysis_methods = glue_collapse(unique(.data$analysis_method), "; "),
      .by = obs
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(obs))) |>
    dplyr::select(dplyr::all_of(obs), "analysis_methods")

  if (nrow(multi) > 0) {
    inform(
      "Multiple measures for a single observation detected, keeping {method} value",
      df = multi
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
