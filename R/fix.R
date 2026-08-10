fix_multiple_measures <- function(
  s,
  method = c("first", "min", "max"),
  by = "analysis_method",
  name = "measurement types"
) {
  method <- match.arg(method)

  # Find multiple measures
  # (i.e. same location, same date, same parameter, and 'by')
  obs <- c(meta_vars(), "bcgwcat_param")
  if (!is.null(by)) {
    obs <- c(obs, by)
  }

  # Only look for non-meq duplicates
  no_meq <- s |>
    dplyr::filter(result_unit != "meq/l") |>
    dplyr::select(dplyr::all_of(obs))

  keys <- purrr::pmap_chr(no_meq[obs], paste)
  n <- table(keys)
  multi <- no_meq[keys %in% names(n)[n > 1], ]

  if (is.null(by)) {
    multi <- dplyr::summarize(multi, n_duplicates = dplyr::n(), .by = obs)
  } else if (by == "analysis_method") {
    multi <- dplyr::summarize(
      multi,
      analysis_methods = glue_collapse(unique(.data$analysis_method), "; "),
      .by = obs
    )
  }
  multi <- dplyr::arrange(multi, dplyr::pick(dplyr::all_of(obs)))

  if (nrow(multi) > 0) {
    inform(
      "Multiple {name} for a single observation point, keeping {method} value",
      df = multi
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
