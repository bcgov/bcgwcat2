fix_multiple_measures <- function(
  s,
  method = c("first", "min", "max", "mean"),
  type = c("reapeated_measure", "multi_measure", "meq")
) {
  # Find multiple measures
  # (i.e. same location, same date, same parameter, and 'by')

  obs <- c(meta_vars(), "bcgwcat_param")

  if (type == "multi_measure") {
    desc <- "Multiple measurement types"
    obs <- c(obs, "analysis_method")
    s_prep <- dplyr::filter(s, result_unit != "meq/l")
  } else if (type == "repeated_measure") {
    desc <- "Repeated measurements"
    s_prep <- dplyr::filter(s, result_unit != "meq/l")
  } else if (type == "meq") {
    desc <- "Multiple MEQ values"
    method <- "mean"
    s_prep <- dplyr::filter(s, result_unit == "meq/l")
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
