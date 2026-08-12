#' Assess problems with water quality
#'
#' Adds water quality guideline limits to the data and adds a column
#' `water_quality_problem` which is `TRUE` if a water quality limit is exceeded,
#' `FALSE` if not. See [wq_std()] for more information on the water quality
#' guidelines.
#'
#' @param samples Data frame. Output from [fmt_minimal()] or [fetch_renmods()].
#' If [fetch_renmods()] samples will be first formatted with [fmt_minimal()].
#'
#' @return Tibble of sites, samples, parameters, water quality limits and problems.
#' @export
#'
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#' water_quality(s)

water_quality <- function(samples) {
  samples <- check_samples(samples)

  wq <- samples |>
    dplyr::filter(result_unit != "meq/l") |> # No meq params
    dplyr::select(dplyr::all_of(c(
      meta_vars(param_vars = TRUE),
      "bcgwcat_param"
    ))) |>
    dplyr::left_join(
      dplyr::select(
        wq_std(),
        "limit",
        "limit_unit" = "units",
        "limit_note" = "limitnotes",
        "bcgwcat_param"
      ),
      by = "bcgwcat_param",
      multiple = "all"
    ) |>
    dplyr::mutate(
      result_value_comp = units_convert(
        .data$result_value,
        from = .data$result_unit,
        to = .data$limit_unit
      ),
      water_quality_problem = .data$result_value > .data$limit
    )

  # Are there any other unconvertable units which don't match?
  uncomp <- dplyr::filter(
    wq,
    !.data$result_unit %in% c("meq/L", "mg/L", "\u00B5g/L", "ug/L")
  )
  if (
    nrow(uncomp) && any(uncomp$result_unit != uncomp$limit_unit, na.rm = TRUE)
  ) {
    abort(
      "Water quality units do not match EnMoDS parameter units.",
      "Please open an issue: https://github.com/bcgov/bcgwcat/"
    )
  }

  wq
}
