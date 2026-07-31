#' Data table for ove
#'
#' @param samples
#' @param which
#'
#' @returns
#'
#' @export
#' @examples
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#' tbl_overview(s, "rel")
#'
#' s <- fetch_renmods(
#'   ems_ids = "E309359",
#'   date_range = c("1900-01-01", "2099-01-01")
#' )
#' tbl_overview(s, "rel")
#'
#'
#' s <- fetch_renmods(
#'   ems_ids = "E328305",
#'   date_range = c("2026-01-01", "2027-01-01")
#' )
#' tbl_overview(s, "rel")

tbl_overview <- function(samples, which) {
  d <- samples |>
    fmt_wide()

  cols_meta <- c(
    "obs_well",
    "location_id",
    "observed_date_time"
  )

  cols_params <- c(
    "calcium_dissolved",
    "magnesium_dissolved",
    "sodium_dissolved",
    "chloride_dissolved",
    "bicarbonate_alkalinity_as_caco3",
    "sulfate_dissolved",
    "cation_sum",
    "anion_sum",
    "charge_balance",
    "water_type"
  )

  cols_round <- stringr::str_subset(names(d), "meq|%")

  if (which == "rel") {
    d <- dplyr::select(
      d,
      dplyr::all_of(cols_meta),
      dplyr::matches(paste0(cols_params, collapse = "|"))
    )
    cols_round <- cols_round[cols_round %in% names(d)]
  }

  d <- d |>
    dplyr::mutate(
      observed_date_time = format(observed_date_time, "%Y-%m-%d %H:%M:%S %Z")
    ) |>
    dplyr::relocate(
      dplyr::all_of(cols_meta),
      dplyr::matches("charge_balance_%|anion_sum_meq/l|cation_sum_meq/l")
    ) |>
    dplyr::relocate("charge_balance_enmods_%", .after = dplyr::last_col()) |>
    dplyr::arrange(
      .data$obs_well,
      .data$location_id,
      .data$observed_date_time
    ) # |>
  #dplyr::rename_with(pretty_names)

  col_fmt <- purrr::map(cols_round, \(c) {
    reactable::colDef(format = reactable::colFormat(digits = 2))
  }) |>
    rlang::set_names(cols_round)

  col_fmt[["charge_balance_%"]] <- reactable::colDef(
    style = \(v) {
      if (is.na(v)) {
        col <- "white"
      } else if (v <= -10 | v >= 10) {
        col <- "#f8d7da"
      } else {
        col <- "#d4edda"
      }
      list(background = col)
    },
    format = reactable::colFormat(digits = 2)
  )

  reactable::reactable(
    d,
    showPageSizeOptions = TRUE,
    defaultPageSize = 20,
    searchable = TRUE,
    defaultColDef = reactable::colDef(
      header = pretty_names
    ),
    columns = col_fmt
  )
}
