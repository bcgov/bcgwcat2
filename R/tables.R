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

#' Reactable table for data overview
#'
#' @param which Character. Which columns to include "rel" for relevant,
#'   otherwise all are included.
#'
#' @returns Reactable data table
#'
#' @export
#' @examples
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#' tbl_water_quality(s)
#' tbl_water_quality(s, "problems")
#' tbl_water_quality(s, "no_missing")
#'
#' s <- fetch_renmods(
#'   ems_ids = c("E298733"),
#'   date_range = c("2026-01-01", "2026-06-01")
#' )
#' tbl_water_quality(s)
#' water_quality(s)

tbl_water_quality <- function(samples, which = NULL, params = NULL) {
  wq <- water_quality(samples) |>
    dplyr::mutate(
      observed_date_time = format(observed_date_time, "%Y-%m-%d %H:%M:%S %Z")
    )

  if (!is.null(params)) {
    wq <- dplyr::filter(wq, .data$bcgwcat_param %in% .env$params)
  }

  if (!is.null(which)) {
    wq <- if (which == "problems") {
      dplyr::filter(wq, .data$water_quality_problem)
    } else if (which == "no_missing") {
      dplyr::filter(wq, !is.na(.data$water_quality_problem))
    }
  }

  wq |>
    dplyr::select(
      "obs_well",
      "location_id",
      "observed_date_time",
      "bcgwcat_param",
      "result_value",
      "result_unit",
      "limit",
      "limit_unit",
      "limit_note",
      "water_quality_problem"
    ) |>
    dplyr::arrange(
      .data$obs_well,
      .data$location_id,
      .data$observed_date_time
    ) |>
    reactable::reactable(
      showPageSizeOptions = TRUE,
      defaultPageSize = 20,
      searchable = TRUE,
      defaultColDef = reactable::colDef(
        header = pretty_names
      ),
      columns = list(
        result_value = reactable::colDef(
          format = reactable::colFormat(digits = 2)
        ),
        water_quality_problem = reactable::colDef(show = FALSE)
      ),
      rowStyle = \(i) {
        col <- if (is.na(wq$water_quality_problem[i])) {
          "white"
        } else if (wq$water_quality_problem[i]) {
          "#f8d7da"
        } else {
          "#d4edda"
        }
        list(background = col)
      }
    )
}
