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
    dplyr::select(
      dplyr::all_of(cols_meta),
      dplyr::matches("charge_balance_%|anion_sum_meq/l|cation_sum_meq/l"),
      dplyr::everything()
    ) |>
    dplyr::arrange(
      .data$obs_well,
      .data$location_id,
      .data$observed_date_time
    )

  DT::datatable(
    d,
    options = list(pageLength = 20, scrollX = TRUE),
    rownames = FALSE,
    class = "nowrap"
  ) |>
    DT::formatRound(columns = cols_round) |>
    DT::formatStyle(
      columns = "charge_balance_%",
      backgroundColor = DT::styleInterval(
        cuts = c(-10.0001, 10),
        values = c("#f8d7da", "#d4edda", "#f8d7da")
      )
    )
}
