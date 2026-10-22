#' Create Stiff Plot
#'
#' @param ems_id IDs to plot if dataset includes more than one
#' @param colour Whether to add colour by `ems_id`
#' @param legend Whether to show the legend
#' @param valid Logical. Keep only valid data (charge balances <=10)
#' @param with_Alk Logical. Whether to use `Meas_Alk_meq` for `HCO3_meq` if
#'   missing. Default TRUE. Matches behaviour of `water_type()`.
#'
#' @returns ggplot Stiff Plot
#'
#' @inheritParams common_docs
#'
#' @export
#'
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030"),
#'   date_range = c("1990-01-01", "2026-06-01")
#' )
#' stiff_plot(s)
#'
#' s <- fetch_renmods(
#'   ems_ids = c("E218582"),
#'   date_range = c("2026-01-01", "2026-06-01")
#' )
#' stiff_plot(s)
#'
#' s <- fetch_renmods(
#'   ems_ids = c("E298733"),
#'   date_range = c("2026-01-01", "2026-06-01")
#' )
#' stiff_plot(s)
#'

stiff_plot <- function(
  samples,
  ems_id = NULL,
  colour = TRUE,
  legend = TRUE,
  valid = TRUE,
  with_Alk = TRUE
) {
  samples <- check_samples(samples)

  if (!is.null(ems_id)) {
    if (length(ems_id) > 1 && !colour) {
      abort("Can only specify one ems_id at a time unless 'colour = TRUE'")
    }
    samples <- dplyr::filter(samples, .data$location_id %in% !!ems_id)
  } else if (length(unique(samples$location_id)) > 1 && !colour) {
    abort(
      "With more than one location_id (EMS ID) included in data, ",
      "need to specify which id OR 'colour = TRUE'"
    )
  }

  s_prep <- samples |>
    dplyr::filter(
      .data$bcgwcat_param %in%
        c(
          "charge_balance",
          "calcium_dissolved",
          "magnesium_dissolved",
          "sodium_dissolved",
          "chloride_dissolved",
          "bicarbonate_alkalinity_as_caco3", #HCO3
          "alkalinity_total_4.5_as_caco3", # Meas_alk
          "sulfate_dissolved",
          "charge_balance"
        ),
      .data$result_unit %in% c("meq/l", "%")
    ) |>
    dplyr::mutate(
      id = paste0(.data$location_id, .data$observed_date_time),
      id = as.numeric(factor(id)),
      .by = "location_id"
    ) |>
    dplyr::select(
      "location_id",
      "id",
      "bcgwcat_param",
      "result_value"
    ) |>
    dplyr::arrange(.data$location_id, .data$id)

  s_prep <- s_prep |>
    tidyr::pivot_wider(
      names_from = "bcgwcat_param",
      values_from = "result_value"
    )

  # Use Meas_Alk if missing HCO3
  if (
    with_Alk &&
      all(
        c(
          "bicarbonate_alkalinity_as_caco3",
          "alkalinity_total_4.5_as_caco3"
        ) %in%
          names(s_prep)
      )
  ) {
    s_prep <- dplyr::mutate(
      s_prep,
      bicarbonate_alkalinity_as_caco3 = dplyr::if_else(
        is.na(.data$bicarbonate_alkalinity_as_caco3),
        .data$alkalinity_total_4.5_as_caco3,
        .data$bicarbonate_alkalinity_as_caco3
      )
    )
  }

  # Keep only valid data if specified
  if (valid) {
    s_prep <- dplyr::filter(s_prep, is_valid(.data$charge_balance))
  }

  s_prep <- s_prep |>
    dplyr::select(-dplyr::any_of("bicarbonate_alkalinity_as_caco3")) |>
    tidyr::pivot_longer(
      cols = -c("location_id", "id", "charge_balance"),
      names_to = "bcgwcat_param",
      values_to = "result_value"
    )

  # Remove rows with any NAs
  s_prep <- dplyr::filter(
    s_prep,
    !anyNA(.data$result_value),
    .by = c("location_id", "id")
  )

  if (nrow(s_prep) == 0) {
    inform(
      paste(
        "Not enough ",
        dplyr::if_else(valid, "good quality ", ""),
        "data for this EMS ID"
      )
    )
    return(invisible())
  }

  stiff <- s_prep |>
    dplyr::mutate(
      y = dplyr::case_when(
        .data$bcgwcat_param == "calcium_dissolved" ~ 0,
        .data$bcgwcat_param == "magnesium_dissolved" ~ 1,
        .data$bcgwcat_param == "sodium_dissolved" ~ 2,
        .data$bcgwcat_param == "chloride_dissolved" ~ 0,
        .data$bcgwcat_param == "alkalinity_total_4.5_as_caco3" ~ 2,
        .data$bcgwcat_param == "sulfate_dissolved" ~ 1
      ),
      result_value = dplyr::if_else(
        .data$bcgwcat_param %in%
          c("calcium_dissolved", "magnesium_dissolved", "sodium_dissolved"),
        -.data$result_value,
        .data$result_value
      ),
      bcgwcat_param = factor(
        .data$bcgwcat_param,
        levels = c(
          "calcium_dissolved",
          "magnesium_dissolved",
          "sodium_dissolved",
          "alkalinity_total_4.5_as_caco3",
          "sulfate_dissolved",
          "chloride_dissolved"
        )
      )
    ) |>
    dplyr::arrange(.data$location_id, .data$id, .data$bcgwcat_param) |>
    dplyr::group_by(.data$location_id, .data$id) |>
    dplyr::mutate(
      sample = factor(paste(.data$id, .data$y)),
      n = sum(!is.na(.data$result_value)),
      element = stringr::str_remove(.data$bcgwcat_param, "_meq")
    ) |>
    dplyr::filter(.data$n == 6) |>
    dplyr::ungroup() |>
    dplyr::mutate(ems_id = location_id)

  if (colour) {
    fill <- "ems_id"
  } else {
    fill <- NULL
  }

  if (nrow(stiff) == 0) {
    if (shiny::isRunning()) {
      validate(need(nrow(stiff) > 0), "Missing too many samples")
    }
    inform("Missing too many samples")
    return(invisible())
  }

  ggplot2::ggplot(
    stiff,
    ggplot2::aes(
      x = .data$result_value,
      y = .data$sample,
      group = .data$id,
      fill = .data[[fill]]
    )
  ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
    ggplot2::geom_polygon(colour = "black", show.legend = legend) +
    ggplot2::geom_vline(xintercept = 0) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = .data$bcgwcat_param),
      fill = "white",
      min.segment.length = 0
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) stringr::str_remove(x, " [0-9]{1}$"),
      breaks = function(x) x[seq(2, by = 3, along.with = x)]
    ) +
    ggplot2::scale_x_continuous(limits = function(x) {
      c(-max(abs(x)), max(abs(x)))
    }) +
    ggplot2::labs(x = "Milliequivalents per litre") +
    ggplot2::facet_wrap(~ems_id, scales = "free_y") +
    ggplot2::scale_fill_viridis_d(name = "EMS ID", end = 0.8)
}
