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

#' Create Stiff Plot
#'
#' @inheritParams piper_plot
#' @inheritParams common_docs
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
#'   date_range = c("1990-01-01", "2024-06-01")
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
#' s <- fetch_renmods(ems_ids = c("1401030", "1401377", "E292373"))
#'
#' s <- fetch_renmods(ems_ids = "1401377")
#' stiff_plot(s)
#'

stiff_plot <- function(
  samples,
  group = "location_id",
  colour = "viridis",
  legend_position = c(0.9, 0.8),
  legend_title = group,
  qc_valid_only = TRUE,
  qc_with_alkalinity = TRUE,
  margin = 10
) {
  # TODO: Use omit outliers here?

  s <- fmt_plot(
    samples,
    plot_type = "stiff_plot",
    group = group,
    valid_only = qc_valid_only,
    omit_outliers = FALSE, #omit_outliers = qc_omit_outliers,
    with_alkalinity = qc_with_alkalinity
  ) |>
    fmt_stiff()

  if (nrow(s) == 0) {
    if (shiny::isRunning()) {
      validate(need(nrow(s) > 0), "Missing too many samples")
    }
    inform(
      "Not enough {dplyr::if_else(qc_valid_only, 'good quality', '')} data."
    )
    return(invisible())
  }

  colour <- check_colour_scale(s, colour)
  legend_position <- if (all(s$group == "group")) "none" else legend_position

  g <- ggplot2::ggplot(
    s,
    ggplot2::aes(
      x = .data$result_value,
      y = .data$sample,
      group = .data$sample_id,
      fill = .data$group
    )
  ) +
    theme_stiff(legend_position, margin = margin) +
    ggplot2::geom_polygon(colour = "black") +
    ggplot2::geom_vline(xintercept = 0) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = .data$water_code),
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
    ggplot2::scale_fill_manual(name = legend_title, values = colour)

  if (!is.null(group)) {
    g <- g +
      ggplot2::facet_wrap(ggplot2::vars(.data$group), scales = "free_y")
  }
  g
}


theme_stiff <- function(legend_position = "bottom", margin = 10) {
  list(
    ggplot2::theme_bw() +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(
        legend.position = legend_position,
        legend.margin = ggplot2::margin(5, 5, 15, 5),
        plot.margin = ggplot2::margin(margin, margin, margin, margin)
      )
  )
}

check_colour_scale <- function(d, colour = NULL) {
  g <- dplyr::n_distinct(d$group)

  if (is.null(colour)) {
    colour <- rep("white", g)
  } else if (length(colour) == 1 && colour == "viridis") {
    colour <- viridisLite::viridis(
      n = g,
      end = 0.8
    )
  } else if (length(colour) == 1) {
    colour <- rep(colour, g)
  } else if (length(colour) != g) {
    abort(
      "The length of {.var colour} doesn't match the number of categories in {.var group}"
    )
  }

  colour
}
