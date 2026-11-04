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

#' Create a Piper Plot
#'
#' @param group Character. Column by which to group data for colour, shape,
#'   filled and size.
#' @param legend_position Character or Numeric. Location of legend passed to
#'   `legend.position` argument of [ggplot2::theme()]. One of none", "left",
#'   "right", "bottom", "top", "inside", OR a vector of two numeric values x,
#'   and y to specify an exact position.
#' @param legend_title Character. Title of legend. Defaults to `group`.
#' @param qc_valid Logical. Keep only valid data (charge balances <=10)
#' @param qc_omit_outliers Logical. Whether to omit outliers by looking at dominant
#'   water types (see `?dominant_water_types()`). Default TRUE.
#' @param qc_with_alkalinity Logical. Whether to use Alkalinity
#'   (`alkalinity_total_4.5_as_caco3_meq/l`) for HCO3
#'   (`bicarbonate_alkalinity_as_caco3_meq/l`) if HCO3 is missing. Default TRUE.
#'   Matches behaviour of `water_type()`.
#' @param point_colour Character. Colour or colours by which to colour points.
#'   Either a single value (applied to all), or a vector of values the same
#'   length as the number of `groups`. Can also be "viridis", which will use the
#'   viridis colour scale.
#' @param point_shape Numeric. Shape of points to use, 1 - 25, see
#'   ?[ggplot2::scale_shape()]. Either a single value (applied to all), or a
#'   vector of values the same length as the number of `groups`.
#' @param margin Numeric. Plot margin size in "pt". See [ggplot2::margin()].
#' @param scale Numeric. Scale text (axes titles and tick labels).
#'
#' @inheritParams common_docs
#'
#' @returns ggplot Piper Plot
#'
#' @export
#' @examplesIf interactive()
#' s <- fetch_renmods(ems_ids = "E208101", date_range = c("2001-01-01", "2015-01-01"))
#' piper_plot(s)
#'
#' ss <- fmt_minimal(s)
#' piper_plot(ss)

piper_plot <- function(
  samples,
  group = "location_id",
  legend_position = c(0.9, 0.8),
  legend_title = group,
  qc_valid_only = TRUE,
  qc_omit_outliers = TRUE,
  qc_with_alkalinity = TRUE,
  point_colour = "viridis",
  point_shape = 16,
  point_size = 3,
  margin = 10,
  scale = 1
) {
  s <- fmt_piper(
    samples,
    group,
    valid_only = qc_valid_only,
    omit_outliers = qc_omit_outliers,
    with_alkalinity = qc_with_alkalinity
  )

  gap <- 0.4

  # Themes and Scales
  legend_position <- if (all(s$group == "group")) "none" else legend_position

  t1 <- scale_piper_points(
    s,
    legend_title,
    point_colour,
    point_shape,
    point_size * scale
  )
  t2 <- theme_piper(legend_position, margin = margin * scale)

  # Grids and points
  g1 <- piper_cation(s, scale = scale)
  g2 <- piper_anion(s, scale = scale, dx = 1 + gap)
  g3 <- piper_combined(
    s,
    scale = scale,
    dx = 1 + gap / 2,
    dy = gap * sqrt(3) / 2
  )

  # Note: Vertical position of the combined diamond plot (sqrt(3) / 2) comes
  # from equilateral triangle hight calculation from the triangle created by the
  # gaps between the three components.

  ggplot2::ggplot() + g1 + g2 + g3 + t1 + t2
}

piper_cation <- function(
  samples,
  scale = 1,
  dx = 0,
  dy = 0
) {
  cat <- dplyr::select(
    samples,
    "location_id",
    "group",
    "observed_date_time",
    "Ca",
    "Mg",
    "Na_K"
  ) |>
    dplyr::mutate(across(c("Ca", "Mg", "Na_K"), \(x) {
      x / (.data$Ca + .data$Mg + .data$Na_K)
    }))

  cat <- dplyr::bind_cols(cat, abc_to_xy(cat$Ca, cat$Na_K, cat$Mg))

  piper_ternary(
    cat,
    axes = c("Mg", "Na + K", "Ca"),
    scale = scale,
    dx = dx,
    dy = dy
  ) # A-C, C-B, B-A
}

piper_anion <- function(
  samples,
  scale = 1,
  dx = 0,
  dy = 0
) {
  an <- dplyr::select(
    samples,
    "location_id",
    "group",
    "observed_date_time",
    "SO4",
    "Cl",
    "HCO3_CO3"
  ) |>
    dplyr::mutate(across(c("SO4", "Cl", "HCO3_CO3"), \(x) {
      x / (.data$SO4 + .data$Cl + .data$HCO3_CO3)
    }))
  an <- dplyr::bind_cols(an, abc_to_xy(an$HCO3_CO3, an$Cl, an$SO4))

  piper_ternary(
    an,
    axes = c("HCO3 + CO3", "SO4", "Cl"),
    reverse = TRUE,
    scale = scale,
    dx = dx,
    dy = dy
  ) # C-A, B-C, A-B
}

piper_combined <- function(
  samples,
  scale = 1,
  dx = 0,
  dy = 0
) {
  all <- dplyr::select(
    samples,
    "location_id",
    "group",
    "observed_date_time",
    "Ca",
    "Mg",
    "Na_K",
    "SO4",
    "Cl",
    "HCO3_CO3"
  ) |>
    dplyr::mutate(
      Ca_Mg = (.data$Ca + .data$Mg) / (.data$Ca + .data$Mg + .data$Na_K),
      SO4_Cl = (.data$SO4 + .data$Cl) / (.data$SO4 + .data$Cl + .data$HCO3_CO3)
    )
  all <- dplyr::bind_cols(all, pq_to_xy(all$Ca_Mg, all$SO4_Cl))

  piper_diamond(
    all,
    axes = c("Cl + SO4", "Ca + Mg"),
    scale = scale,
    dx = dx,
    dy = dy
  )
}


piper_ternary <- function(
  ions,
  axes,
  reverse = FALSE,
  scale = 1,
  dx = 0,
  dy = 0
) {
  # Axis labels at triangle corners
  #     C
  #    / \
  # c /   \ b
  #  /     \
  # A-------B
  #     a
  # a = scale on the bottom (corner is lower left) B - A
  # b = scale on the right (corner is lower right) C - B
  # c = scale on the left (corner is upper right)  A - C
  #
  # If reverse = TRUE
  # a = scale on the bottom (corner is lower RIGHT) A - B
  # b = scale on the right (corner is lower LEFT)   B - C
  # c = scale on the left (corner is LOWER right)   C - A

  # Axes Titles
  axes_titles <- dplyr::bind_cols(
    abc_to_xy(a = c(0.5, 0, 0.5), b = c(0, 0.5, 0.5), c = c(0.5, 0.5, 0)),
    name = axes, # left-side, right-side, bottom
    angle = c(60, -60, 0),
    vjust = c(-2.5, -2.5, 2.5)
  )

  a_labs <- grid_ternary() |>
    dplyr::filter(.data$axis == "a", .data$y == 0) |>
    dplyr::mutate(
      lab = stringr::str_extract(.data$group, "\\d+(\\.\\d+)?") |> as.numeric(),
      lab = format(.data$lab * 100, digits = 2)
    ) |>
    dplyr::distinct()

  b_labs <- grid_ternary() |>
    dplyr::filter(.data$axis == "b") |>
    dplyr::slice(seq(1, 12, by = 2)) |>
    dplyr::mutate(
      lab = stringr::str_extract(.data$group, "\\d+(\\.\\d+)?") |> as.numeric(),
      lab = format(.data$lab * 100, digits = 2)
    ) |>
    dplyr::distinct()

  c_labs <- grid_ternary() |>
    dplyr::filter(.data$axis == "c") |>
    dplyr::slice(seq(2, 12, by = 2)) |>
    dplyr::mutate(
      lab = stringr::str_extract(.data$group, "\\d+(\\.\\d+)?") |> as.numeric(),
      lab = format(.data$lab * 100, digits = 2)
    ) |>
    dplyr::distinct()

  a <- list(angle = -60, hjust = 0, vjust = 0.5)
  b <- list(angle = 60, hjust = 0, vjust = 1)
  c <- list(angle = 0, hjust = 1, vjust = 0)

  if (reverse) {
    a_labs$lab <- rev(a_labs$lab)
    b_labs$lab <- rev(b_labs$lab)
    c_labs$lab <- rev(c_labs$lab)
    a <- list(angle = 60, hjust = 1.25, vjust = 0.5)
    b <- list(angle = 0, hjust = 0, vjust = 0)
    c <- list(angle = -60, hjust = 1.25, vjust = 0.5)
  }

  layers_lst <- list(
    geom_piper_grid(grid_ternary()),
    geom_piper_text(a_labs, scale = scale, a$angle, a$hjust, a$vjust),
    geom_piper_text(b_labs, scale = scale, b$angle, b$hjust, b$vjust),
    geom_piper_text(c_labs, scale = scale, c$angle, c$hjust, c$vjust),
    geom_piper_labels(axes_titles, scale = scale),
    geom_piper_points(ions, scale = scale)
  )

  purrr::map(layers_lst, \(x) shift_xy(x, dx, dy))
}


piper_diamond <- function(
  ions,
  axes,
  scale = 1,
  dx = 0,
  dy = 0
) {
  # Axis labels at Diamond corners
  #
  #    / \
  # p /   \ q
  #  /     \
  #  \     /
  #   \   /
  #    \ /
  #

  # Axis Titles at the middle of the upper edges (lower edges labelled by triangular plots)
  axes_titles <- dplyr::bind_cols(
    pq_to_xy(p = c(1, 0.5), q = c(0.5, 1)),
    name = axes,
    angle = c(60, -60),
    vjust = -2.5
  )

  # Tick labels along *all* edges
  labs <- seq(0, 1, 0.2)
  p_labs <- dplyr::bind_cols(pq_to_xy(p = labs, q = 1), lab = labs * 100)
  q_labs <- dplyr::bind_cols(pq_to_xy(p = 1, q = labs), lab = labs * 100)

  lower_p_labs <- dplyr::bind_cols(
    pq_to_xy(p = labs, q = 0),
    lab = (1 - labs) * 100
  )
  lower_q_labs <- dplyr::bind_cols(
    pq_to_xy(p = 0, q = labs),
    lab = (1 - labs) * 100
  )

  layers_lst <- list(
    geom_piper_grid(grid_diamond()),
    geom_piper_text(
      p_labs,
      scale = scale,
      angle = 60,
      vjust = 0.5,
      hjust = -0.5
    ),
    geom_piper_text(
      q_labs,
      scale = scale,
      angle = -60,
      vjust = 0.5,
      hjust = 1.5
    ),
    geom_piper_text(
      lower_p_labs,
      scale = scale,
      angle = 60,
      vjust = 0.5,
      hjust = 1.5
    ),
    geom_piper_text(
      lower_q_labs,
      scale = scale,
      angle = -60,
      vjust = 0.5,
      hjust = -0.5
    ),
    geom_piper_labels(axes_titles, scale = scale),
    geom_piper_points(ions, scale = scale)
  )

  purrr::map(layers_lst, \(x) shift_xy(x, dx, dy))
}

geom_piper_grid <- function(grid) {
  ggplot2::geom_line(
    data = grid,
    ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
    color = "grey70",
    linewidth = 0.3
  )
}

geom_piper_text <- function(l, scale = 1, angle = 0, hjust = 0, vjust = 0) {
  ggplot2::geom_text(
    data = l,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab),
    size = 4 * scale,
    hjust = hjust,
    vjust = vjust,
    angle = angle
  )
}

geom_piper_labels <- function(labels, scale = 1, fontface = "bold") {
  ggplot2::geom_text(
    data = labels,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      label = .data$name,
      angle = .data$angle,
      vjust = .data$vjust
    ),
    size = 5 * scale,
    fontface = fontface
  )
}

geom_piper_points <- function(ions, scale = 1) {
  ggplot2::geom_point(
    data = ions,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      colour = .data$group,
      fill = .data$group,
      shape = .data$group,
      size = .data$group
    )
  )
}

scale_piper_points <- function(
  ions,
  legend_title,
  point_colour = "black",
  point_shape = 16,
  point_size = 3
) {
  point_colour <- check_point_colour(ions, point_colour)
  point_shape <- check_point_scale(ions, "point_shape", point_shape)
  point_size <- check_point_scale(ions, "point_size", point_size)

  list(
    ggplot2::scale_colour_manual(
      name = legend_title,
      values = point_colour,
      aesthetics = c("colour", "fill")
    ),
    ggplot2::scale_shape_manual(
      name = legend_title,
      values = point_shape
    ),
    ggplot2::scale_size_manual(
      name = legend_title,
      values = point_size
    )
  )
}

theme_piper <- function(legend_position = "bottom", margin = 10) {
  list(
    ggplot2::coord_fixed(clip = "off"),
    ggplot2::theme_void(),
    ggplot2::theme(
      legend.position = legend_position,
      legend.margin = ggplot2::margin(5, 5, 15, 5),
      plot.margin = ggplot2::margin(margin, margin, margin, margin)
    )
  )
}
