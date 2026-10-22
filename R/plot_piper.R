#' Create a Piper Plot
#'
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

piper_plot <- function(samples, margin = 10, scale = 1) {
  s <- fmt_piper(samples)

  gap <- 0.4
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

  ggplot2::ggplot() + g1 + g2 + g3 + theme_piper(margin = margin * scale)
}

piper_cation <- function(samples, scale = 1, dx = 0, dy = 0) {
  cat <- dplyr::select(
    samples,
    "location_id",
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

piper_anion <- function(samples, scale = 1, dx = 0, dy = 0) {
  an <- dplyr::select(
    samples,
    "location_id",
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

piper_combined <- function(samples, scale = 1, dx = 0, dy = 0) {
  all <- dplyr::select(
    samples,
    "location_id",
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
    geom_piper_points(ions)
  )

  purrr::map(layers_lst, \(x) shift_xy(x, dx, dy))
}


piper_diamond <- function(ions, axes, scale = 1, dx = 0, dy = 0) {
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
    geom_piper_points(ions)
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
    ggplot2::aes(x = .data$x, y = .data$y),
    size = 3 * scale
  )
}

theme_piper <- function(margin = 10) {
  list(
    ggplot2::coord_fixed(clip = "off"),
    ggplot2::theme_void(),
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(margin, margin, margin, margin)
    )
  )
}
