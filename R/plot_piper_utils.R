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

# Convert ternary (a, b, c) points to Cartesian (x, y)
abc_to_xy <- function(a, b, c) {
  s <- a + b + c
  x <- (0.5 * (2 * b + c)) / s
  y <- (sqrt(3) / 2 * c) / s
  data.frame(x = x, y = y)
}

# Convert diamond points (p, q) to Cartesian (x, y)
pq_to_xy <- function(p, q) {
  data.frame(x = (q - p) / 2, y = (p + q) * sqrt(3) / 2)
}

# all_pnts <- function(a, b, c, name) {
#   purrr::pmap(list(a = a, b = b, c = c), abc_to_xy) |>
#     purrr::list_rbind() |>
#     dplyr::mutate(group = name)
# }

#' Title
#'
#' @param breaks
#'
#' @returns
#'
#' @noRd
#' @examples
#' grid_ternary()
grid_ternary <- function(breaks = seq(0, 100, 20)) {
  props <- breaks / max(breaks)
  purrr::map(c("a", "b", "c"), \(i) grid_ternary_lines(props, i)) |>
    purrr::list_rbind()
}

grid_ternary_lines <- function(p, i) {
  purrr::map(
    rlang::set_names(p),
    \(w) {
      if (i == "c") {
        c <- w
        a <- seq(0, 1 - c, length.out = 2)
        b <- 1 - a - c
      } else if (i == "b") {
        b <- w
        a <- seq(0, 1 - b, length.out = 2)
        c <- 1 - a - b
      } else if (i == "a") {
        a <- w
        b <- seq(0, 1 - a, length.out = 2)
        c <- 1 - a - b
      }
      abc_to_xy(a, b, c)
    }
  ) |>
    purrr::list_rbind(names_to = "id") |>
    dplyr::mutate(axis = i, group = paste0(i, "_", id))
}

#' Title
#'
#' @param breaks
#'
#' @returns
#'
#' @noRd
#' @examples
#' grid_diamond()
grid_diamond <- function(breaks = seq(0, 100, 20)) {
  props <- breaks / max(breaks)
  p_lines <- purrr::map(
    rlang::set_names(props),
    \(w) pq_to_xy(p = c(w, w), q = c(0, 1))
  ) |>
    purrr::list_rbind(names_to = "id") |>
    dplyr::mutate(axis = "p", group = paste0("p_", .data$id))

  q_lines <- purrr::map(
    rlang::set_names(props),
    \(w) pq_to_xy(p = c(0, 1), q = c(w, w))
  ) |>
    purrr::list_rbind(names_to = "id") |>
    dplyr::mutate(axis = "q", group = paste0("q_", .data$id))

  dplyr::bind_rows(p_lines, q_lines)
}


shift_xy <- function(x, dx = 0, dy = 0) {
  if (!is.null(x$data)) {
    x$data <- dplyr::mutate(x$data, x = .data$x + dx, y = .data$y + dy)
  }
  x
}

check_point_scale <- function(d, type = "point_colour", scale = NULL) {
  g <- dplyr::n_distinct(d$group)

  if (!is.null(scale)) {
    if (length(scale) == 1) {
      scale <- rep(scale, g)
    } else if (length(scale) != g) {
      abort(
        "The length of {.var {{type}}} doesn't match the number of categories in {.var group}"
      )
    }
  }
  scale
}

check_point_colour <- function(d, point_colour) {
  g <- dplyr::n_distinct(d$group)

  if (is.null(point_colour)) {
    point_colour <- rep("black", g)
  } else if (length(point_colour) == 1 && point_colour == "viridis") {
    point_colour <- viridisLite::viridis(
      n = g,
      end = 0.8
    )
  }

  point_colour <- check_point_scale(d, scale = point_colour)

  point_colour
}
