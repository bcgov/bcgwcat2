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
  x$data <- dplyr::mutate(x$data, x = .data$x + dx, y = .data$y + dy)
  x
}
