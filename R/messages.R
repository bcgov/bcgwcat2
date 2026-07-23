#' Title
#'
#' @param ...
#' @param .envir
#'
#' @returns
#'
#' @noRd
#' @examples
#' inform(c("This", "is", "a", "message"))

inform <- function(message, df = NULL, ..., .envir = rlang::caller_env()) {
  cli::cli_inform(message, ..., .envir = .envir)
  if (!is.null(df)) cli::cli_code(utils::capture.output(as.data.frame(df)))
}

#' Title
#'
#' @param ...
#' @param .envir
#' @param call
#'
#' @returns
#'
#' @noRd
#' @examples

abort <- function(..., .envir = rlang::caller_env(), call = NULL) {
  cli::cli_abort(..., call = call, .envir = .envir)
}
