meq_conversion <- function() {
  read_data("meq_conversion.csv")
}

#' Helper to return metadata columns for different data
#'
#' @param which Which data to fetch metadata columns for?
#'
#' @returns
#'
#' @export
#' @examples
#' meta_vars()
#' meta_vars("enmods")
#' meta_vars(param_vars = TRUE)

meta_vars <- function(which = "bcgwcat", param_vars = FALSE) {
  v <- index_meta()[[paste0(which, "_col")]]

  if (param_vars) {
    v <- c(v, index_meta_params()[[paste0(which, "_col")]])
  }

  v[!is.na(v)]
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_meta <- function() {
  read_data("index_meta.csv")
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_params <- function() {
  read_data("index_params.csv")
}

#' Title
#'
#' @returns
#'
#' @export
#' @examples
index_meta_params <- function() {
  read_data("index_meta_params.csv")
}

read_data <- function(file) {
  readr::read_csv(
    system.file(
      "extdata",
      file,
      package = "bcgwcat2"
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
}
