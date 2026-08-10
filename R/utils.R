#' Make column names pretty
#'
#' @param x Character vector of names to make pretty.
#' @param units Whether or not these include units
#'
#' @returns
#'
#' @noRd
#' @examples
#' pretty_names("observed_date_time")
#' pretty_names(c("observed_date_time", "Cation sum meq/l"))

pretty_names <- function(x) {
  stringr::str_replace_all(x, c("-|_" = " ")) |>
    stringr::str_to_title() |>
    stringr::str_replace_all(c(
      "(m|M)eq\\/(l|L)" = "(mEq/L)",
      "(m|M)g\\/(l|L)" = "(mg/L)"
    )) |>
    stringr::str_replace_all(c("enmods" = "EnMoDS", "(I|i)d" = "ID"))
}

pretty_renmods <- function(x) {
  dplyr::recode_values(
    x,
    "this_yr" ~ "This year",
    "yr_2_5" ~ "Last 2-5 years",
    "yr_5_10" ~ "Last 5-10 years",
    "historic" ~ "Historic data"
  )
}

units_convert <- function(x, from, to) {
  dplyr::case_when(
    from == "mg/L" & to == "\u00B5g/L" ~ x * 1000,
    from == "mg/L" & to == "ug/L" ~ x * 1000,
    from == "\u00B5g/L" & to == "mg/L" ~ x / 1000,
    from == "ug/L" & to == "mg/L" ~ x / 1000,
    from == to | (is.na(from) & is.na(to)) | from == "meq/L" ~ x,
    TRUE ~ NA_real_
  )
}
