pretty_names <- function(x) {
  stringr::str_replace_all(x, c("-|_" = " ")) |>
    stringr::str_to_sentence()
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
