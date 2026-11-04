tbl_dates <- function(data_status) {
  dates <- data_dates()
  years <- lubridate::year(names(dates)) |> unique() |> as.character()
  g <- purrr::map(years, \(y) {
    reactable::colGroup(
      name = y,
      columns = stringr::str_subset(names(dates), y)
    )
  })

  reactable::reactable(
    dates,
    class = "hidden",
    width = "100%",
    defaultColDef = reactable::colDef(
      cell = \(v) {
        tags$abbr(
          style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
          title = pretty_names(v),
          ""
        )
      },
      style = function(value) {
        color <- dplyr::recode_values(
          value,
          "okay" ~ "green",
          "out_of_date" ~ "orange",
          "missing" ~ "red"
        )
        list(background = color)
      },
      minWidth = 5
    ),
    columnGroups = g,
    bordered = TRUE
  )
}

data_dates <- function() {
  dt <- char_to_range(renmods::cache_status()$date_range)
  min_date <- Sys.Date() - lubridate::years(15)

  renmods::cache_status() |>
    dplyr::mutate(
      start = lubridate::ymd(stringr::str_extract(date_range, "^[\\d-]+")),
      end = lubridate::ymd(stringr::str_extract(date_range, "[\\d-]+$")),
      start = dplyr::if_else(is.na(start), dplyr::lead(end), start),
      end = dplyr::if_else(is.na(end), dplyr::lag(start), end),
      start = dplyr::if_else(
        type == "historic" & is.na(.data$start),
        min_date,
        start
      ),
      status = dplyr::case_when(
        is.na(date_range) ~ "missing",
        difftime(Sys.time(), .data$last_downloaded, units = "weeks") >
          4 ~ "out_of_date",
        .default = "okay"
      ),
      dates = purrr::map2(.data$start, .data$end, \(d1, d2) {
        seq(d1, d2, by = "month")
      })
    ) |>
    tidyr::unnest(dates) |>
    dplyr::select(status, dates) |>
    dplyr::arrange(dates) |>
    tidyr::pivot_wider(names_from = "dates", values_from = "status")
}
