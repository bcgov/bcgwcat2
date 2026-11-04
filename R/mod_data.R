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

#' Title
#'
#' @param id
#'
#' @returns Shiny module UI
#'
#' @export
#' @examplesIf interactive()
#' test_mod("data")

mod_data_ui <- function(id = "data") {
  ns <- NS(id)

  nav_panel(
    title = "Data",
    layout_sidebar(
      sidebar = sidebar(
        gap = "1em",
        selectizeInput(
          ns("ems_ids"),
          "EMS IDs",
          choices = NULL,
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        dateRangeInput(
          ns("date_range"),
          "Date range",
          start = lubridate::floor_date(Sys.Date(), unit = "year"),
          end = lubridate::ceiling_date(Sys.Date(), unit = "year") -
            lubridate::days(1),
          min = "1900-01-01",
          max = "2099-01-01"
        ),
        div(
          div(
            em("Quick date select"),
            style = "font-size:90%; padding-bottom:5px;"
          ),
          layout_column_wrap(
            gap = "2px",
            width = 1 / 4,
            style = css(grid_template_columns = "1fr 6fr 6fr 1fr"),
            span(),
            mini_button(ns("period_this_year"), label = "This year"),
            mini_button(ns("period_last_year"), label = "Last year"),
            span(),
            span(),
            mini_button(ns("period_5_years"), label = "Last 5 years"),
            mini_button(ns("period_all_years"), label = "All years"),
            span()
          )
        ),
        actionButton(ns("fetch_data"), "Collect data"),
        uiOutput(ns("messages")),
        radioButtons(
          ns("data_columns"),
          "Data to show",
          choices = list("Relevant Columns" = "rel", "All Columns" = "all")
        ),
        radioButtons(
          ns("data_download"),
          "Data format to download",
          choices = list(
            "Wide (matches data shown)" = "wide",
            "Long (includes all data)" = "long"
          )
        )
      ),
      navset_card_pill(
        nav_panel(
          title = "Data",
          card_body(
            gap = 0,
            strong("EnMoDS Water Chemistry Data"),
            "Note: ",
            tags$ul(
              tags$li(
                "Charge balances are highlighted as acceptable",
                "(absolute value < 10; ",
                span(
                  "green",
                  style = "background-color:#d4edda",
                  .noWS = "after"
                ),
                ")",
                "or not (absolute value > 10; ",
                span(
                  "red",
                  style = "background-color:#f8d7da",
                  .noWS = "after"
                ),
                ").",
              ),
              tags$li(
                "Parameter names are different from those in the EnMoDs data base.",
              ),
              tags$li(
                "Please see ",
                actionLink(ns("link_to_details"), "Details"),
                "for more specifics regarding how Charge Balances differ from those in the EMS data base and how bcgwcat parameter names correspond to EnMoDs and EMS data."
              )
            ),
            reactable::reactableOutput(ns("table_samples")),
            shinyjs::disabled(
              downloadButton(
                ns("download_csv"),
                label = "Download as CSV"
              )
            ),
          )
        ),
        nav_panel(
          title = "Messages",
          uiOutput(ns("console_messages"), container = pre)
        )
      )
    )
  )
}


mod_data_server <- function(updated, app_session, id = "data", ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Use Reactive Vals to pass among modules
    ems_ids <- reactiveVal(NULL)
    samples <- reactiveVal(NULL)
    samples_wide <- reactiveVal(NULL)

    # Navigation MOds -------------
    observe({
      nav_select("bcgwcat", selected = "Details", session = app_session)
    }) |>
      bindEvent(input$link_to_details)

    # Messages -----------------
    output$messages <- renderUI({
      ems_ids()
      invisible()
    })

    # UI --------------------------
    updateSelectizeInput(
      session,
      "ems_ids",
      choices = ems_ids_all,
      server = TRUE
    )

    purrr::map(
      c(
        "period_this_year",
        "period_last_year",
        "period_5_years",
        "period_all_years"
      ),
      \(i) {
        observe({
          # Default is "period_this_year"
          dt <- c(
            lubridate::floor_date(Sys.Date(), unit = "year"),
            lubridate::ceiling_date(Sys.Date(), unit = "year") -
              lubridate::days(1)
          )

          dt <- if (i == "period_last_year") {
            dt - lubridate::years(1)
          } else if (i == "period_5_years") {
            c(dt[1] - lubridate::years(4), dt[2])
          } else if (i == "period_all_years") {
            lubridate::as_date(c(
              "1900-01-01",
              "2099-01-01"
            ))
          } else {
            dt
          }

          updateDateRangeInput(
            session,
            "date_range",
            start = dt[1],
            end = dt[2]
          )
        }) |>
          bindEvent(input[[i]])
      }
    )

    # Check EMS IDs ----------------
    observe({
      req(input$ems_ids)
      ids <- input$ems_ids
      # Check ID formatting
      len <- nchar(ids) %in% c(7, 8)
      sym <- stringr::str_detect(ids, "^[a-zA-Z0-9]*$")
      problems <- glue_collapse(ids[!len | !sym], sep = ", ")
      # TODO: Check that this is working
      validate(need(
        all(len) & all(sym),
        message = glue("Invalid EMS ID(s): {problems}")
      ))
      ems_ids(ids)
    })

    date_range <- reactive(input$date_range)

    # samples() ------------------------------
    observe({
      req(ems_ids())
      # Let user know what's going on
      id <- showNotification(
        "Collecting data from local EnMoDS database...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # Messages to console
      with_messages({
        s <- fetch_renmods(ems_ids(), date_range()) |> # Fetch data
          fmt_minimal() # Format
      })

      samples(s)
      samples_wide(samples())
    }) |>
      bindEvent(input$fetch_data)

    observe({
      shinyjs::toggleState("download_csv", nrow(samples()) > 0)
    }) |>
      bindEvent(samples())

    # Table ---------------------
    output$table_samples <- reactable::renderReactable({
      updated() # refresh if data is updated
      data_dts <- check_date_range(input$date_range)
      shinyjs::toggleState("fetch_data", data_dts)
      validate(
        need(
          data_dts,
          "Missing data for this data range. Go to the status tab to download the required data set."
        )
      )

      validate(need(ems_ids(), "Choose EMS Ids to fetch data for"))
      validate(need(samples(), "Click 'Collect Data'"))
      validate(need(nrow(samples()) > 0, "No data to display"))
      req(input$data_columns)

      # Create table
      with_messages(tbl_overview(samples_wide(), which = input$data_columns))
    })

    # Download ------------------
    output$download_csv <- downloadHandler(
      filename = function() {
        req(samples())
        suffix <- dplyr::if_else(input$data_download == "long", "long", "wide")
        # Use the selected dataset as the suggested file name
        paste0(
          paste0(input$ems_ids, collapse = "-"),
          "_",
          suffix,
          "_",
          Sys.Date(),
          ".csv"
        )
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        if (input$data_download == "long") {
          readr::write_csv(samples(), file)
        } else if (input$data_download == "wide") {
          readr::write_csv(samples_wide(), file)
        }
      }
    )

    # Returns -------------------------------------------
    list("samples" = samples, "ems_ids" = ems_ids)
  })
}


#' Check if a date range has
#'
#' @param date_ranges
#'
#' @returns Logical TRUE if check passes
#'
#' @noRd
#' @examples
#' check_date_range(c("1990-01-01", "2020-01-01"))
#' check_date_range(c("2022-01-01", "2026-01-01"))
#' check_date_range(c("1900-01-01", "2099-01-01"))

check_date_range <- function(date_range) {
  if (date_range[1] < min_date) {
    date_range[1] <- as.character(lubridate::as_date(min_date))
  }
  if (date_range[2] > Sys.Date()) {
    date_range[2] <- as.character(lubridate::as_date(Sys.Date()))
  }

  date_range <- lubridate::interval(date_range[1], date_range[2])
  data_dts <- renmods::cache_status()$date_range |>
    char_to_range() |>
    purrr::map(\(i) lubridate::interval(start = i[1], end = i[2]))

  i <- purrr::reduce(data_dts, \(x1, x2) {
    if (length(x1) > 1) {
      x0 <- x1[2]
    } else {
      x0 <- x1
    }
    if (
      lubridate::int_end(x2) + lubridate::days(1) == lubridate::int_start(x0)
    ) {
      i <- lubridate::interval(
        start = lubridate::int_start(x2),
        end = lubridate::int_end(x0)
      )
      if (length(x1) > 1) {
        i <- append(x1[1], i)
      }
    } else {
      i <- append(x1, x2)
    }

    i
  })

  purrr::map_lgl(i, \(ii) lubridate::`%within%`(date_range, ii)) |>
    tidyr::replace_na(FALSE) |>
    any()
}


char_to_range <- function(date_range) {
  purrr::map(date_range, \(d) {
    if (is.na(d)) {
      dt <- c(NA, NA)
    } else {
      dt <- stringr::str_split_1(d, " to ")
      if (length(dt) == 1) {
        dt <- c(min_date, dt)
      }
      # If it's the "this_yr", push the end date to the end of the year
      if (lubridate::year(dt[2]) == lubridate::year(Sys.Date())) {
        dt[2] <- stringr::str_replace(dt[2], "\\d{2}-\\d{2}$", "12-31")
      }
    }
    lubridate::as_date(dt)
  })
}
