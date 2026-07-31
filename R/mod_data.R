#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
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
          "Data to preview",
          choices = list("Relevant Columns" = "rel", "All Columns" = "all")
        )
      ),
      navset_card_pill(
        nav_panel(
          title = "Data Preview",
          card_body(
            gap = 0,
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
                "Parameter names, ",
                em("and occasionally units,"),
                "are different from those in the EMS data base.",
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


mod_data_server <- function(id = "data", app_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    updateSelectizeInput(session, "ems_ids", choices = ems_ids, server = TRUE)

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

    # Check IDs ----------------
    ems_ids <- reactive({
      req(input$ems_ids)

      ids <- input$ems_ids
      # Check ID formatting
      len <- nchar(ids) == 7
      sym <- stringr::str_detect(ids, "^[a-zA-Z0-9]*$")
      problems <- glue_collapse(ids[!len | !sym], sep = ", ")
      validate(need(
        all(len) & all(sym),
        message = glue("Invalid EMS ID(s): {problems}")
      ))

      ids
    })

    date_range <- reactive(input$date_range)

    # Fetch Data ------------------------------
    samples <- reactive({
      req(ems_ids())
      # Let user know what's going on
      id <- showNotification(
        "Collecting data from local EnMoDS database...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)

      # Messages to console
      withCallingHandlers(
        fetch_renmods(ems_ids(), date_range()), # Fetch data
        message = \(m) {
          shinyjs::html(
            id = "console_messages",
            html = cli::ansi_strip(m$message),
            add = TRUE
          )
        }
      )
    }) |>
      bindEvent(input$fetch_data)

    observe({
      shinyjs::toggleState("download_csv", nrow(samples()) > 0)
    }) |>
      bindEvent(samples())

    # Table ---------------------
    output$table_samples <- reactable::renderReactable({
      validate(need(input$ems_ids, "Choose EMS Ids to fetch data for"))
      validate(need(samples(), "Click 'Collect Data'"))
      validate(need(nrow(samples()) > 0, "No data to display"))
      req(input$data_columns)

      # Messages to console
      withCallingHandlers(
        tbl_overview(samples(), which = input$data_columns), # Create table
        message = \(m) {
          shinyjs::html(
            id = "console_messages",
            html = cli::ansi_strip(m$message),
            add = TRUE
          )
        }
      )
    })

    # Download ------------------
    output$download_csv <- downloadHandler(
      filename = function() {
        req(samples())
        # Use the selected dataset as the suggested file name
        paste0(paste0(input$ems_ids, collapse = "-"), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        readr::write_csv(samples(), file)
      }
    )
  })
}
