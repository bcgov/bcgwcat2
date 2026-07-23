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
    title = "Read Data",
    layout_sidebar(
      sidebar = sidebar(
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
          start = "1900-01-01",
          end = "2099-01-01",
          min = "1900-01-01",
          max = "2099-01-01"
        ),
        actionButton(ns("fetch_data"), "Collect data"),
        uiOutput(ns("messages")),
        radioButtons(
          ns("data_columns"),
          "Data to preview",
          choices = list("Relevant Columns" = "rel", "All Columns" = "all")
        ),
      ),
      card("Data Preview", DT::DTOutput(ns("table_samples"))),
      card("Messages", uiOutput(ns("console_messages"), container = pre))
    )
  )
}


mod_data_server <- function(id = "data") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Messages -----------------
    output$messages <- renderUI({
      ems_ids()
      invisible()
    })

    # UI --------------------------
    updateSelectizeInput(session, "ems_ids", choices = ems_ids, server = TRUE)

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

    output$table_samples <- DT::renderDT({
      validate(need(input$ems_ids, "Choose EMS Ids to fetch data for"))
      validate(need(samples(), "Click 'Collect Data'"))
      validate(need(nrow(samples()) > 0, "No data to display"))
      req(input$data_columns)
      tbl_overview(samples(), which = input$data_columns)
    })
  })
}
