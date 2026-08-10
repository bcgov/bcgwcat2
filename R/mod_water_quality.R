#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
#' test_mod("water_quality")

mod_water_quality_ui <- function(id = "water_quality") {
  ns <- NS(id)

  nav_panel(
    title = "Water Quality",
    layout_sidebar(
      sidebar = sidebar(
        gap = "1em",
        radioButtons(
          ns("show"),
          "Samples to include",
          choices = c("All", "Problems only", "Non-missing only")
        ),
        uiOutput(ns("ui_params"))
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

mod_water_quality_server <- function(
  sample_data,
  app_session,
  id = "water_quality"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data
    samples <- sample_data$samples
    ems_ids <- sample_data$ems_ids

    # Navigation Mods -------------
    observe({
      nav_select("bcgwcat", selected = "Details", session = app_session)
    }) |>
      bindEvent(input$link_to_details)

    # Messages -----------------
    output$messages <- renderUI({
      ems_ids()
      invisible()
    })

    # UI --------------------
    output$ui_params <- renderUI({
      params <- unique(samples()$bcgwcat_params)
      selectizeInput(
        "parameters",
        label = "Parameters to include",
        multiple = TRUE,
        choices = params,
        selected = params
      )
    })

    # Table ---------------------
    output$table_water_quality <- reactable::renderReactable({
      validate(need(ems_ids(), "Choose EMS Ids to fetch data for"))
      validate(need(samples(), "Click 'Collect Data'"))
      validate(need(nrow(samples()) > 0, "No data to display"))

      req(input$params)
      req(input$show)

      # Create table
      with_messages(tbl_water_quality(
        samples(),
        which = input$show,
        params = input$params
      ))
    })
  })
}
