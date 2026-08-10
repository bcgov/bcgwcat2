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
          choices = c(
            "All" = "all",
            "Problems only" = "problems",
            "Non-missing only" = "no_missing"
          )
        ),
        uiOutput(ns("ui_params")),
        mini_button(
          ns("params_toggle"),
          label = "Select/Unselect All",
          width = 120
        )
      ),
      navset_card_pill(
        nav_panel(
          title = "Data",
          card_body(
            gap = 0,
            strong(
              "Summarized Water Quality based to the 'Upper Limits' ",
              "of the 'Maximum Acceptable Concentration' for 'Drinking Water'"
            ),
            "Note: ",
            tags$ul(
              tags$li(
                "Parameter names are different from those in the EMS data base.",
              ),
              tags$li(
                "Please see ",
                actionLink(ns("link_to_details"), "Details"),
                "for more specifics regarding how bcgwcat parameter names correspond to EnMoDs and EMS data."
              )
            ),
            reactable::reactableOutput(ns("table_water_quality")),
            # shinyjs::disabled(
            #   downloadButton(
            #     ns("download_csv"),
            #     label = "Download as CSV"
            #   )
            # )
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
  id = "water_quality",
  ...
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

    # UI --------------------
    params <- reactive(unique(samples()$bcgwcat_param))

    output$ui_params <- renderUI({
      selectizeInput(
        ns("params"),
        label = "Parameters to include",
        multiple = TRUE,
        choices = params(),
        selected = params()
      )
    })

    observe({
      browser()
      if (!is.null(input$params) && length(input$params) > 0) {
        updateSelectizeInput(session, "params", selected = character(0))
      } else {
        updateSelectizeInput(session, "params", selected = params())
      }
    }) |>
      bindEvent(input$params_toggle)

    # Table ---------------------
    output$table_water_quality <- reactable::renderReactable({
      validate(need(samples(), "Collect data first (see Data tab)"))
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
