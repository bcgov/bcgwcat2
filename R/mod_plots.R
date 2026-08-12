#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
#' test_mod("water_quality")

mod_plots_ui <- function(id = "plots") {
  ns <- NS(id)

  nav_panel(
    title = "Plots",
    layout_sidebar(
      sidebar = sidebar(
        gap = "1em",
        selectizeInput(
          ns("ems_ids"),
          "EMS IDs to plot",
          choices = NULL,
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        radioButtons(
          ns("omit_bad"),
          "Omit 'bad' charge balances?",
          choices = c("Yes" = TRUE, "No" = FALSE)
        ),
        radioButtons(
          ns("legends"),
          "Plot legends",
          choices = c("Yes" = TRUE, "No" = FALSE)
        ),
      ),
      navset_card_pill(
        nav_panel(
          title = "Plots",
          card_body(
            gap = 0,
            layout_column_wrap(
              width = 1 / 2,
              card(
                full_screen = TRUE,
                card_header("Stiff Plot"),
                card_body(plotOutput(ns("plot_stiff")))
              ),
              card(
                full_screen = TRUE,
                card_header("Piper Plot"),
                card_body(plotOutput(ns("plot_piper")))
              )
            )
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

mod_plots_server <- function(
  sample_data,
  app_session,
  id = "plots",
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
    observe({
      updateSelectizeInput(
        session,
        "ems_ids",
        choices = ems_ids(),
        server = TRUE
      )
    })

    # Plots - Stiff ---------------------
    output$plot_stiff <- renderPlot({
      validate(need(samples(), "Collect data first (see Data tab)"))
      validate(need(nrow(samples()) > 0, "No data to display"))

      req(input$ems_ids)
      req(input$omit_bad)
      req(input$legends)

      stiff_plot(
        samples(),
        ems_id = input$ems_ids,
        valid = as.logical(input$omit_bad),
        legend = as.logical(input$legends)
      )
    })

    # Plots - Piper ---------------------
    output$plot_piper <- renderPlot({
      validate(need(samples(), "Collect data first (see Data tab)"))
      validate(need(nrow(samples()) > 0, "No data to display"))

      req(input$ems_ids)
      req(input$omit_bad)
      req(input$legends)

      # Create plot
    })
  })
}
