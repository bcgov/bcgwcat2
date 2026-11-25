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
#' test_mod("plots", ems_ids = c("E298733", "E336585", "E335827"))

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
          options = selectize_options()
        ),
        checkboxGroupInput(
          ns("qc"),
          label = strong("Quality control"),
          choiceNames = tt_labels(inputs_qc()),
          choiceValues = inputs_qc()$arg
        ),
        checkboxGroupInput(
          ns("display"),
          strong("Plot display"),
          choiceNames = inputs_display()$text,
          choiceValues = inputs_display()$arg
        )
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
                card_body(
                  plotOutput(ns("plot_stiff")),
                  em(textOutput(ns("msg_stiff")))
                )
              ),
              card(
                full_screen = TRUE,
                card_header("Piper Plot"),
                card_body(
                  plotOutput(ns("plot_piper")),
                  em(textOutput(ns("msg_piper")))
                )
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
    msg_stiff <- reactiveVal(NULL)
    msg_piper <- reactiveVal(NULL)

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
        server = TRUE,
        options = selectize_options()
      )
    })

    output$msg_stiff <- renderText({
      msg_stiff()
    })
    output$msg_piper <- renderText({
      msg_piper()
    })
    # Options ---------------------------
    qc <- reactive({
      input_args(inputs_qc(), input$qc)
    })

    display <- reactive({
      i <- input_args(inputs_display(), input$display)
      if (!i["plot_legend"]) {
        i["legend_position"] <- "none"
      }
      i <- i[-1]
      i
    })

    # Samples for plots -----------------
    samples_sub <- reactive({
      validate(need(!is.null(input$ems_ids), "Select EMS IDs to plot"))
      validate(need(samples(), "Collect data first (see Data tab)"))
      s <- dplyr::filter(samples(), .data$location_id %in% input$ems_ids)
      validate(need(nrow(s) > 0, "No data to display"))
      s
    })

    # Plots - Stiff ---------------------
    output$plot_stiff <- renderPlot({
      # Create plot, catch messages
      msg_stiff(NULL)
      m <- capture.output(
        p <- rlang::exec(stiff_plot, samples_sub(), !!!qc(), !!!display()),
        type = "message"
      )
      msg_stiff(m)
      p
    }) |>
      bindCache(samples_sub(), qc(), display())

    # Plots - Piper ---------------------
    output$plot_piper <- renderPlot({
      msg_piper(NULL)
      # Create plot
      m <- capture.output(
        p <- rlang::exec(piper_plot, samples_sub(), !!!qc(), !!!display()),
        type = "message"
      )
      msg_piper(m)
      p
    }) |>
      bindCache(samples_sub(), qc(), display())
  })
}

inputs_qc <- function() {
  dplyr::tribble(
    ~text                  , ~arg                 , ~tt                                                                          ,
    "Valid charge balance" , "qc_valid_only"      , "Only include samples with valid charge blances"                             ,
    "Omit outliers"        , "qc_omit_outliers"   , "Omit samples within a location which do not have a Dominant Water Type"     ,
    "Use Alkalinity"       , "qc_with_alkalinity" , "Use Alkalinity as a substitute for Bicarbonate, if Bicarbonate is missing."
  )
}

inputs_display <- function() {
  dplyr::tribble(
    ~text         , ~arg          ,
    "Show legend" , "plot_legend"
  )
}

tt_labels <- function(i, filter = NULL, label = FALSE) {
  purrr::map2(i$text, i$tt, \(t, tt) {
    tooltip(
      trigger = list(t, bsicons::bs_icon("info-circle")),
      tt
    )
  })
}

input_args <- function(i, filter) {
  rlang::set_names(i$arg %in% filter, i$arg)
}
