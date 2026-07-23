#' Title
#'
#' @returns
#'
#' @export
#' @examples
#' bcgwcat_app()

bcgwcat_app <- function() {
  ui <- bcgwcat_ui(
    mod_overview_ui("overview"),
    mod_data_ui("data"),
  )

  server <- function(input, output, session) {
    mod_overview_server("overview")
    mod_data_server("data")
  }

  bcgwcat_run_app(ui, server)
}

bcgwcat_ui <- function(...) {
  # Set as non-interactive so renmods doesn't use terminal to ask about updates
  options(rlang_interactive = FALSE)

  page_navbar(
    title = "BC Groundwater Chemistry Analysis Tool",
    theme = bcgwcat_theme(),
    gap = 0,
    padding = 0,
    header = shinyjs::useShinyjs(),
    # sidebar = bcgwcat_sidebar(),
    ...
  )
}

bcgwcat_sidebar <- function() {
  sidebar(
    title = "Read Data",
    selectizeInput(
      "ems_ids",
      "EMS IDs",
      choices = NULL,
      multiple = TRUE,
      options = list(create = TRUE)
    )
  )
}

#' Theme for bslib
#'
#' @returns bslib theme
#'
#' @export
#' @examples
#' bcgwcat_theme()

bcgwcat_theme <- function() {
  #https://www2.gov.bc.ca/gov/content/digital/design-system/foundations/colour#theme
  bs_theme(
    brand = system.file("shiny-assets", "_brand.yml", package = "bcgwcat2"),
    `modal-header-padding` = "1rem",
    `alert-padding-x` = "0.3rem",
    `alert-padding-y` = "0.3rem"
  ) |>
    bs_add_rules(
      "
  .action-button, .btn-default {
    background-color: $brand-surface-primary-default;
    color: $brand-typography-primary-invert;
  }
  .action-button:hover, .btn-default:hover {
    background-color: $brand-surface-primary-hover;
  }
"
    ) # |>
  # bs_add_rules(format(cli::ansi_html_style()))
}

bcgwcat_run_app <- function(ui, server, options = list(port = 7405)) {
  shiny::shinyApp(ui, server, options = options)
}
