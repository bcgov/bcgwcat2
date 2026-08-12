#' Title
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
#' bcgwcat_app()

bcgwcat_app <- function() {
  ui <- bcgwcat_ui(
    mod_overview_ui(),
    mod_data_ui(),
    mod_water_quality_ui(),
    mod_plots_ui(),
    mod_details_ui()
  )

  server <- function(input, output, session) {
    updated <- mod_overview_server()
    sample_data <- mod_data_server(updated, session)
    mod_water_quality_server(sample_data, session)
    mod_plots_server(sample_data, session)
    mod_details_server()
  }

  bcgwcat_run_app(ui, server)
}

bcgwcat_ui <- function(...) {
  # Set as non-interactive so renmods doesn't use terminal to ask about updates
  options(rlang_interactive = FALSE)
  Sys.setenv(NO_COLOR = TRUE, R_CLI_DYNAMIC = FALSE) # Fix progress bars

  page_navbar(
    id = "bcgwcat",
    title = "BC Groundwater Chemistry Analysis Tool",
    theme = bcgwcat_theme(),
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
#' @noRd
#' @examplesIf interactive()
#' bcgwcat_theme()

bcgwcat_theme <- function() {
  #https://www2.gov.bc.ca/gov/content/digital/design-system/foundations/colour#theme
  bs_theme(
    brand = system.file("shiny-assets", "_brand.yml", package = "bcgwcat2"),
    #`modal-header-padding` = "1rem",
    #`alert-padding-x` = "0.3rem",
    #`alert-padding-y` = "0.3rem",
    font_scale = 0.9
  ) |>
    bs_add_rules(
      "
  .messages.recalculating {
    opacity: 1 !important;
  }

  /* Hide reactive headers */
  .hidden .rt-tr-header {
    display: none !important;
  }
  .btn {
    max-width: 250px;
  }
  
  .mini-button {
    font-size: 90%;
    padding: 2px;
  }
  
  .action-button.mini-button {
    background-color: $brand-primary-gold;
    color: $brand-typography-primary;
    }
  
  li > .mini-button {
    padding: 1px;
  }
  
  .action-button.action-link {
    background-color: inherit;
    color: $brand-surface-primary-default;
  }

  .btn-default.disabled {
    background-color: $brand-surface-primary-disabled;
    color: $brand-typography-primary;
    border-color: $brand-surface-border-dark;
  }
  .action-button.disabled {
    background-color: $brand-surface-primary-disabled;
    color: $brand-typography-primary;
    border-color: $brand-surface-border-dark;
  }
  .action-button, .btn-default {
    background-color: $brand-surface-primary-default;
    color: $brand-typography-primary-invert;
  }
  .action-button:hover, .btn-default:hover {
    background-color: $brand-surface-primary-hover;
  }

  .card-header {
    background-color: $brand-surface-primary-default;
    color: $brand-typography-primary-invert;
  }
  .card-header:has(.nav-pills) {
    background-color: $brand-surface-background-white;
  }

  /* Active tab */
  .nav-pills .nav-link.active {
    background-color: $brand-primary-gold;
  }
  .nav-pills .nav-link.active:hover {
    background-color: $brand-primary-gold;
    color: $brand-typography-primary;
  }

  /* Other tab */
  .nav-pills .nav-link {
    color: $brand-primary-blue;
  }
  .nav-pills .nav-link:hover {
    background-color: $brand-surface-primary-hover;
    color: $brand-typography-primary-invert;
  }

  

  

  /* Reactable Search */
  .rt-search {
    align-self: flex-start;
  }


"
    ) # |>
  # bs_add_rules(format(cli::ansi_html_style()))
}

bcgwcat_run_app <- function(ui, server, options = list(port = 7405)) {
  shiny::shinyApp(ui, server, options = options)
}
