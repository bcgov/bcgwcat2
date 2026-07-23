#' Create Test App for each module
#'
#' Not all arguments are used by all modules.
#'
#' @param module Character. Module Name.
#'
#' @returns Shiny App
#' @noRd
#' @examples
#' test_mod("overview")
#'

test_mod <- function(module) {
  ui <- bcgwcat_ui(get(paste0("mod_", module, "_ui"))())

  server <- function(input, output, session) {
    get(paste0("mod_", module, "_server"))()
  }

  bcgwcat_run_app(ui, server)
}

#' Create Test App for Component Modules
#'
#' Not all arguments are used by all modules.
#'
#' @param module Character. Module Name.
#' @param use Character vector. Objects to include
#' @param deployment_id Character. Example Deployment ID.
#' @param model_id Character. Example Model ID.
#' @param species_id Character. Example Species ID.
#' @param spatial_ids Character. Optional starting spatial ids.
#' @param spatial_selection List. Optional starting spatial selection
#' (`show_clicked` and `show_spatial_ids`).
#' @param map_views List. Optional set map views for map synchronization.
#'
#' @returns Shiny App
#'
#' @export
test_comp <- function(
  module,
  use = c(
    "parent_id",
    "deployment_id",
    "model_id",
    "species_id",
    "spatial_ids",
    "spatial_selection",
    "map_views"
  ),
  deployment_id = "deployment1",
  model_id = "bam_v5_can71",
  species_id = "BBWO",
  spatial_ids = NULL,
  spatial_selection = list(show_clicked = NULL, show_spatial_ids = NULL),
  map_views = list(active_tab = NULL, set_by = NULL, view = NULL)
) {
  if (testthat::is_testing()) {
    sdmevaltool_options(
      base = testthat::test_path("../../../misc/base")
    )
  }

  ui <- bslib::page(theme = sdm_theme(), get(paste0(module, "_ui"))())

  u <- list(
    "parent_id" = "test",
    "deployment_id" = reactive(deployment_id),
    "model_id" = reactive(model_id),
    "species_id" = reactive(species_id),
    "spatial_ids" = reactiveVal(spatial_ids),
    "spatial_selection" = purrr::map(spatial_selection, \(s) {
      reactive(force(s))
    }),
    "map_views" = purrr::map(map_views, \(m) reactive(force(m)))
  )

  u <- u[names(u) %in% use]

  server <- function(input, output, session) {
    do.call(get(paste0(module, "_server")), u)
  }

  shiny::shinyApp(ui, server, options = list(port = 7405))
}


#' Skip testthat tests if no data is detected
#'
#' @returns Logical
#'
#' @export

skip_if_no_data <- function() {
  if (!have_data()) {
    testthat::skip(
      paste0(
        "No data available for testing: ",
        normalizePath(sdmEvalToolCore::sdmevaltool_options()$base)
      )
    )
  }
}
