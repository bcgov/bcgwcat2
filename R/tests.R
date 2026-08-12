#' Create Test App for each module
#'
#' Not all arguments are used by all modules.
#'
#' @param module Character. Module Name.
#'
#' @returns Shiny App
#' @noRd
#' @examplesIf interactive()
#' test_mod("overview")
#'

test_mod <- function(module) {
  ui <- bcgwcat_ui(get(paste0("mod_", module, "_ui"))())

  server <- function(input, output, session) {
    get(paste0("mod_", module, "_server"))(updated = reactiveVal(1))
  }

  bcgwcat_run_app(ui, server)
}
