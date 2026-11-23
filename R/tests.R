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

test_mod <- function(module, ems_ids = NULL) {
  if (!is.null(ems_ids)) {
    samples <- fetch_renmods(ems_ids)
    sample_data <- list(
      "samples" = reactive(samples),
      "ems_ids" = reactive(ems_ids)
    )
  } else {
    sample_data <- NULL
  }

  ui <- bcgwcat_ui(get(paste0("mod_", module, "_ui"))())

  server <- function(input, output, session) {
    get(paste0("mod_", module, "_server"))(
      updated = reactiveVal(1),
      sample_data = sample_data
    )
  }

  bcgwcat_run_app(ui, server)
}
