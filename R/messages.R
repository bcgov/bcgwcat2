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
#'
#' @param ...
#' @param .envir
#'
#' @returns
#'
#' @noRd
#' @examplesIf interactive()
#' inform(c("This", "is", "a", "message"))

inform <- function(message, df = NULL, ..., .envir = rlang::caller_env()) {
  cli::cli_inform(message, ..., .envir = .envir)
  if (!is.null(df)) cli::cli_code(utils::capture.output(as.data.frame(df)))
}

#' Title
#'
#' @param ...
#' @param .envir
#' @param call
#'
#' @returns
#'
#' @noRd
#' @examplesIf interactive()

abort <- function(..., .envir = rlang::caller_env(), call = NULL) {
  cli::cli_abort(..., call = call, .envir = .envir)
}
