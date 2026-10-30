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

#' Show an informative message
#'
#' Wrapper around `cli::cli_inform()` and can also print a data frame below
#' the message.
#'
#' @param message Character vector. Message to show, formatted with cli.
#' @param df Data frame. Optional data to print below the message.
#' @param ... Passed on to `cli::cli_inform()`.
#' @param .envir Environment in which to evaluate the cli glue expressions.
#'
#' @returns Nothing, called for its side effect of showing a message.
#'
#' @noRd
#' @examplesIf interactive()
#' inform(c("This", "is", "a", "message"))
#' inform("Some data:", df = data.frame(x = 1:3))

inform <- function(message, df = NULL, ..., .envir = rlang::caller_env()) {
  cli::cli_inform(message, ..., .envir = .envir)
  if (!is.null(df)) cli::cli_code(utils::capture.output(as.data.frame(df)))
}

#' Stop with an error
#'
#' Wrapper around `cli::cli_abort()` and by default doesn't show the
#' calling function in the error message.
#'
#' @param ... Passed on to `cli::cli_abort()`, starting with the message.
#' @param .envir Environment in which to evaluate the cli glue expressions.
#' @param call Call to show in the error message. `NULL` shows none.
#'
#' @returns Nothing, throws an error.
#'
#' @noRd
#' @examplesIf interactive()
#' abort("Something went wrong")

abort <- function(..., .envir = rlang::caller_env(), call = NULL) {
  cli::cli_abort(..., call = call, .envir = .envir)
}
