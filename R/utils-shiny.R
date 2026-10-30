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

mini_button <- function(inputId, label, width = 80) {
  actionButton(
    inputId,
    label,
    width = width,
    class = "mini-button",
    inline = TRUE
  )
}

with_messages <- function(code) {
  withCallingHandlers(code, message = \(m) {
    msg <- m$message
    if (!stringr::str_detect(msg, "\\n$")) {
      msg <- paste0(msg, "\n")
    }
    if (stringr::str_count(msg, "\\n") > 1) {
      msg <- paste0(msg, "\n")
    }
    shinyjs::html(
      id = "console_messages",
      html = cli::ansi_strip(msg),
      add = TRUE
    )
  })
}
