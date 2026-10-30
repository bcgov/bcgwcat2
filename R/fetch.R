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

#' Fetch renmods data for specific ems_ids
#'
#' Light wrapper around [renmods::renmods_connect()] to fetch data by date and
#' EMS ID. Also collects data into memory, so can take some time if a lot of
#' data are requested.
#'
#' @param ems_ids Character Vector. EMS ids to filter EnMoDS samples to.
#' @param date_range Character/Date Vector. Start/end dates to filter to. `NULL`
#'   returns all data.
#'
#' @returns Data frame of filtered EnMoDS data.
#'
#' @export
#' @examplesIf interactive()
#' s <- fetch_renmods(
#'   ems_ids = c("1401030", "1401377"),
#'   date_range = c("2020-01-01", "2022-02-01")
#' )
#'
#'  s <- fetch_renmods(
#'   ems_ids = 22769669,
#'   date_range = c("2020-01-01", "2022-02-01")
#' )

fetch_renmods <- function(ems_ids, date_range = NULL) {
  ems_ids <- as.character(ems_ids)

  date_range <- date_range %||% c(min_date, Sys.Date())

  s_tbl <- renmods::renmods_connect(dates = date_range) |>
    dplyr::filter(.data$Location_ID %in% .env$ems_ids)

  s <- dplyr::collect(s_tbl)

  renmods::renmods_disconnect(s_tbl)

  # TODO: CHECK IF ANY DATA -> check_present()

  # TODO: Check if ANY MEDIUM != "Water - Ground"

  s
}
