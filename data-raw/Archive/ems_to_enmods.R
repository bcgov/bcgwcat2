# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and

# This is a conversion script to convert the bcgwcat parameter index from
# ems to EnMoDS. This script is kept of posterity but SHOULD NOT be rerun
# unless absolutely necessary.
#
# The outputs of this script (index_params.csv, index_meta.csv) should be
# updated and modify *by hand* as needed.
#
# This script merely transforms the existing, hand-created, indices into new ones.

library(dplyr)
library(janitor)
library(stringr)
library(assertr)

# Index - Parameters ----------------------------------------------------------
# Links EnMoDS, to existing index with EMS, Aquachem, and SMWR (USGOV)

enmods_units <- readr::read_csv(
  "https://coms.api.gov.bc.ca/api/v1/object/733598c1-8c1b-4296-90c0-b17af7cc348e",
  show_col_types = FALSE
) |>
  clean_names() |>
  select("enmods_unit" = "code")

# New EnMods Parameters (only, no meta)
enmods_params <- readr::read_csv(
  "https://coms.api.gov.bc.ca/api/v1/object/449df51a-94d7-4a7b-be26-d200dc271fcb",
  show_col_types = FALSE
) |>
  clean_names() |>
  select(
    "enmods_name" = "observed_property_id",
    "ems_code", # Also called observed_property_name in downloaded data
    "enmods_description" = "description",
    "enmods_unit" = "default_unit"
  ) |>
  mutate(
    enmods_unit = str_extract(.data$enmods_unit, "^[^-]+") |> str_trim(),
    ems_code = str_remove(ems_code, "^PCB")
  ) |>
  left_join(enmods_units, by = "enmods_unit")

ems_params <- readr::read_csv(
  "data-raw/params_list_rems.csv",
  show_col_types = FALSE
) |>
  rename_with(\(x) str_replace_all(x, "rems", "ems")) |>
  filter(type == "param", !is.na(ems_name)) |> # Ignore empty Aquachem fields
  select(-"type", -"data_type") |>
  dplyr::mutate(
    ems_unit = str_replace_all(ems_unit, c("meq" = "mEq", "^pH$" = "pH units")),
    water_quality = tidyr::replace_na(water_quality, TRUE),
    # Add missing leading zeros
    ems_code = dplyr::if_else(
      stringr::str_detect(ems_code, "^[0-9]{1,3}$"),
      stringr::str_pad(ems_code, width = 4, pad = "0"),
      ems_code
    )
  )

index_params <- left_join(
  ems_params,
  filter(enmods_params, !is.na(ems_code)),
  by = c("ems_code", "ems_unit" = "enmods_unit")
) |>
  assertr::assert(not_na, enmods_name)

readr::write_csv(index_params, "data-raw/index_params.csv")

# Index - Parameter Metadata --------------------------------------------------
index_meta_params <- tribble(
  ~enmods_name             , ~ems_name           ,
  "observed_property_id"   , "parameter"         ,
  "observed_property_name" , "parameter_code"    ,
  "analysis_method"        , "analytical_method" ,
  "result_value"           , "result"            ,
  "result_unit"            , "unit"              ,
)

readr::write_csv(index_meta_params, "data-raw/index_meta_params.csv")

# Index - Sample Metadata -----------------------------------------------------

# Link new EnMoDS metadata to older EMS metadata
index_meta <- tribble(
  ~enmods_name         , ~ems_name          ,
  "location_id"        , "ems_id"           ,
  "observed_date_time" , "collection_start" ,
  #""                   , "location_purpose" , # TODO: No equivalent?
  "medium"             , "sample_state"     ,
  "location_latitude"  , "latitude"         ,
  "location_longitude" , "longitude"        ,
  "location_name"      , "obs_well" # enmods location_name has the obs_well but needs formatting
)

readr::write_csv(index_meta, "data-raw/index_meta.csv")
