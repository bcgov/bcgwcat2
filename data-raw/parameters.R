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

library(dplyr)
library(readr)
library(assertr)
library(stringr)

# Indices ----------------------------------------------------------------
index_params <- read_csv("data-raw/index_params.csv", col_types = cols()) |>
  mutate(
    bcgwcat_wide = tolower(enmods_name) |>
      str_remove_all("\\(|\\)") |>
      str_remove(" fl\\. conc\\.") |>
      str_remove(" turb\\.") |>
      str_remove(" elec\\. cond\\.") |>
      str_remove(": filterable 1\\.0u tds") |>
      str_remove(" acidity") |>
      str_remove(" equiv\\./vol\\.") |>
      str_remove(" %=100\\*cations-anions\\/cations\\+anions ratio") |>
      str_replace_all(c(
        "nitrateno3" = "nitrate no3",
        "nitriteno2" = "nitrite no2"
      )) |>
      str_replace("ort.dis-p", "ort_dissolved_as_p") |>
      str_replace("kjel\\.totn", "kjeldahl_total") |>
      str_remove_all(";") |>
      str_replace_all(c(" " = "_", "-" = "_")) |>
      str_replace_all("_+", "_")
  )

index_meta_params <- read_csv(
  "data-raw/index_meta_params.csv",
  col_types = cols()
)
index_meta <- read_csv("data-raw/index_meta.csv", col_types = cols())


# MEQ Conversions ------------------------------------
# Atomic mass values from https://en.wikipedia.org/wiki/List_of_elements_by_atomic_properties

o <- 15.9994
cl <- 35.453
s <- 32.065
f <- 18.9984032
n <- 14.0067
c <- 12.0107
ca <- 40.078
h <- 1.008

meq_conversion <- tribble(
  ~ems_code , ~mass          , ~valency_state , ~type    ,
  # Anions
  "1104"    , cl             ,              1 , "anion"  , # Cl - Chloride Dissolved
  "1121"    , s + 4 * o      ,              2 , "anion"  , # SO4 - Sulfate Dissolved
  "1106"    , f              ,              1 , "anion"  , # F - Fluoride Dissolved
  "1110"    , n              ,              1 , "anion"  , # NO3 - Nitrate Dissolved - as N
  "1111"    , n              ,              1 , "anion"  , # NO2 - Nitrite Dissolved - as N
  "0102"    , ca + c + 3 * o ,              2 , "anion"  , # Alkalinity Total 4.5 (as CaCO3)
  "0150"    , h + c + 3 * o  ,              1 , "anion"  , # Bicarbonate Alkalinity (as CaCO3)
  "0149"    , c + 3 * o      ,              2 , "anion"  , # Carbonate Alkalinity (as CaCO3)

  # Cations
  "CA-D"    , 40.078         ,              2 , "cation" , # Calcium Dissolved
  "MG-D"    , 24.3050        ,              2 , "cation" , # Magnesium Dissolved
  "NA-D"    , 22.98976928    ,              1 , "cation" , # Sodium Dissolved
  "K--D"    , 39.0983        ,              1 , "cation" , # Potassium Dissolved
  "AL-D"    ,  8.993615      ,              1 , "cation" , # Aluminum Dissolved
  "CU-D"    , 63.546         ,              2 , "cation" , # Copper Dissolved
  "FE-D"    , 55.845         ,              2 , "cation" , # Iron Dissolved
  "MN-D"    , 54.938045      ,              2 , "cation" , # Manganese Dissolved
  "ZN-D"    , 65.38          ,              2 , "cation" , # Zinc Dissolved
  "1108"    , 14.0067        ,              1 , "cation" , # Nitrogen Ammonia Dissolved
) |>
  mutate(conversion = round(mass / valency_state, 5)) |>
  arrange(ems_code)

# Water Quality Standards ----------------------------------------------------
not_as_n <- function(x) !(!is.na(x) & str_detect(x, "Reported as N\\\\."))

wq_std <- bcdata::bcdc_get_data(
  record = '85d3990a-ec0a-4436-8ebd-150de3ba0747',
  resource = '6f32a85b-a3d9-44c3-9a14-15175eba25b6',
  col_types = cols()
) |>
  rename_all(tolower) |>
  filter(
    use == "Drinking Water",
    media == "Water",
    direction == "Upper Limit", # All upper limit anyway, but be explicit
    type == "Maximum Acceptable Concentration" # Remove Aesthetic objectives
  ) |>
  mutate(
    ems_code = stringr::str_remove(ems_code, "^EMS_"),
    ems_code = stringr::str_replace_all(ems_code, "_", "-")
  ) |>
  filter(
    ems_code %in% index_params$ems_code,
    !is.na(ems_code),
    days == 1,
    !is.na(limit),
    # !uniqueid %in% c(591, 627) # remove Nitrate and Nitrite "reported as N." (limitnotes)
  ) |>
  select(
    "uniqueid",
    "variable",
    "component",
    "ems_code",
    "limit",
    "limitnotes",
    "units",
    "condition",
    "predictedeffectlevel",
    "status"
  ) |>
  left_join(
    select(index_params, "ems_code", "enmods_name", "aqua_code"),
    by = "ems_code"
  ) |>
  mutate(limit = as.numeric(limit)) |>
  # Ensure that Nitrate and Nitrite with 'Reported as N.' are removed
  assert(not_as_n, limitnotes) |>
  # Find any duplicates (not true duplicates but different ways of measuring)
  mutate(n = n(), .by = "ems_code") |>
  assert(within_bounds(1, Inf), n)


# Interal Data ---------------------------------
usethis::use_data(
  index_params,
  index_meta_params,
  index_meta,
  wq_std,
  internal = TRUE,
  overwrite = TRUE
)

# External Data ----------------------------------
usethis::use_data(meq_conversion, internal = FALSE, overwrite = TRUE)
