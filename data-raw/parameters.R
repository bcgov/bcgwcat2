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
    bcgwcat_param = tolower(enmods_param) |>
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
  ) |>
  mutate(
    bcgwcat_param = if_else(
      bcgwcat_param == "cation_anion_balance",
      "charge_balance",
      bcgwcat_param
    ),
    bcgwcat_param = if_else(
      bcgwcat_param %in% c("charge_balance", "anion_sum", "cation_sum"),
      paste0(bcgwcat_param, "_enmods"),
      bcgwcat_param
    )
  ) |>
  rows_append(data.frame(
    bcgwcat_param = c("charge_balance", "anion_sum", "cation_sum")
  ))


index_meta_params <- read_csv(
  "data-raw/index_meta_params.csv",
  col_types = cols()
)
index_meta <- read_csv("data-raw/index_meta.csv", col_types = cols())

# Ion Use --------------------------------------------

ion_use <- tribble(
  ~bcgwcat_param                    , ~charge_balance , ~water_type , ~piper_plot , ~stiff_plot ,

  # Anions
  "chloride_dissolved"              , TRUE            , TRUE        , TRUE        , TRUE        ,
  "sulfate_dissolved"               , TRUE            , TRUE        , TRUE        , TRUE        ,
  "fluoride_dissolved"              , TRUE            , FALSE       , FALSE       , FALSE       ,
  "nitrate_no3_dissolved"           , TRUE            , FALSE       , FALSE       , FALSE       ,
  "nitrogen_nitrite_dissolved_no2"  , TRUE            , FALSE       , FALSE       , FALSE       ,
  "alkalinity_total_4.5_as_caco3"   , TRUE            , TRUE        , TRUE        , TRUE        ,
  "bicarbonate_alkalinity_as_caco3" , FALSE           , TRUE        , TRUE        , TRUE        ,
  "carbonate_alkalinity_as_caco3"   , FALSE           , FALSE       , TRUE        , FALSE       ,

  # Cations
  "calcium_dissolved"               , TRUE            , TRUE        , TRUE        , TRUE        ,
  "magnesium_dissolved"             , TRUE            , TRUE        , TRUE        , TRUE        ,
  "sodium_dissolved"                , TRUE            , TRUE        , TRUE        , TRUE        ,
  "potassium_dissolved"             , TRUE            , TRUE        , TRUE        , FALSE       ,
  "aluminum_dissolved"              , TRUE            , FALSE       , FALSE       , FALSE       ,
  "copper_dissolved"                , TRUE            , FALSE       , FALSE       , FALSE       ,
  "iron_dissolved"                  , TRUE            , FALSE       , FALSE       , FALSE       ,
  "manganese_dissolved"             , TRUE            , FALSE       , FALSE       , FALSE       ,
  "zinc_dissolved"                  , TRUE            , FALSE       , FALSE       , FALSE       ,
  "nitrogen_ammonia_dissolved"      , TRUE            , FALSE       , FALSE       , FALSE       ,

  "ph"                              , TRUE            , FALSE       , FALSE       , FALSE
)


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

ion_details <- tribble(
  ~bcgwcat_param                    , ~ems_code , ~mass          , ~valency_state , ~type    , ~water_code ,

  # Anions
  "chloride_dissolved"              , "1104"    , cl             ,              1 , "anion"  , "Cl"        , # Cl - Chloride Dissolved
  "sulfate_dissolved"               , "1121"    , s + 4 * o      ,              2 , "anion"  , "SO4"       , # SO4 - Sulfate Dissolved
  "fluoride_dissolved"              , "1106"    , f              ,              1 , "anion"  , ""          , # F - Fluoride Dissolved
  "nitrate_no3_dissolved"           , "1110"    , n              ,              1 , "anion"  , ""          , # NO3 - Nitrate Dissolved - as N
  "nitrogen_nitrite_dissolved_no2"  , "1111"    , n              ,              1 , "anion"  , ""          , # NO2 - Nitrite Dissolved - as N
  "alkalinity_total_4.5_as_caco3"   , "0102"    , ca + c + 3 * o ,              2 , "anion"  , "HCO3*"     , # Alkalinity Total 4.5 (as CaCO3)
  "bicarbonate_alkalinity_as_caco3" , "0150"    , h + c + 3 * o  ,              1 , "anion"  , "HCO3"      , # HCO3 - Bicarbonate Alkalinity (as CaCO3)
  "carbonate_alkalinity_as_caco3"   , "0149"    , c + 3 * o      ,              2 , "anion"  , "CO3"       , # CO3 - Carbonate Alkalinity (as CaCO3)

  # Cations
  "calcium_dissolved"               , "CA-D"    , 40.078         ,              2 , "cation" , "Ca"        , # Calcium Dissolved
  "magnesium_dissolved"             , "MG-D"    , 24.3050        ,              2 , "cation" , "Mg"        , # Magnesium Dissolved
  "sodium_dissolved"                , "NA-D"    , 22.98976928    ,              1 , "cation" , "Na"        , # Sodium Dissolved
  "potassium_dissolved"             , "K--D"    , 39.0983        ,              1 , "cation" , "K"         , # Potassium Dissolved
  "aluminum_dissolved"              , "AL-D"    ,  8.993615      ,              1 , "cation" , ""          , # Aluminum Dissolved
  "copper_dissolved"                , "CU-D"    , 63.546         ,              2 , "cation" , ""          , # Copper Dissolved
  "iron_dissolved"                  , "FE-D"    , 55.845         ,              2 , "cation" , ""          , # Iron Dissolved
  "manganese_dissolved"             , "MN-D"    , 54.938045      ,              2 , "cation" , ""          , # Manganese Dissolved
  "zinc_dissolved"                  , "ZN-D"    , 65.38          ,              2 , "cation" , ""          , # Zinc Dissolved
  "nitrogen_ammonia_dissolved"      , "1108"    , 14.0067        ,              1 , "cation" , ""          , # Nitrogen Ammonia Dissolved

  "ph"                              , "4"       , NA             , NA             , "cation" , "" # pH Lab - converts to [H ion] for charge_balances
) |>
  mutate(conversion = round(mass / valency_state, 5)) |>
  arrange(.data$type, .data$bcgwcat_param) |>
  left_join(ion_use, by = "bcgwcat_param")


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
    !is.na(limit)
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
    select(index_params, "ems_code", "enmods_param", "bcgwcat_param"),
    by = "ems_code"
  ) |>
  mutate(limit = as.numeric(limit)) |>
  # Ensure that Nitrate and Nitrite with 'Reported as N.' are removed
  assert(not_as_n, limitnotes) |>
  # Find any duplicates (not true duplicates but different ways of measuring)
  mutate(n = n(), .by = "ems_code") |>
  assert(within_bounds(1, Inf), n)

# EMS Ids ---------------------------------------
db <- renmods::renmods_connect()
ems_ids_all <- db |>
  dplyr::select("Location_ID") |>
  dplyr::distinct() |>
  dplyr::pull()
renmods::renmods_disconnect(db)

db <- renmods::renmods_connect(types = "historic")
min_date <- db |>
  dplyr::summarize(min_date = min(Observed_Date_Time, na.rm = TRUE)) |>
  dplyr::pull(min_date)
renmods::renmods_disconnect(db)

# Date ranges -----------------------------------

# Interal Data ---------------------------------
usethis::use_data(
  ems_ids_all,
  min_date,
  internal = TRUE,
  overwrite = TRUE
)

# External Data ----------------------------------
write_csv(wq_std, "inst/extdata/wq_std.csv")
write_csv(ion_details, "inst/extdata/ion_details.csv")
write_csv(index_params, "inst/extdata/index_params.csv")
write_csv(index_meta_params, "inst/extdata/index_meta_params.csv")
write_csv(index_meta, "inst/extdata/index_meta.csv")
