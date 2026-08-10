#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
#' test_mod("details")

mod_details_ui <- function(id = "details") {
  ns <- NS(id)

  nav_panel(
    title = "Details",
    layout_sidebar(
      sidebar = sidebar(
        a("About", href = "#about"),
        a("EMS IDs", href = "#ems-ids"),
        a("Charge Balances", href = "#charge-balances"),
        a("Water Quality", href = "#water-quality"),
        a("Parameter Names", href = "#parameters")
      ),

      ## About ---------------------------------------------------------------
      card(
        card_header("About this Shiny App", id = "about"),
        min_height = 400,
        p(
          "This app allows functions the R package bcgwcat (BC Groundwater Chemistry Analysis Tools)",
          "to be used in a simpler, and interactive format."
        ),
        p(
          "BC Government Environmental Monitoring System (EMS) data is retrieved using the",
          a('rems', href = "https://github.com/bcgov/rems", target = "_blank"),
          "package and filtered. ",
          "Data is filtered to the EMS IDs and the Date Range specified",
          "and can then be exported for use in AquaChem, or",
          "water quality summaries can be viewed, ",
          "or piper and stiff plots can be created."
        ),
        p(
          "Data can be downloaded for AquaChem as either csv or a colour-coded xlsx."
        ),
        p(
          "Piper plots and stiff diagrams can be viewed and downloaded as png."
        ),
        p(
          "Piper plots are created via the ",
          a(
            "smwrGraphs",
            href = "https://code.usgs.gov/water/analysis-tools/smwrGraphs"
          ),
          "package developed by Dave Lorenz and Laura DeCicco in the USGS"
        )
      ),

      ## EMS IDS ----------------------------
      card(
        card_header("EMS IDs", id = "ems-ids"),
        min_height = 400,
        "In order to download water chemistry data from the EMS database, ",
        "you'll need to supply",
        em("public"),
        "EMS IDs, in the format of '1401030' or 'E292373'.",
        "Because this app works with the publically available EMS data base, only public data",
        "can be accessed. However, if you have private data you are able to mark as public,",
        "it will then be accessible here"
      ),

      ## Charge Balances ----------------------
      card(
        card_header("Charge Balances", id = "charge-balances"),
        min_height = 400,
        p(
          "In the data, values `charge_balance`, `cation_sum` and `anion_sum` are calculated locally ",
          "by the bcgwcat R package, based on calculation details from ALS Global. ",
          "Potential changes in workflows over the years have made it difficult to ascertain exactly",
          "how charge balances were calculated in older samples. This resulted in",
          "discrepancies between EMS and locally calculated charge balances. Therefore",
          "for consistency, we calculate charge balances for all samples using the ALS",
          "formula below."
        ),

        p(
          "One difference between this calculation and that of ALS, is that we use more",
          "significant digits when calculating MEQ."
        ),
        p(
          "anion sum = Cl_meq + SO4_meq + F_meq + NO3_meq + NO2_meq + Means_Alk_meq"
        ),

        p(
          "cation sum = Ca_meq + Mg_meq + Na_meq + K_meq + Al_diss_meq +",
          "Cu_diss_meq + Fe_diss_meq + Mn_diss_meq + Zn_diss_meq + NH4_meq +",
          "(10 ^ (-pH_lab)) * 1000"
        ),

        p(
          "Charge balance = 100 x (Cation Sum - Anion sum) / (Cation Sum + Anion Sum)"
        ),

        p(
          "Missing values are ignored (ie. generally treated as 0). However, if all",
          "values for cations or anions are missing the charge balance is NA."
        )
      ),

      ## Water Quality --------------
      card(
        card_header("Water Quality", id = "water-quality"),
        min_height = 400,
        p(
          "Water Quality is summarized by whether parameters exceed '",
          strong("Upper Limits", .noWS = "outside"),
          "' of the '",
          strong("Maximum Acceptable Concentration", .noWS = "outside"),
          "' for '",
          strong("Drinking Water", .noWS = "outside"),
          "' based on the",
          a(
            "Water Quality Guidelines of B.C.",
            href = "https://catalogue.data.gov.bc.ca/dataset/85d3990a-ec0a-4436-8ebd-150de3ba0747",
            target = "blank"
          ),
          "downloaded via the bcdata package."
        )
      ),

      ## Param Names --------------
      card(
        card_header("Parameter Names", id = "parameters"),
        min_height = 400,
        p(
          "The parameter names presented in the Preview/Export tab are modified from the EMS data",
          "to match required parameter names for AquaChem."
        ),
        p(
          "Here is the list of parameters used by this App and how the names compare between ",
          "the incoming EMS data and the data prepared for export"
        ),

        reactable::reactableOutput(ns("index_params")),
        reactable::reactableOutput(ns("index_meta_params")),
        reactable::reactableOutput(ns("index_meta"))
      )
    )
  )
}

#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
mod_details_server <- function(id = "details", ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Parameter tables ----------------------------------------------------------
    output$index_params <- reactable::renderReactable({
      index_params() |>
        reactable::reactable()
    })

    output$index_meta <- reactable::renderReactable({
      index_meta() |>
        reactable::reactable()
    })

    output$index_meta_params <- reactable::renderReactable({
      index_meta_params() |>
        reactable::reactable()
    })
  })
}
