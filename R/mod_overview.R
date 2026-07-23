#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
#' test_mod("overview")

mod_overview_ui <- function(id = "overview") {
  ns <- NS(id)

  nav_panel(
    title = "EnMoDS Data Status",
    layout_columns(
      fill = FALSE,
      uiOutput(ns("status_box1")),
      uiOutput(ns("status_box2")),
      uiOutput(ns("status_box3")),
      uiOutput(ns("status_box4"))
    ),

    card(
      card_body(
        fillable = FALSE,
        reactable::reactableOutput(ns("status_table"))
      )
    ),
    card(
      card_body(
        fillable = FALSE,
        actionButton(
          ns("check_data"),
          "Check EnMoDS Data status"
        ),
        actionButton(
          ns("data_download"),
          "Fetch/Update GWELLS data",
          class = "btn-success m-2"
        ),
        actionButton(
          ns("data_cache"),
          "Clear cache",
          class = "btn-warning m-2"
        )
      )
    )
  )
  # card(card_header(h4("Details")), tableOutput(ns("data_meta"))),

  # card(
  #   card_header("Messages"),
  #   div(
  #     style = "overflow-y:scroll;max-height:330px",
  #     verbatimTextOutput(ns("messages"), placeholder = TRUE)
  #   )
  # )o")
}


mod_overview_server <- function(id = "overview") {
  moduleServer(id, function(input, output, session) {
    # Output data status ----

    data_status <- reactive({
      s <- renmods::cache_status()
      if (!length(s)) {
        t <- data.frame()
      } else {
        t <- dplyr::mutate(s, update = renmods:::renmods()$update) |>
          dplyr::mutate(
            up_to_date = difftime(
              .data$last_downloaded,
              Sys.time(),
              units = "weeks"
            ),
            status = .data$up_to_date > .data$update
          ) |>
          dplyr::select(status, type, last_downloaded, date_range)
      }
      t
    })

    output$status_table <- reactable::renderReactable({
      data_status() |>
        dplyr::rename("dataset" = "type", "data_date_range" = "date_range") |>
        dplyr::rename_with(pretty_names) |>
        reactable::reactable(
          columns = list(
            Status = reactable::colDef(html = TRUE, cell = \(x) {
              if (x) {
                i <- "check-circle-fill"
                c <- "text-success"
              } else {
                i <- "x-circle-fill"
                c <- "text-danger"
              }
              as.character(bsicons::bs_icon(i, class = c))
            })
          )
        )
    })

    output$status_box1 <- renderUI(value_box_status(data_status(), "this_yr"))
    output$status_box2 <- renderUI(value_box_status(data_status(), "yr_2_5"))
    output$status_box3 <- renderUI(value_box_status(data_status(), "yr_5_10"))
    output$status_box4 <- renderUI(value_box_status(data_status(), "historic"))
  })
}

value_box_status <- function(data_status, type) {
  if (type == "overall") {
    t <- data_status
    title <- ""
    if (!nrow(t)) {
      value <- "No data"
      theme <- "danger"
      icon <- bsicons::bs_icon("x-circle-fill")
    } else if (all(t$status)) {
      value <- "All data up-to-date"
      theme <- "success"
      icon <- bsicons::bs_icon("check-circle-fill")
    } else {
      value <- "Some data out-of-date"
      theme <- "warning"
      icon <- bsicons::bs_icon("x-circle-fill")
    }
    other <- list(
      p(paste0("bcgwcat2 v", packageVersion("bcgwcat2"))),
      p(paste0("renmods v", packageVersion("renmods")))
    )
  } else {
    t <- dplyr::filter(data_status, .data$type == .env$type)
    title <- pretty_renmods(type)

    if (!nrow(t)) {
      value <- "No data"
      theme <- "danger"
      icon <- bsicons::bs_icon("x-circle-fill")
      other <- list(toolbar_input_button(
        "btn",
        label = "Download Data",
        icon = bsicons::bs_icon("download"),
        class = "danger"
      ))
    } else if (!t$status) {
      value <- "Out of date"
      deets <- difftime(Sys.time(), t$last_downloaded) |> round()

      other <- list(p("Last downloaded", format(deets), "ago"))
      theme <- "warning"
      icon <- bsicons::bs_icon("x-circle-fill")
    } else {
      value <- paste0("Up to date")
      theme <- "success"
      icon <- bsicons::bs_icon("x-circle-fill")
      other <- list()
    }
  }

  value_box(
    title = title,
    value = value,
    showcase = icon,
    theme = theme,
    !!!other
  )
}
