#' Title
#'
#' @param id
#'
#' @returns
#'
#' @export
#' @examples
#' test_mod("overview")

mod_overview_ui <- function(
  id = "overview",
  data_types = renmods_data_types()
) {
  ns <- NS(id)

  nav_panel(
    title = "Status",
    layout_column_wrap(
      fill = FALSE,
      !!!purrr::map(rev(data_types), \(t) {
        uiOutput(ns(paste0("status_box_", t)))
      })
    ),

    # card(
    #   card_header("EnMoDS Datasets"),
    #   card_body(
    #     #height = 100,
    #     reactable::reactableOutput(ns("status_table"))
    #   ) #,
    #   # p(paste0("bcgwcat2 v", packageVersion("bcgwcat2"))),
    #   # p(paste0("renmods v", packageVersion("renmods")))
    # ),
    card(
      card_header("Messages"),
      card_body(
        uiOutput(ns("console_messages"), container = pre, class = "messages"),
        layout_columns(
          fill = FALSE,
          actionButton(
            ns("btn_data_check"),
            "Refresh EnMoDS Data status"
          ),
          actionButton(
            ns("dtn_data_download"),
            "Download/Update ALL datasets"
          ),
          actionButton(
            ns("btn_data_cache"),
            "Clear cache"
          )
        )
      )
    )
  )
}


mod_overview_server <- function(
  id = "overview",
  data_types = renmods_data_types(),
  ...
) {
  moduleServer(id, function(input, output, session) {
    updated <- reactiveVal(0)

    # Overall Buttons ----------------------------------
    observe({
      updated(updated() + 1)
    }) |>
      bindEvent(input$btn_data_check, ignoreInit = TRUE)

    observe({
      with_messages(renmods::renmods_update(types = "all"))
      updated(updated() + 1)
    }) |>
      bindEvent(input$btn_data_download, ignoreInit = TRUE)

    observe({
      removeModal()
      with_messages(renmods::cache_remove())
      updated(updated() + 1)
    }) |>
      bindEvent(input$btn_data_cache_confirmed, ignoreInit = TRUE)

    observe({
      showModal(modalDialog(
        title = "Are you sure?",
        "This will delete all files in your renmods cache as well as the renmods cache folder. You can remove files individually with the 'Remove data' button, or update the data with the 'Download' button.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("btn_data_cache_confirmed"),
            "Proceed",
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    }) |>
      bindEvent(input$btn_data_cache, ignoreInit = TRUE)

    # Data status -------------

    data_status <- reactive({
      s <- renmods::cache_status()

      if (!length(s)) {
        t <- data.frame(
          type = data_types,
          status = FALSE,
          last_downloaded = NA,
          date_range = NA
        )
      } else {
        t <- dplyr::mutate(s, update = renmods:::renmods()$update) |>
          dplyr::mutate(
            date_since = difftime(
              Sys.time(),
              .data$last_downloaded,
              units = "weeks"
            ),
            status = .data$date_since < .data$update,
            status = tidyr::replace_na(.data$status, FALSE)
          ) |>
          dplyr::select(status, type, last_downloaded, date_range)
      }
      t
    }) |>
      bindEvent(updated())

    output$status_fig <- ggiraph::renderGirafe({
      tbl_dates(data_status())
    })

    output$status_table <- reactable::renderReactable({
      tbl_dates(data_status())
      # data_status() |>
      #   dplyr::rename("dataset" = "type", "data_date_range" = "date_range") |>
      #   dplyr::rename_with(pretty_names) |>

      #   reactable::reactable(
      #     columns = list(
      #       Status = reactable::colDef(html = TRUE, cell = \(x) {
      #         if (x) {
      #           i <- "check-circle-fill"
      #           c <- "text-success"
      #         } else {
      #           i <- "x-circle-fill"
      #           c <- "text-danger"
      #         }
      #         as.character(bsicons::bs_icon(i, class = c))
      #       })
      #     )
      #   )
    })

    purrr::map(data_types, \(t) {
      output[[paste0("status_box_", t)]] <- renderUI({
        value_box_status(data_status(), t, session)
      })

      # Download buttons
      observe({
        with_messages(renmods::renmods_update(types = t, force = TRUE))
        updated(updated() + 1)
      }) |>
        bindEvent(input[[paste0("btn-download-", t)]])

      # Clear cache buttons
      observe({
        with_messages(renmods::cache_remove(types = t))
        updated(updated() + 1)
      }) |>
        bindEvent(input[[paste0("btn-clear-cache-", t)]])
    })

    # Outputs -------------------------------------------
    updated
  })
}

value_box_status <- function(data_status, type, session) {
  t <- dplyr::filter(data_status, .data$type == .env$type)
  title <- pretty_renmods(type)
  deets <- difftime(Sys.time(), t$last_downloaded) |> round()
  icon <- bsicons::bs_icon("x-circle-fill")
  last_dl <- p("Last downloaded", format(deets), "ago")
  dt_range <- p(
    "Date range: ",
    span(strong(t$date_range), style = "font-size:110%")
  )

  if (!t$status) {
    if (!is.na(t$last_downloaded)) {
      value <- "Out of date"
      other <- list(
        toolbar_input_button(
          "btn-download-all",
          label = "Download All Data",
          icon = bsicons::bs_icon("download"),
          class = "danger"
        )
      )
      theme <- "warning"
    } else {
      value <- "Not downloaded"
      last_dl <- p("No data available")
      dt_range <- p("No date range available")
      theme <- "danger"
    }
    icon <- bsicons::bs_icon("x-circle-fill")
  } else {
    value <- paste0("Up to date")
    theme <- "success"
    icon <- bsicons::bs_icon("check-circle-fill")
    last_dl <- p("Last downloaded", format(deets), "ago")
  }

  value_box(
    title = title,
    value = value,
    showcase = icon,
    theme = theme,
    last_dl,
    dt_range,
    p(
      style = "margin-top: 1ch",
      btn_dl_data(t$type, session),
      btn_clear_cache(t$type, session)
    )
  )
}


btn_dl_data <- function(type, session) {
  toolbar_input_button(
    session$ns(paste0("btn-download-", type)),
    label = "Download",
    show_label = TRUE,
    icon = bsicons::bs_icon("download")
  )
}

btn_clear_cache <- function(type, session) {
  toolbar_input_button(
    session$ns(paste0("btn-clear-cache-", type)),
    label = "Remove data",
    show_label = TRUE,
    icon = bsicons::bs_icon("trash3-fill")
  )
}
