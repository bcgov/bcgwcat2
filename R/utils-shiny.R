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
