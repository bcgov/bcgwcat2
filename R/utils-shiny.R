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
    shinyjs::html(
      id = "console_messages",
      html = cli::ansi_strip(m$message),
      add = TRUE
    )
  })
}
