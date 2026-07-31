mini_button <- function(inputId, label, width = 80) {
  actionButton(
    inputId,
    label,
    width = width,
    class = "mini-button",
    inline = TRUE
  )
}
