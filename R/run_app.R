#' Lancer l'application principale Takuzu
#'
#' @export
run_app <- function() {
  appFile <- system.file("Takuzu/app.R", package = "Takuzu")
  if (appFile == "") {
    stop("Fichier app.R introuvable.")
  }

  source(appFile, local = TRUE)

  shiny::shinyApp(ui = ui, server = server)
}
