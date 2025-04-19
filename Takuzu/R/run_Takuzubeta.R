#' Lancer l'application Takuzubeta
#'
#' @export
run_Takuzubeta <- function() {
  appFile <- system.file("Takuzubeta/app.R", package = "Takuzu")
  if (appFile == "") {
    stop("Fichier app.R introuvable dans le package.")
  }

  source(appFile, local = TRUE)

  shiny::shinyApp(ui = ui, server = server)
}