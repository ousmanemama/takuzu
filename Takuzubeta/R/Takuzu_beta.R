library(shiny)
library(shinyjs)
# La taille de la matrice 
taille_matrice <- 6

#  Validation des lignes et colonnes

ligne_valide_simple <- function(ligne) {
  if (sum(ligne == 0) > 3 || sum(ligne == 1) > 3) return(FALSE)
  for (i in 1:(length(ligne) - 2)) {
    if (!is.na(ligne[i]) && ligne[i] == ligne[i+1] && ligne[i] == ligne[i+2]) return(FALSE)
  }
  return(TRUE)
}


# Générateur de grille Takuzu complète

generer_grille_complete_logique <- function() {
  grille <- matrix(NA, nrow = taille_matrice, ncol = taille_matrice)
  lignes_existantes <- list()
  for (i in 1:taille_matrice) {
    valide <- FALSE
    essais <- 0
    while (!valide && essais < 1000) {
      ligne <- sample(rep(0:1, 3))
      if (!ligne_valide_simple(ligne)) { essais <- essais + 1; next }
      if (any(sapply(lignes_existantes, function(l) all(l == ligne)))) {
        essais <- essais + 1; next
      }
      grille[i, ] <- ligne
      lignes_existantes[[length(lignes_existantes)+1]] <- ligne
      valide <- TRUE
    }
  }
  for (j in 1:taille_matrice) {
    col <- grille[, j]
    if (!ligne_valide_simple(col) || sum(col == 0) != 3 ||
        any(duplicated(apply(grille[, j, drop=FALSE], 2, paste, collapse="")))) {
      return(generer_grille_complete_logique())
    }
  }
# Vérifier que toutes les colonnes sont uniques
  colonnes <- apply(grille, 2, paste, collapse = "")
  if (any(duplicated(colonnes))) {
    return(generer_grille_complete_logique())
  }
  return(apply(grille, c(1,2), as.character))
}


# Génération d'une grille partiellement remplie selon le niveau

grille_initiale <- function(niveau) {
  sol <- generer_grille_complete_logique()
  nb_cases <- taille_matrice^2
  nb_vides <- floor(nb_cases * switch(niveau, "Facile"=0.3, "Moyen"=0.5, "Difficile"=0.65, 0.5))
  grille <- sol
  grille[sample(1:nb_cases, nb_vides)] <- ""
  return(list(grille = grille, solution = sol))
}


# Interface utilisateur

ui <- fluidPage(
  tags$style(HTML(".sidebar { max-height: 100vh; overflow-y: auto; }")),
  useShinyjs(),
  titlePanel(" Jeu de Takuzu (6x6) — Logique Naturelle"),
  sidebarLayout(
    sidebarPanel(id = "sidebar-buttons",
                 selectInput("niveau", "Niveau de difficulté :", c("Facile", "Moyen", "Difficile"), "Moyen"),
                 actionButton("reset", "Réinitialiser"),
                 actionButton("check", "Vérifier la solution"),
                 actionButton("reveler", " Afficher la solution"),
                 hr(), br(), br(),
                 h4("⏱️ Temps écoulé :"),
                 textOutput("timer")
    ),
    mainPanel(
      uiOutput("matrice"),
      verbatimTextOutput("message")
    )
  )
)


# Logique serveur

server <- function(input, output, session) {
  solution_complete <- reactiveVal()
  matrice <- reactiveVal()
  chronometre <- reactiveVal(Sys.time())
  
  tips <- "Règles : 
  \n- chaque case de la grille doit être remplie avec un 0 ou un 1.\n- chaque ligne et chaque colonne doivent contenir autant de 0 que de 1.\n- il est interdit d’avoir trois 0 ou trois 1 consécutifs dans une ligne ou une colonne.\n-deux lignes ou deux colonnes identiques sont interdites dans la même grille."
  output$message <- renderText(tips)
  
# Initialisation
  observeEvent(TRUE, {
    niveaux <- grille_initiale("Moyen")
    matrice(niveaux$grille)
    solution_complete(niveaux$solution)
  }, once = TRUE)
  
# Réinitialisation
  observeEvent(input$reset, {
    niveaux <- grille_initiale(input$niveau)
    matrice(niveaux$grille)
    solution_complete(niveaux$solution)
    chronometre(Sys.time())
    output$message <- renderText(tips)
  })
  
# Afficher la solution
  observeEvent(input$reveler, {
    matrice(solution_complete())
  })
  
# Chronomètre
  output$timer <- renderText({
    invalidateLater(1000, session)
    paste0(as.integer(difftime(Sys.time(), chronometre(), units = "secs")), " sec")
  })
  
# Interface interactive
  output$matrice <- renderUI({
    grille <- matrice()
    lapply(1:taille_matrice, function(i) {
      fluidRow(
        lapply(1:taille_matrice, function(j) {
          actionButton(
            inputId = paste0("cell_", i, "_", j),
            label = grille[i, j],
            style = "width: 50px; height: 50px; font-size: 18px;",
            onclick = paste0("Shiny.setInputValue('cell_click', {i:", i, ", j:", j, "}, {priority: 'event'})")
          )
        })
      )
    })
  })
  
# Gestion des clics
  observeEvent(input$cell_click, {
    i <- input$cell_click$i
    j <- input$cell_click$j
    grille <- matrice()
    grille[i, j] <- ifelse(grille[i, j] == "", "0", ifelse(grille[i, j] == "0", "1", ""))
    matrice(grille)
  })
  
# Vérification
  observeEvent(input$check, {
    grille <- matrice()
    valid <- TRUE
    message <- ""
    
    for (i in 1:taille_matrice) {
      if (sum(grille[i, ] == "0") != 3 || sum(grille[i, ] == "1") != 3 ||
          sum(grille[, i] == "0") != 3 || sum(grille[, i] == "1") != 3) {
        valid <- FALSE; message <- "Déséquilibre ligne ou colonne."; break
      }
    }
    
    if (valid) {
      for (i in 1:taille_matrice) {
        for (j in 1:(taille_matrice - 2)) {
          if (grille[i, j] == grille[i, j+1] && grille[i, j+1] == grille[i, j+2] && grille[i, j] != "") {
            valid <- FALSE; message <- "3 identiques dans une ligne."; break
          }
          if (grille[j, i] == grille[j+1, i] && grille[j+1, i] == grille[j+2, i] && grille[j, i] != "") {
            valid <- FALSE; message <- "3 identiques dans une colonne."; break
          }
        }
      }
    }
    
    if (valid) {
      lignes <- apply(grille, 1, paste, collapse="")
      colonnes <- apply(grille, 2, paste, collapse="")
      if (any(duplicated(lignes)) || any(duplicated(colonnes))) {
        valid <- FALSE; message <- "Lignes ou colonnes dupliquées."
      }
    }
    
    if (valid) message <- "✅ Bravo ! Grille correcte."
    output$message <- renderText(message)
  })
}

# Fonction pour démarrer l'application
run_app <- function() {
  shinyApp(ui = ui, server = server)
}

# Démarrer l'application
run_app()