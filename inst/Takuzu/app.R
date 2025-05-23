# Installer shinyjs si ce n'est pas déjà fait
# install.packages("shinyjs")

library(shiny)
library(shinyjs)

# Définir la taille de la grille
taille_matrice <- 6  # Définir la taille de la matrice (par exemple 6x6)

# Fonction pour générer une grille vide
matrice_vide <- function() {
  matrix("", nrow = taille_matrice, ncol = taille_matrice)
}

# Interface utilisateur (UI)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("jeu de Takuzu"),  # Titre de l'application
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reset", "Réinitialiser le jeu"),  # Bouton pour réinitialiser la grille
      actionButton("check", "Vérifier la solution"),  # Bouton pour vérifier si la solution est correcte
      br(), br(),
      h4("⏱️ Temps écoulé :"),
      textOutput("timer")  # Affichage du chronomètre
    ),
    mainPanel(
      uiOutput("matrice"),  # Interface dynamique pour afficher la grille de jeu
      verbatimTextOutput("message")  # Zone de texte pour afficher les messages du jeu
    )
  )
)

# Serveur (logique de l'application)
server <- function(input, output, session) {
  matrice <- reactiveVal(matrice_vide())  # Grille réactive
  chronometre <- reactiveVal(Sys.time())  # Temps de démarrage pour le chronomètre
  
  tips <- "Règles du jeu
  — chaque case de la grille doit être remplie avec un 0 ou un 1 ;
  — chaque ligne et chaque colonne doivent contenir autant de 0 que de 1 ;
  — il est interdit d’avoir trois 0 ou trois 1 consécutifs dans une ligne ou une colonne ;
  — deux lignes ou deux colonnes identiques sont interdites dans la même grille."
  
  output$message <- renderText(tips)  # Affichage des règles
  
  # Réinitialisation du jeu lorsque l'utilisateur appuie sur le bouton "reset"
  observeEvent(input$reset, {
    matrice(matrice_vide())  # Remet la grille à l'état initial
    output$message <- renderText(tips)  # Réaffiche les règles
    chronometre(Sys.time())  # Redémarre le chronomètre
  })
  
  # Affichage du temps écoulé
  output$timer <- renderText({
    invalidateLater(1000, session)  # Rafraîchissement chaque seconde
    paste0(as.integer(difftime(Sys.time(), chronometre(), units = "secs")), " secondes")
  })
  
  # Affichage de la grille sous forme de boutons interactifs
  output$matrice <- renderUI({
    matrice_data <- matrice()
    lapply(1:taille_matrice, function(i) {
      fluidRow(
        lapply(1:taille_matrice, function(j) {
          cell_value <- matrice_data[i, j]  # Récupération de la valeur actuelle de la cellule
          actionButton(
            inputId = paste0("cell_", i, "_", j),
            label = cell_value,  # Affichage de la valeur actuelle (0, 1 ou vide)
            style = "width: 50px; height: 50px;",  # Taille du bouton
            
            # Ajout d'un événement onclick pour mettre à jour la cellule sélectionnée
            onclick = paste0("Shiny.setInputValue('cell_click', {i: ", i, ", j: ", j, "}, {priority: 'event'})")
          )
        })
      )
    })
  })
  
  # Gestion des clics sur les cellules de la grille
  observeEvent(input$cell_click, {
    i <- input$cell_click$i  # Récupération de l'indice de la ligne
    j <- input$cell_click$j  # Récupération de l'indice de la colonne
    
    matrice_data <- matrice()
    
    # Changement cyclique de la valeur de la cellule entre "", "0" et "1"
    if (matrice_data[i, j] == "") {
      matrice_data[i, j] <- "0"
    } else if (matrice_data[i, j] == "0") {
      matrice_data[i, j] <- "1"
    } else {
      matrice_data[i, j] <- "0"
    }
    matrice(matrice_data)  # Mise à jour de la grille avec la nouvelle valeur
  })
  
  # Vérification de la solution lorsque l'utilisateur appuie sur "check"
  observeEvent(input$check, {
    matrice_data <- matrice()
    valid <- TRUE
    message <- ""
    
    # Vérification que chaque ligne et colonne contiennent autant de 0 que de 1
    for (i in 1:taille_matrice) {
      row <- matrice_data[i, ]
      col <- matrice_data[, i]
      if (sum(row == "0") != sum(row == "1") || sum(col == "0") != sum(col == "1")) {
        valid <- FALSE
        message <- "Erreur : une ligne ou une colonne n'a pas un nombre égal de 0 et de 1."
        break
      }
    }
    
    if (valid) {
      # Vérification qu'il n'y a pas plus de deux chiffres identiques consécutifs
      for (i in 1:taille_matrice) {
        for (j in 1:(taille_matrice - 2)) {
          # Vérification dans les lignes
          if (matrice_data[i, j] != "" && matrice_data[i, j] == matrice_data[i, j + 1] && matrice_data[i, j] == matrice_data[i, j + 2]) {
            valid <- FALSE
            message <- "Erreur : Plus de deux chiffres identiques sont adjacents."
            break
          }
          # Vérification dans les colonnes
          if (matrice_data[j, i] != "" && matrice_data[j, i] == matrice_data[j + 1, i] && matrice_data[j, i] == matrice_data[j + 2, i]) {
            valid <- FALSE
            message <- "Erreur : Plus de deux chiffres identiques sont adjacents."
            break
          }
        }
      }
    }
    
    # Vérification que toutes les lignes et colonnes sont uniques
    if (valid) {
      rows <- apply(matrice_data, 1, paste, collapse = "")
      cols <- apply(matrice_data, 2, paste, collapse = "")
      if (any(duplicated(rows)) || any(duplicated(cols))) {
        valid <- FALSE
        message <- "Erreur : Certaines lignes ou colonnes ne sont pas uniques."
      }
    }
    
    # Affichage du message final selon la validité de la solution
    if (valid) {
      message <- "Félicitations ! La solution est correcte."
    }
    output$message <- renderText(message)
  })
}

# Fonction pour démarrer l'application
run_app <- function() {
  shinyApp(ui = ui, server = server)
}

