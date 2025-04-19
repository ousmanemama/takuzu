# Chargement des bibliothèques nécessaires
library(shiny)     #
library(shinyjs) 

# Définir la taille de la grille Takuzu
taille_matrice <- 6  

# Vérifie si une ligne est valide (pas plus de 3 de chaque, pas 3 consécutifs)
ligne_valide_simple <- function(ligne) {
  if (sum(ligne == 0) > 3 || sum(ligne == 1) > 3) return(FALSE)
  for (i in 1:(length(ligne) - 2)) {
    if (!is.na(ligne[i]) && ligne[i] == ligne[i+1] && ligne[i] == ligne[i+2]) return(FALSE)
  }
  return(TRUE)
}

# Fonction de validation stricte (utilisée sur les lignes/colonnes)
ligne_valide_stricte <- function(vec) {
  sum(vec == "0") == 3 &&
    sum(vec == "1") == 3 &&
    !any(rle(vec)$lengths >= 3)  # pas 3 chiffres consécutifs
}

# Génère une grille complète et valide selon les règles Takuzu
generer_grille_complete_logique <- function(max_essais = 1000) {
  essais <- 0

  repeat {
    essais <- essais + 1
    grille <- matrix(NA, nrow = taille_matrice, ncol = taille_matrice)
    lignes_existantes <- list()

    for (i in 1:taille_matrice) {
      valide <- FALSE
      tentatives <- 0

      # Génération d'une ligne valide unique
      while (!valide && tentatives < 1000) {
        ligne <- sample(rep(0:1, 3))  # 3 zéros, 3 uns
        if (!ligne_valide_simple(ligne)) { tentatives <- tentatives + 1; next }
        if (any(sapply(lignes_existantes, function(l) all(l == ligne)))) {
          tentatives <- tentatives + 1; next
        }
        grille[i, ] <- ligne
        lignes_existantes[[length(lignes_existantes)+1]] <- ligne
        valide <- TRUE
      }
    }

    # Vérification globale de la grille
    grille_chr <- apply(grille, c(1, 2), as.character)
    lignes <- apply(grille_chr, 1, paste, collapse = "")
    colonnes <- apply(grille_chr, 2, paste, collapse = "")

    if (
      all(apply(grille_chr, 1, ligne_valide_stricte)) &&
      all(apply(grille_chr, 2, ligne_valide_stricte)) &&
      length(unique(lignes)) == taille_matrice &&
      length(unique(colonnes)) == taille_matrice
    ) {
      return(grille_chr)  # Grille valide
    }

    if (essais >= max_essais) {
      stop("Impossible de générer une grille valide après ", max_essais, " essais.")
    }
  }
}

# Crée une grille partiellement remplie en masquant des cases selon le niveau choisi
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
  titlePanel("Jeu de Takuzu"),
  sidebarLayout(
    sidebarPanel(
      selectInput("niveau", "Niveau de difficulté :", c("Facile", "Moyen", "Difficile"), "Moyen"),
      actionButton("reset", "Réinitialiser"),
      actionButton("check", "Vérifier la solution"),
      actionButton("reveler", "Afficher la solution"),
      hr(), br(), h4("⏱️ Temps écoulé :"),
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

  # Texte explicatif initial
  tips <- "Règles : 
  \n- chaque case doit être remplie avec un 0 ou un 1.
  \n- chaque ligne et chaque colonne doivent contenir autant de 0 que de 1.
  \n- il est interdit d’avoir trois 0 ou trois 1 consécutifs.
  \n- deux lignes ou deux colonnes identiques sont interdites."

  output$message <- renderText(tips)

  # Démarrage (pas de message d'erreur visible)
  observeEvent(TRUE, {
    tryCatch({
      niveaux <- grille_initiale("Moyen")
      matrice(niveaux$grille)
      solution_complete(niveaux$solution)
    }, error = function(e) {})
  }, once = TRUE)

  # Réinitialisation du jeu 
  observeEvent(input$reset, {
    tryCatch({
      niveaux <- grille_initiale(input$niveau)
      matrice(niveaux$grille)
      solution_complete(niveaux$solution)
      chronometre(Sys.time())
      output$message <- renderText(tips)
    }, error = function(e) {})
  })

  # Bouton : révéler la solution
  observeEvent(input$reveler, {
    matrice(solution_complete())
  })

  # Affichage du chrono en direct
  output$timer <- renderText({
    invalidateLater(1000, session)
    paste0(as.integer(difftime(Sys.time(), chronometre(), units = "secs")), " sec")
  })

  # Génération dynamique de la grille interactive
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

  # Gestion des clics sur les cellules
  observeEvent(input$cell_click, {
    i <- input$cell_click$i
    j <- input$cell_click$j
    grille <- matrice()
    grille[i, j] <- ifelse(grille[i, j] == "", "0", ifelse(grille[i, j] == "0", "1", ""))
    matrice(grille)
  })

  # Vérification de la grille proposée par l'utilisateur
  observeEvent(input$check, {
    grille <- matrice()
    valid <- TRUE
    message <- ""

    # Vérifie l'équilibre des lignes/colonnes
    for (i in 1:taille_matrice) {
      if (sum(grille[i, ] == "0") != 3 || sum(grille[i, ] == "1") != 3 ||
          sum(grille[, i] == "0") != 3 || sum(grille[, i] == "1") != 3) {
        valid <- FALSE; message <- " ❌ Déséquilibre ligne ou colonne."; break
      }
    }

    # Vérifie les répétitions interdites
    if (valid) {
      for (i in 1:taille_matrice) {
        for (j in 1:(taille_matrice - 2)) {
          if (grille[i, j] == grille[i, j+1] && grille[i, j+1] == grille[i, j+2] && grille[i, j] != "") {
            valid <- FALSE; message <- " 🚫 3 identiques dans une ligne."; break
          }
          if (grille[j, i] == grille[j+1, i] && grille[j+1, i] == grille[j+2, i] && grille[j, i] != "") {
            valid <- FALSE; message <- " ❗ 3 identiques dans une colonne."; break
          }
        }
      }
    }

    # Vérifie les duplications
    if (valid) {
      lignes <- apply(grille, 1, paste, collapse = "")
      colonnes <- apply(grille, 2, paste, collapse = "")
      if (any(duplicated(lignes)) || any(duplicated(colonnes))) {
        valid <- FALSE; message <- "❌ Lignes ou colonnes dupliquées."
      }
    }

    if (valid) message <- "✅ Bravo ! Grille correcte."
    output$message <- renderText(message)
  })
}
