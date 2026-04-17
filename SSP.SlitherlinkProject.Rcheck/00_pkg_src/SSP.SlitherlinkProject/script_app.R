library(shiny)
# On charge les fonctions du package (init_game, etc.)

# -----------------------------
# Génération d'une solution simple (contour)
# -----------------------------
generate_border_loop <- function(n) {

  h <- matrix(0, nrow = n + 1, ncol = n)
  v <- matrix(0, nrow = n, ncol = n + 1)

  # Haut et bas
  h[1, ] <- 1
  h[n+1, ] <- 1

  # Gauche et droite
  v[, 1] <- 1
  v[, n+1] <- 1

  list(h = h, v = v)
}

# -----------------------------
# Calcul des indices
# -----------------------------
compute_clues <- function(h, v) {

  n <- nrow(v)
  m <- ncol(h)

  clues <- matrix(0, nrow = n, ncol = m)

  for (i in 1:n) {
    for (j in 1:m) {
      clues[i, j] <-
        h[i, j] +
        h[i+1, j] +
        v[i, j] +
        v[i, j+1]
    }
  }

  clues
}

# -----------------------------
# Masquer certains indices
# -----------------------------
mask_clues <- function(clues, prob = 0.5) {

  mask <- matrix(runif(length(clues)) < prob,
                 nrow = nrow(clues))

  clues[mask] <- NA

  clues
}

# -----------------------------
# Pipeline complet
# -----------------------------
generate_puzzle <- function(n) {

  sol <- generate_border_loop(n)

  clues <- compute_clues(sol$h, sol$v)

  clues <- mask_clues(clues, 0.5)

  list(
    h = sol$h,
    v = sol$v,
    clues = clues
  )
}

ui <- fluidPage(
  titlePanel("Slitherlink - Master Bioinfo"),

  sidebarLayout(
    sidebarPanel(
      helpText("Règles : Reliez les points pour former une boucle unique."),
      # Un bouton pour réinitialiser la partie
      actionButton("reset", "Nouvelle Partie"),
      hr(),
      # Zone pour afficher si on a gagné
      textOutput("victoire_status")
    ),

    mainPanel(
      # Zone où on va dessiner la grille
      # On ajoute 'click' pour pouvoir capturer les clics de souris
      plotOutput("game_plot", click = "plot_click", height = "600px")
    )
  )
)

server <- function(input, output, session) {

  result_text <- reactiveVal("")

  output$game_result <- renderText({
    result_text()
  })

  # Stocker la grille courante
  current_grid <- reactiveVal(NULL)

  # ▶ Bouton "Commencer"
  observeEvent(input$start_game, {

    n <- as.numeric(input$grid_size)

    grid <- generate_puzzle(n)

    current_grid(grid)

    result_text("Nouvelle partie lancée !")
  })

  # Bouton validation
  observeEvent(input$validate_grid, {
    result_text("Validation en cours...")
  })

  # Affichage
  output$gridPlot <- renderPlot({

    grid <- current_grid()

    if (is.null(grid)) return()

    draw_grid(grid$h, grid$v, grid$clues)
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
