library(shiny)

###
# Une appli shiny se découpe en trois parties :
# La partie UI qui gère toute la partie affichage
# La partie Server qui fait le lien entre affichage et scripts
###

### Partie UI
ui <- fluidPage(
  titlePanel("Jeu Slitherlink"),
  sidebarLayout(
    sidebarPanel(
      # Choix de la taille de la grille
      selectInput(
        inputId = "grid_size",
        label   = "Taille de la grille :",
        choices  = c("5 x 5"   = 5,
                     "7 x 7"   = 7,
                     "10 x 10" = 10),
        selected = 5
      ),
      # Bouton démarrer
      actionButton(
        inputId = "start_game",
        label   = "Commencer une partie"
      ),
      br(), br(),
      # Bouton validation
      actionButton(
        inputId = "validate_grid",
        label   = "Valider la grille"
      ),
      br(), br(),
      h4("Résultat :"),
      textOutput("game_result")
    ),
    mainPanel(
      plotOutput("gridPlot", height = "500px")
    )
  )
)

### Partie Server
server <- function(input, output, session) {

  result_text <- reactiveVal("")

  output$game_result <- renderText({
    result_text()
  })

  observeEvent(input$start_game, {
    result_text("Nouvelle partie lancée !")
  })

  observeEvent(input$validate_grid, {
    result_text("Validation en cours...")
  })

  # ----------------------------------------------------------------
  # Grille réactive : on appelle generate_puzzle() du code source
  # qui renvoie une liste avec $h, $v et $clues
  # ----------------------------------------------------------------
  grid_data <- eventReactive(input$start_game, {
    n <- as.numeric(input$grid_size)   # taille choisie dans le menu
    generate_puzzle(n)                 # renvoie list(h, v, clues)
  })

  # ----------------------------------------------------------------
  # Rendu de la grille : on passe h, v ET clues à draw_grid()
  # ----------------------------------------------------------------
  output$gridPlot <- renderPlot({
    req(grid_data())                   # attend qu'une partie soit lancée
    puzzle <- grid_data()
    draw_grid(
      h_mat  = puzzle$h,
      v_mat  = puzzle$v,
      clues  = puzzle$clues           # affiche les indices dans les cellules
    )
  })
}

### Lancement de l'application
shinyApp(ui = ui, server = server)
