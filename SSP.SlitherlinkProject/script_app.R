library(shiny)

### Partie UI
ui <- fluidPage(
  titlePanel("Jeu Slitherlink"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "grid_size",
        label    = "Taille de la grille :",
        choices  = c("5 x 5"   = 5,
                     "7 x 7"   = 7,
                     "10 x 10" = 10),
        selected = 5
      ),
      actionButton("start_game",    "Commencer une partie"),
      br(), br(),
      actionButton("validate_grid", "Valider la grille"),
      br(), br(),
      h4("Résultat :"),
      textOutput("game_result"),
    ),
    mainPanel(
      plotOutput("gridPlot", height = "500px", click = "plot_click")
    )
  )
)

### Partie Server
server <- function(input, output, session) {

  # --- Valeurs réactives (stockage côté app) ---
  result_text  <- reactiveVal("")
  click_info   <- reactiveVal("Aucun")
  puzzle_data  <- reactiveVal(NULL)   # solution + indices  (depuis generate_puzzle)
  player_state <- reactiveVal(NULL)   # état courant du joueur (depuis init_player_state)

  # --- Sorties texte ---
  output$game_result <- renderText({ result_text() })
  output$click_info  <- renderText({ click_info()  })

  # --- Nouvelle partie ---
  observeEvent(input$start_game, {
    n <- as.numeric(input$grid_size)
    puzzle_data(generate_puzzle(n))        # génération du puzzle (functions)
    player_state(init_player_state(n))     # initialisation de l'état joueur (functions)
    result_text("Nouvelle partie lancée !")
    click_info("Aucun")
  })

  # --- Clic sur la grille ---
  observeEvent(input$plot_click, {
    req(player_state(), puzzle_data())
    n     <- as.numeric(input$grid_size)
    click <- input$plot_click              # contient $x et $y

    seg <- nearest_segment(click$x, click$y, n)   # identification du segment (functions)
    if (is.null(seg)) return()

    player_state(toggle_segment(player_state(), seg))  # toggle (functions)
    click_info(describe_segment(seg, player_state()))  # description lisible (functions)
  })

  # --- Validation ---
  observeEvent(input$validate_grid, {
    req(player_state(), puzzle_data())
    result_text(check_solution(player_state(), puzzle_data()))  # vérification (functions)
  })

  # --- Rendu de la grille ---
  output$gridPlot <- renderPlot({
    req(puzzle_data())
    state <- player_state()

    # Si la partie n'a pas encore démarré : grille vide
    if (is.null(state)) state <- init_player_state(as.numeric(input$grid_size))

    draw_grid(
      h_mat = state$h,
      v_mat = state$v,
      clues = puzzle_data()$clues
    )
  })
}

shinyApp(ui = ui, server = server)
