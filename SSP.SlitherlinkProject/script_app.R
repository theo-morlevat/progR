library(shiny)   # charge le package Shiny pour construire l'application web

# =============================================================
# Une application Shiny se compose de trois parties :
#   1. ui     : décrit l'interface affichée dans le navigateur
#   2. server : fait le lien entre l'interface et la logique métier
#   3. shinyApp() : assemble et lance l'application
#
# Ce fichier ne contient PAS de logique métier : tous les calculs
# sont délégués aux fonctions de slitherlink_functions.R
# =============================================================


# -------------------------------------------------------------
# Partie UI — interface utilisateur
# -------------------------------------------------------------
# fluidPage() crée une page qui s'adapte à la largeur du navigateur
ui <- fluidPage(

  titlePanel("Jeu Slitherlink"),    # titre affiché en haut de la page

  # sidebarLayout() divise la page en deux zones :
  #   - sidebarPanel : panneau latéral gauche (contrôles)
  #   - mainPanel    : zone principale droite (grille de jeu)
  sidebarLayout(

    sidebarPanel(

      # --- Menu déroulant : taille de la grille ---
      # inputId = "grid_size" : identifiant récupéré côté server via input$grid_size
      # La valeur transmise est le nombre n (5, 7 ou 10), pas le texte affiché
      selectInput(
        inputId  = "grid_size",
        label    = "Taille de la grille :",
        choices  = c("5 x 5"   = 5,
                     "7 x 7"   = 7,
                     "10 x 10" = 10),
        selected = 5              # valeur sélectionnée par défaut au démarrage
      ),

      # --- Menu déroulant : difficulté ---
      # La valeur transmise est la chaîne "facile", "moyen" ou "difficile"
      # Elle est passée à generate_puzzle() qui la convertit en probabilité de masquage
      selectInput(
        inputId  = "difficulty",
        label    = "Difficulté :",
        choices  = c("Facile"    = "facile",
                     "Moyen"     = "moyen",
                     "Difficile" = "difficile"),
        selected = "moyen"        # difficulté par défaut
      ),

      # --- Bouton : démarrer une nouvelle partie ---
      # Déclenche observeEvent(input$start_game) dans le server
      actionButton("start_game", "Commencer une partie"),

      br(), br(),                 # sauts de ligne pour l'espacement visuel

      # --- Zone de résultat ---
      h4("Résultat :"),           # sous-titre de section
      # textOutput() affiche la valeur de result_text() mise à jour par le server
      textOutput("game_result")
    ),

    mainPanel(

      # --- Zone d'affichage de la grille ---
      # outputId = "gridPlot" : relié à output$gridPlot dans le server
      # height    : hauteur fixe du plot en pixels
      # click     : active la capture des clics souris sur le plot ;
      #             les coordonnées (x, y) seront accessibles via input$plot_click
      plotOutput("gridPlot", height = "500px", click = "plot_click")
    )
  )
)


# -------------------------------------------------------------
# Partie Server — logique réactive
# -------------------------------------------------------------
# input   : liste en lecture seule des valeurs issues de l'UI (menus, boutons, clics)
# output  : liste dans laquelle on écrit les sorties vers l'UI (textes, plots)
# session : objet de session (non utilisé ici mais requis par Shiny)
server <- function(input, output, session) {

  # --- Valeurs réactives : stockage de l'état de l'application ---
  # reactiveVal() crée une variable dont la modification déclenche
  # automatiquement la mise à jour de tous les éléments qui en dépendent.

  result_text  <- reactiveVal("")      # texte affiché dans la zone "Résultat"
  puzzle_data  <- reactiveVal(NULL)    # puzzle courant : list(h, v, clues)
  player_state <- reactiveVal(NULL)    # état joueur    : list(h, v) des segments tracés

  # --- Rendu du texte de résultat ---
  # renderText() réévalue son contenu chaque fois que result_text() change
  output$game_result <- renderText({ result_text() })


  # --- Démarrage d'une nouvelle partie ---
  # observeEvent() s'exécute chaque fois que le bouton "start_game" est cliqué
  observeEvent(input$start_game, {

    n          <- as.numeric(input$grid_size)   # récupère la taille choisie (5, 7 ou 10)
    difficulty <- input$difficulty              # récupère la difficulté choisie

    puzzle_data(generate_puzzle(n, difficulty)) # génère le puzzle (solution + indices masqués)
    player_state(init_player_state(n))          # initialise la grille joueur à zéro
    result_text("Nouvelle partie lancée !")     # message d'information
  })


  # --- Gestion d'un clic sur la grille ---
  # S'exécute chaque fois que le joueur clique dans la zone du plot
  observeEvent(input$plot_click, {

    # req() interrompt l'exécution si les valeurs sont NULL
    # (protection contre un clic avant le démarrage d'une partie)
    req(player_state(), puzzle_data())

    n     <- as.numeric(input$grid_size)   # taille de la grille active
    click <- input$plot_click              # coordonnées du clic : $x et $y en unités grille

    # Identifie le segment le plus proche du clic (ou NULL si trop loin)
    seg <- nearest_segment(click$x, click$y, n)
    if (is.null(seg)) return()             # clic hors zone valide : on ignore

    # Bascule l'état du segment (0 -> 1 tracé, ou 1 -> 0 effacé)
    new_state <- toggle_segment(player_state(), seg)
    player_state(new_state)               # met à jour l'état -> redessine le plot

    # Vérifie automatiquement la victoire après chaque coup
    # is_victory() contrôle les indices ET la boucle fermée unique
    if (is_victory(new_state, puzzle_data())) {
      result_text("Felicitations, vous avez resolu le puzzle !")
    }
  })


  # --- Rendu de la grille ---
  # renderPlot() réévalue et redessine la grille chaque fois que
  # puzzle_data() ou player_state() change
  output$gridPlot <- renderPlot({

    req(puzzle_data())    # ne rien dessiner tant qu'aucune partie n'est lancée

    state <- player_state()

    # Si le joueur n'a pas encore joué (state NULL), afficher une grille vierge
    # plutôt que de laisser le plot vide
    if (is.null(state)) state <- init_player_state(as.numeric(input$grid_size))

    # Appel à draw_grid() avec les segments du joueur et les indices du puzzle
    draw_grid(
      h_mat = state$h,            # segments horizontaux tracés par le joueur
      v_mat = state$v,            # segments verticaux   tracés par le joueur
      clues = puzzle_data()$clues # indices visibles selon la difficulté choisie
    )
  })
}


# -------------------------------------------------------------
# Lancement de l'application
# -------------------------------------------------------------
# Assemble ui et server puis démarre le serveur web local
shinyApp(ui = ui, server = server)
