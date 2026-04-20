library(shiny)   # charge le package Shiny pour construire l'application web

# =============================================================
# Une application Shiny se compose de trois parties :
#le script suivant a été en partie créé avec l'aide d'un llm.
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

      # --- Bouton : effacer tous les traits ---
      # Réinitialise uniquement player_state à zéro sans toucher à puzzle_data
      # Déclenche observeEvent(input$clear_grid) dans le server
      actionButton("clear_grid", "Effacer les traits"),

      br(), br(),                 # sauts de ligne pour l'espacement visuel

      # --- Affichage du timer ---
      # textOutput("timer_display") est mis à jour toutes les secondes par invalidateLater()
      h4("Temps :"),
      textOutput("timer_display"),

      br(),

      # --- Affichage du compteur de coups ---
      # textOutput("moves_display") est mis à jour après chaque clic valide
      h4("Nombre de coups :"),
      textOutput("moves_display"),

      br(),

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

  # --- Timer : heure de démarrage de la partie ---
  # Proc.time() retourne le temps CPU et elapsed (temps réel écoulé depuis le lancement de R)
  # On stocke la valeur au moment du clic "Commencer" pour calculer la différence ensuite
  start_time   <- reactiveVal(NULL)    # heure de départ (NULL = pas de partie en cours)
  game_active  <- reactiveVal(FALSE)   # TRUE uniquement pendant une partie non terminée
  # FALSE après victoire pour figer le timer

  # --- Compteur de coups ---
  # Incrémenté à chaque clic valide (segment togglé), remis à 0 à chaque nouvelle partie
  move_count   <- reactiveVal(0L)


  # --- Rendu des sorties texte ---
  output$game_result  <- renderText({ result_text() })

  # Rendu du compteur de coups : affiche simplement la valeur entière courante
  output$moves_display <- renderText({
    move_count()   # se met à jour automatiquement quand move_count() change
  })

  # Rendu du timer : se rafraîchit toutes les secondes grâce à invalidateLater()
  output$timer_display <- renderText({

    # invalidateLater() force la réévaluation de ce bloc toutes les 1000 ms (1 seconde)
    # tant que la partie est active — sans déclencher de recalcul inutile après victoire
    if (game_active()) invalidateLater(1000, session)

    # Si aucune partie n'est lancée, on n'affiche rien
    if (is.null(start_time())) return("--:--")

    # Calcul du temps écoulé en secondes depuis le démarrage de la partie
    elapsed <- as.integer(difftime(Sys.time(), start_time(), units = "secs"))

    # Conversion en minutes et secondes pour un affichage MM:SS
    minutes <- elapsed %/% 60L           # division entière : nombre de minutes complètes
    seconds <- elapsed %%  60L           # modulo           : secondes restantes
    sprintf("%02d:%02d", minutes, seconds)   # formatage avec zéro devant si nécessaire
  })


  # --- Démarrage d'une nouvelle partie ---
  # observeEvent() s'exécute chaque fois que le bouton "start_game" est cliqué
  observeEvent(input$start_game, {

    n          <- as.numeric(input$grid_size)   # récupère la taille choisie (5, 7 ou 10)
    difficulty <- input$difficulty              # récupère la difficulté choisie

    puzzle_data(generate_puzzle(n, difficulty)) # génère le puzzle (solution + indices masqués)
    player_state(init_player_state(n))          # initialise la grille joueur à zéro

    # Réinitialisation du timer et du compteur pour la nouvelle partie
    start_time(Sys.time())    # enregistre l'heure exacte du démarrage
    game_active(TRUE)         # active le rafraîchissement du timer
    move_count(0L)            # remet le compteur de coups à zéro

    result_text("Nouvelle partie lancée !")
  })


  # --- Effacement de tous les traits ---
  # Réinitialise player_state à zéro sans regénérer le puzzle :
  # les indices et la solution sont conservés, seuls les traits du joueur sont effacés.
  # Le timer et le compteur de coups continuent : l'effacement compte comme une action.
  observeEvent(input$clear_grid, {

    req(puzzle_data())    # ne rien faire si aucune partie n'est en cours

    n <- as.numeric(input$grid_size)
    player_state(init_player_state(n))   # remet toutes les matrices joueur à zéro
    result_text("")                      # efface le message de résultat éventuel
    # Note : start_time et move_count ne sont PAS remis à zéro ici —
    # le timer et le compteur continuent pour refléter l'effort total du joueur
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
    if (is.null(seg)) return()             # clic hors zone valide : on ignore et on ne compte pas

    # Bascule l'état du segment (0 -> 1 tracé, ou 1 -> 0 effacé)
    new_state <- toggle_segment(player_state(), seg)
    player_state(new_state)               # met à jour l'état -> redessine le plot

    # Incrémente le compteur uniquement pour les clics valides (segment identifié)
    move_count(move_count() + 1L)

    # Vérifie automatiquement la victoire après chaque coup
    # is_victory() contrôle les indices ET la boucle fermée unique
    if (is_victory(new_state, puzzle_data())) {
      game_active(FALSE)    # fige le timer : invalidateLater() ne sera plus appelé

      # Calcul du temps final directement depuis start_time() —
      # on ne peut pas lire output$timer_display (écriture seule dans Shiny)
      elapsed <- as.integer(difftime(Sys.time(), start_time(), units = "secs"))
      minutes <- elapsed %/% 60L
      seconds <- elapsed %%  60L
      temps   <- sprintf("%02d:%02d", minutes, seconds)

      result_text(sprintf(
        "Felicitations ! Puzzle resolu en %s et %d coups.",
        temps,
        move_count()
      ))
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

    # Appel à draw_grid() :
    #   - h_player / v_player : segments tracés par le joueur (dessinés en bleu)
    #   - clues               : indices visibles selon la difficulté choisie
    #   - col_player          : couleur des segments joueur (steelblue par défaut)
    draw_grid(
      h_player   = state$h,              # segments horizontaux tracés par le joueur
      v_player   = state$v,              # segments verticaux   tracés par le joueur
      clues      = puzzle_data()$clues,  # indices visibles selon la difficulté choisie
      col_player = "steelblue"           # couleur distinctive pour les segments joueur
    )
  })
}


# -------------------------------------------------------------
# Lancement de l'application
# -------------------------------------------------------------
# Assemble ui et server puis démarre le serveur web local
shinyApp(ui = ui, server = server)
