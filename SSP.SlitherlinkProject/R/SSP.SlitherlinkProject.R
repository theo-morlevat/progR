# =============================================================
# slitherlink_functions.R
# Contient toute la logique métier du jeu Slitherlink :
#   - Dessin de la grille         draw_grid()
#   - Initialisation joueur       init_player_state()
#   - Détection du segment cliqué nearest_segment()
#   - Toggle d'un segment         toggle_segment()
#   - Description d'un segment    describe_segment()
#   - Vérification manuelle       check_solution()
#   - Vérification boucle unique  is_single_loop()
#   - Détection de victoire       is_victory()
#   - Conversion difficulté       difficulty_to_prob()
#   - Génération contour          generate_border_loop()
#   - Calcul des indices          compute_clues()
#   - Masquage des indices        mask_clues()
#   - Pipeline complet            generate_puzzle()
# =============================================================


# -------------------------------------------------------------
# Dessin de la grille
# -------------------------------------------------------------
#' @param h_player  matrice (n+1) x n  — segments horizontaux tracés par le joueur
#' @param v_player  matrice n x (n+1)  — segments verticaux   tracés par le joueur
#' @param clues     matrice n x n      — indices par cellule (NA = caché), NULL par défaut
#' @param col_player couleur des segments tracés par le joueur (défaut : "steelblue")
#' @return          aucune valeur (effet de bord : dessin)
#' @export
draw_grid <- function(h_player, v_player, clues = NULL, col_player = "steelblue") {

  n_row <- nrow(v_player)    # nombre de lignes   de cellules (= n)
  n_col <- ncol(h_player)    # nombre de colonnes de cellules (= n)

  # Initialisation d'un plot vide aux dimensions de la grille
  plot(0, 0, type = "n",
       xlim = c(0, n_col), ylim = c(0, n_row),
       xlab = "", ylab = "", axes = FALSE, asp = 1)

  # --- Noeuds : points noirs aux intersections de la grille ---
  # i parcourt les colonnes (0 à n_col), j les lignes (0 à n_row)
  for (i in 0:n_col)
    for (j in 0:n_row)
      points(i, j, pch = 16, cex = 0.5)    # pch=16 : disque plein, cex=0.5 : petit

  # --- Segments horizontaux du joueur ---
  # H[i, j] relie le noeud (j-1, n_row-(i-1)) au noeud (j, n_row-(i-1))
  # i indexe la ligne de la matrice h_player (de haut en bas)
  # j indexe la colonne de la matrice h_player (de gauche à droite)
  for (i in 1:nrow(h_player))
    for (j in 1:ncol(h_player))
      if (h_player[i, j] == 1)                  # ne dessine que les segments actifs
        segments(j - 1, n_row - (i - 1),        # point de départ (x1, y1)
                 j,     n_row - (i - 1),        # point d'arrivée (x2, y2)
                 lwd = 2, col = col_player)      # couleur et épaisseur joueur

  # --- Segments verticaux du joueur ---
  # V[i, j] relie le noeud (j-1, n_row-i) au noeud (j-1, n_row-(i-1))
  for (i in 1:nrow(v_player))
    for (j in 1:ncol(v_player))
      if (v_player[i, j] == 1)                  # ne dessine que les segments actifs
        segments(j - 1, n_row - i,              # point de départ (x1, y1)
                 j - 1, n_row - (i - 1),        # point d'arrivée (x2, y2)
                 lwd = 2, col = col_player)      # couleur et épaisseur joueur

  # --- Indices dans les cellules ---
  # Le centre de la cellule (i, j) est en (j - 0.5, n_row - i + 0.5)
  if (!is.null(clues))
    for (i in 1:nrow(clues))
      for (j in 1:ncol(clues))
        if (!is.na(clues[i, j]))                # n'affiche que les indices non masqués
          text(j - 0.5, n_row - i + 0.5, clues[i, j], cex = 1.2)
}


# -------------------------------------------------------------
# Initialisation de l'état joueur
# -------------------------------------------------------------
#' @param n  taille de la grille (n x n cellules)
#' @return   list(h, v) — deux matrices de zéros
#' @export
init_player_state <- function(n) {
  list(
    h = matrix(0L, nrow = n + 1, ncol = n),    # (n+1) x n  : tous les segments H possibles
    v = matrix(0L, nrow = n, ncol = n + 1)     # n x (n+1)  : tous les segments V possibles
  )
}


# -------------------------------------------------------------
# Détection du segment le plus proche d'un clic
# -------------------------------------------------------------
#' Calcule le point milieu de chaque segment possible et retourne
#' celui dont le milieu est le plus proche du clic (si sous le seuil tol).
#'
#' Points milieux :
#'   H[i,j]  ->  (j - 0.5 ,  n - (i-1))
#'   V[i,j]  ->  (j - 1   ,  n - i + 0.5)
#'
#' @param x    coordonnée x du clic (espace grille)
#' @param y    coordonnée y du clic (espace grille)
#' @param n    taille de la grille
#' @param tol  distance maximale acceptée (défaut 0.3)
#' @return     list(type, i, j) ou NULL si aucun segment assez proche
#' @export
nearest_segment <- function(x, y, n, tol = 0.3) {

  best_dist <- Inf    # distance minimale trouvée jusqu'ici
  best_seg  <- NULL   # segment correspondant

  # --- Parcours de tous les segments horizontaux possibles ---
  for (i in 1:(n + 1))
    for (j in 1:n) {
      d <- sqrt((x - (j - 0.5))^2 + (y - (n - (i - 1)))^2)   # distance au milieu de H[i,j]
      if (d < best_dist) {
        best_dist <- d
        best_seg  <- list(type = "h", i = i, j = j)           # mémorise le candidat
      }
    }

  # --- Parcours de tous les segments verticaux possibles ---
  for (i in 1:n)
    for (j in 1:(n + 1)) {
      d <- sqrt((x - (j - 1))^2 + (y - (n - i + 0.5))^2)     # distance au milieu de V[i,j]
      if (d < best_dist) {
        best_dist <- d
        best_seg  <- list(type = "v", i = i, j = j)           # mémorise le candidat
      }
    }

  if (best_dist > tol) return(NULL)   # clic trop loin de tout segment : ignoré
  best_seg
}


# -------------------------------------------------------------
# Toggle d'un segment
# -------------------------------------------------------------
#' @param state  list(h, v) — état courant du joueur
#' @param seg    list(type, i, j) — segment à basculer
#' @return       list(h, v) mis à jour
#' @export
toggle_segment <- function(state, seg) {
  if (seg$type == "h") {
    state$h[seg$i, seg$j] <- 1L - state$h[seg$i, seg$j]   # 0 -> 1 ou 1 -> 0 (horizontal)
  } else {
    state$v[seg$i, seg$j] <- 1L - state$v[seg$i, seg$j]   # 0 -> 1 ou 1 -> 0 (vertical)
  }
  state   # retourne l'état mis à jour
}


# -------------------------------------------------------------
# Description lisible du dernier segment cliqué
# -------------------------------------------------------------
#' @param seg    list(type, i, j)
#' @param state  list(h, v) — état après toggle
#' @return       chaîne descriptive
#' @export
describe_segment <- function(seg, state) {
  if (seg$type == "h") {
    # Récupère l'état actuel du segment horizontal dans la matrice h
    sprintf("Horizontal  ligne=%d  col=%d  etat=%d", seg$i, seg$j, state$h[seg$i, seg$j])
  } else {
    # Récupère l'état actuel du segment vertical dans la matrice v
    sprintf("Vertical    ligne=%d  col=%d  etat=%d", seg$i, seg$j, state$v[seg$i, seg$j])
  }
}


# -------------------------------------------------------------
# Vérification manuelle (conservée pour usage futur)
# -------------------------------------------------------------
#' Compare bit à bit l'état joueur à la solution du puzzle.
#' Utilisée par le bouton "Valider" si réintroduit.
#'
#' @param player  list(h, v) — état joueur
#' @param puzzle  list(h, v, clues) — puzzle généré
#' @return        message de résultat
#' @export
check_solution <- function(player, puzzle) {
  if (identical(player$h, puzzle$h) && identical(player$v, puzzle$v)) {
    "Felicitations, la grille est correcte !"
  } else {
    "La grille n'est pas encore correcte."
  }
}


# -------------------------------------------------------------
# Vérification que les segments forment une boucle fermée unique
# -------------------------------------------------------------
#' Teste trois conditions nécessaires à la victoire :
#'   1. Au moins un segment est tracé
#'   2. Chaque nœud actif a exactement degré 2 (boucle sans embranchement ni dead-end)
#'   3. Tous les segments actifs forment un seul composant connexe (boucle unique)
#'
#' Représentation en graphe :
#'   Les nœuds sont les (n+1) x (n+1) intersections de la grille.
#'   Un nœud (row, col) est identifié par l'entier (row-1)*(n+1) + col (base 1).
#'   Les arêtes actives sont les segments H et V dont la valeur vaut 1.
#'
#' @param h  matrice (n+1) x n  — segments horizontaux du joueur
#' @param v  matrice n x (n+1) — segments verticaux du joueur
#' @return   TRUE si les segments forment une boucle fermée unique, FALSE sinon
#' @export
is_single_loop <- function(h, v) {

  n        <- nrow(v)                       # nombre de lignes   de cellules
  n_col    <- ncol(h)                       # nombre de colonnes de cellules
  nb_nodes <- (n + 1L)^2                    # nombre total de noeuds de la grille

  # Fonction de conversion (row, col) -> identifiant entier du noeud
  node_id <- function(row, col) (row - 1L) * (n + 1L) + col

  # --- Initialisation de la liste d'adjacence ---
  degree    <- integer(nb_nodes)            # degree[k] = nb de segments actifs au noeud k
  neighbors <- vector("list", nb_nodes)     # neighbors[[k]] = liste des voisins actifs de k
  for (k in seq_len(nb_nodes)) neighbors[[k]] <- integer(0)

  # Fonction interne : ajoute une arête entre les noeuds (r1,c1) et (r2,c2)
  add_edge <- function(r1, c1, r2, c2) {
    a <- node_id(r1, c1)
    b <- node_id(r2, c2)
    degree[a]      <<- degree[a] + 1L          # incrémente le degré des deux noeuds
    degree[b]      <<- degree[b] + 1L
    neighbors[[a]] <<- c(neighbors[[a]], b)     # enregistre la relation de voisinage
    neighbors[[b]] <<- c(neighbors[[b]], a)
  }

  # --- Construction du graphe à partir des segments actifs ---

  # Arêtes horizontales : H[i, j] relie le noeud (i, j) au noeud (i, j+1)
  for (i in 1:(n + 1))
    for (j in 1:n)
      if (h[i, j] == 1L) add_edge(i, j, i, j + 1L)

  # Arêtes verticales : V[i, j] relie le noeud (i, j) au noeud (i+1, j)
  for (i in 1:n)
    for (j in 1:(n + 1))
      if (v[i, j] == 1L) add_edge(i, j, i + 1L, j)

  # --- Condition 1 : au moins un segment tracé ---
  active_nodes <- which(degree > 0L)        # noeuds touchés par au moins un segment
  if (length(active_nodes) == 0L) return(FALSE)

  # --- Condition 2 : chaque noeud actif a exactement degré 2 ---
  # degré < 2 : dead-end (segment sans suite)
  # degré > 2 : embranchement (plusieurs boucles se croisent)
  if (any(degree[active_nodes] != 2L)) return(FALSE)

  # --- Condition 3 : un seul composant connexe ---
  # BFS depuis le premier noeud actif : tous les noeuds actifs doivent être atteints
  visited        <- logical(nb_nodes)
  queue          <- active_nodes[1L]        # point de départ du BFS
  visited[queue] <- TRUE

  while (length(queue) > 0L) {
    current <- queue[1L]                    # noeud courant
    queue   <- queue[-1L]                   # retire le noeud courant de la file
    for (nb in neighbors[[current]]) {
      if (!visited[nb]) {
        visited[nb] <- TRUE
        queue <- c(queue, nb)               # ajoute le voisin non visité à la file
      }
    }
  }

  # Si un noeud actif n'a pas été visité, la boucle est fragmentée
  all(visited[active_nodes])
}


# -------------------------------------------------------------
# Détection automatique de victoire après chaque coup
# -------------------------------------------------------------
#' Vérifie si le joueur a gagné en combinant deux conditions :
#'   1. Tous les indices visibles (clues non-NA) sont respectés
#'   2. Les segments forment une boucle fermée unique (is_single_loop)
#'
#' N'utilise PAS la solution stockée : raisonne sur les règles du jeu.
#' Une solution alternative valide est donc aussi acceptée.
#'
#' @param player  list(h, v) — état courant du joueur
#' @param puzzle  list(h, v, clues) — puzzle (seul $clues est utilisé)
#' @return        TRUE si le joueur a gagné, FALSE sinon
#' @export
is_victory <- function(player, puzzle) {

  h     <- player$h
  v     <- player$v
  clues <- puzzle$clues
  n     <- nrow(v)           # taille de la grille

  # --- Condition 1 : respect de tous les indices visibles ---
  for (i in 1:n) {
    for (j in 1:n) {
      if (!is.na(clues[i, j])) {
        # Somme des 4 segments bordant la cellule (i, j)
        count <- h[i, j]      +   # segment horizontal du dessus
          h[i + 1L, j] +   # segment horizontal du dessous
          v[i, j]      +   # segment vertical de gauche
          v[i, j + 1L]     # segment vertical de droite
        if (count != clues[i, j]) return(FALSE)   # indice non respecté : pas de victoire
      }
    }
  }

  # --- Condition 2 : les segments forment une boucle fermée unique ---
  is_single_loop(h, v)
}


# -------------------------------------------------------------
# Conversion difficulté -> probabilité de masquage
# -------------------------------------------------------------
#' Plus la probabilité est élevée, plus d'indices sont masqués (= plus difficile) :
#'   facile    -> 0.25  (75% des indices visibles)
#'   moyen     -> 0.50  (50% des indices visibles)
#'   difficile -> 0.80  (20% des indices visibles)
#'
#' @param difficulty  "facile", "moyen" ou "difficile"
#' @return            probabilité numérique (entre 0 et 1)
#' @export
difficulty_to_prob <- function(difficulty) {
  switch(difficulty,
         facile    = 0.25,
         moyen     = 0.50,
         difficile = 0.80,
         stop(sprintf("Difficulte inconnue : '%s'. Valeurs acceptees : facile, moyen, difficile.",
                      difficulty))
  )
}


# -------------------------------------------------------------
# Génération d'une solution simple (boucle sur le contour)
# -------------------------------------------------------------
#' Trace un rectangle sur le pourtour de la grille.
#' Sert de solution de base avant l'implémentation d'un générateur aléatoire.
#'
#' @param n  taille de la grille (n x n cellules)
#' @return   list(h, v) — matrices de la solution
#' @export
generate_border_loop <- function(n) {
  h <- matrix(0L, nrow = n + 1, ncol = n)    # tous les segments H initialisés à 0
  v <- matrix(0L, nrow = n, ncol = n + 1)    # tous les segments V initialisés à 0

  h[1,     ] <- 1L    # ligne du haut    : tous les segments H de la première ligne
  h[n + 1, ] <- 1L    # ligne du bas     : tous les segments H de la dernière ligne
  v[, 1    ] <- 1L    # colonne gauche   : tous les segments V de la première colonne
  v[, n + 1] <- 1L    # colonne droite   : tous les segments V de la dernière colonne

  list(h = h, v = v)
}


# -------------------------------------------------------------
# Calcul des indices (nombre de segments autour de chaque cellule)
# -------------------------------------------------------------
#' Pour chaque cellule (i, j), compte les 4 segments qui l'entourent.
#'
#' @param h  matrice (n+1) x n — segments horizontaux
#' @param v  matrice n x (n+1) — segments verticaux
#' @return   matrice n x n des indices (valeurs de 0 à 4)
#' @export
compute_clues <- function(h, v) {
  n     <- nrow(v)                          # nombre de lignes   de cellules
  m     <- ncol(h)                          # nombre de colonnes de cellules
  clues <- matrix(0L, nrow = n, ncol = m)  # matrice résultat initialisée à 0

  for (i in 1:n)
    for (j in 1:m)
      clues[i, j] <- h[i,     j    ] +   # segment horizontal du dessus
    h[i + 1L, j   ] +   # segment horizontal du dessous
    v[i,     j    ] +   # segment vertical de gauche
    v[i,     j + 1L]    # segment vertical de droite
  clues
}


# -------------------------------------------------------------
# Masquage aléatoire des indices
# -------------------------------------------------------------
#' Remplace aléatoirement certains indices par NA selon la probabilité prob.
#' Un indice masqué est invisible pour le joueur (case sans chiffre).
#'
#' @param clues  matrice n x n des indices calculés
#' @param prob   probabilité de masquer chaque indice (0 = tout visible, 1 = tout masqué)
#' @return       même matrice avec certaines valeurs remplacées par NA
#' @export
mask_clues <- function(clues, prob) {
  # Génère une matrice booléenne : TRUE = cet indice sera masqué
  mask        <- matrix(runif(length(clues)) < prob, nrow = nrow(clues))
  clues[mask] <- NA    # remplace les cases sélectionnées par NA
  clues
}


# -------------------------------------------------------------
# Pipeline complet : génère un puzzle prêt à jouer
# -------------------------------------------------------------
#' Enchaîne generate_border_loop -> compute_clues -> mask_clues
#' en traduisant d'abord la difficulté en probabilité.
#'
#' @param n           taille de la grille (n x n cellules)
#' @param difficulty  "facile", "moyen" ou "difficile" (défaut "moyen")
#' @return            list(h, v, clues)
#'                      $h, $v   : matrices de la solution
#'                      $clues   : indices partiels visibles pour le joueur
#' @export
generate_puzzle <- function(n, difficulty = "moyen") {
  prob  <- difficulty_to_prob(difficulty)   # conversion difficulté -> probabilité
  sol   <- generate_border_loop(n)          # génération de la solution
  clues <- compute_clues(sol$h, sol$v)      # calcul de tous les indices
  clues <- mask_clues(clues, prob)          # masquage selon la difficulté
  list(h = sol$h, v = sol$v, clues = clues)
}
