# =============================================================
# slitherlink_functions.R
#le script suivant a été en partie créé avec l'aide d'un llm.
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
#   - Générateur de boucle aléatoire :
#       get_node_degree()           calcul du degré d'un noeud
#       get_neighbors()             liste des noeuds voisins d'un noeud
#       add_segment()               trace un segment entre deux noeuds
#       can_close_loop()            vérifie si la boucle peut être fermée
#       generate_random_loop()      algorithme principal Growing Path
#   - Calcul des indices          compute_clues()
#   - Masquage des indices        mask_clues()
#   - Pipeline complet            generate_puzzle()
# =============================================================


# -------------------------------------------------------------
# Dessin de la grille
# -------------------------------------------------------------
#' @param h_player   matrice (n+1) x n  — segments horizontaux tracés par le joueur
#' @param v_player   matrice n x (n+1)  — segments verticaux   tracés par le joueur
#' @param clues      matrice n x n      — indices par cellule (NA = caché), NULL par défaut
#' @param col_player couleur des segments tracés par le joueur (défaut : "steelblue")
#' @return           aucune valeur (effet de bord : dessin)
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
    sprintf("Horizontal  ligne=%d  col=%d  etat=%d", seg$i, seg$j, state$h[seg$i, seg$j])
  } else {
    sprintf("Vertical    ligne=%d  col=%d  etat=%d", seg$i, seg$j, state$v[seg$i, seg$j])
  }
}


# -------------------------------------------------------------
# Vérification manuelle (conservée pour usage futur)
# -------------------------------------------------------------
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
#' @param h  matrice (n+1) x n  — segments horizontaux du joueur
#' @param v  matrice n x (n+1)  — segments verticaux du joueur
#' @return   TRUE si les segments forment une boucle fermée unique, FALSE sinon
#' @export
is_single_loop <- function(h, v) {

  n        <- nrow(v)
  nb_nodes <- (n + 1L)^2

  node_id <- function(row, col) (row - 1L) * (n + 1L) + col

  degree    <- integer(nb_nodes)
  neighbors <- vector("list", nb_nodes)
  for (k in seq_len(nb_nodes)) neighbors[[k]] <- integer(0)

  add_edge <- function(r1, c1, r2, c2) {
    a <- node_id(r1, c1)
    b <- node_id(r2, c2)
    degree[a]      <<- degree[a] + 1L
    degree[b]      <<- degree[b] + 1L
    neighbors[[a]] <<- c(neighbors[[a]], b)
    neighbors[[b]] <<- c(neighbors[[b]], a)
  }

  for (i in 1:(n + 1))
    for (j in 1:n)
      if (h[i, j] == 1L) add_edge(i, j, i, j + 1L)

  for (i in 1:n)
    for (j in 1:(n + 1))
      if (v[i, j] == 1L) add_edge(i, j, i + 1L, j)

  active_nodes <- which(degree > 0L)
  if (length(active_nodes) == 0L) return(FALSE)
  if (any(degree[active_nodes] != 2L)) return(FALSE)

  visited        <- logical(nb_nodes)
  queue          <- active_nodes[1L]
  visited[queue] <- TRUE

  while (length(queue) > 0L) {
    current <- queue[1L]
    queue   <- queue[-1L]
    for (nb in neighbors[[current]]) {
      if (!visited[nb]) {
        visited[nb] <- TRUE
        queue <- c(queue, nb)
      }
    }
  }

  all(visited[active_nodes])
}


# -------------------------------------------------------------
# Détection automatique de victoire
# -------------------------------------------------------------
#' @param player  list(h, v) — état courant du joueur
#' @param puzzle  list(h, v, clues) — puzzle (seul $clues est utilisé)
#' @return        TRUE si le joueur a gagné, FALSE sinon
#' @export
is_victory <- function(player, puzzle) {

  h     <- player$h
  v     <- player$v
  clues <- puzzle$clues
  n     <- nrow(v)

  for (i in 1:n) {
    for (j in 1:n) {
      if (!is.na(clues[i, j])) {
        count <- h[i, j]      +
          h[i + 1L, j] +
          v[i, j]      +
          v[i, j + 1L]
        if (count != clues[i, j]) return(FALSE)
      }
    }
  }

  is_single_loop(h, v)
}


# =============================================================
# GÉNÉRATEUR DE BOUCLE ALÉATOIRE — Growing Path Algorithm
# =============================================================
#
# Principe général :
#   On construit la boucle pas à pas en marchant sur les noeuds
#   de la grille. À chaque étape, on choisit aléatoirement un
#   voisin valide et on trace le segment qui y mène.
#   On s'arrête dès qu'on peut revenir au noeud de départ pour
#   fermer la boucle proprement.
#
# Représentation des noeuds :
#   La grille a (n+1) x (n+1) noeuds, indicés (row, col)
#   avec row et col allant de 1 à n+1.
#   Chaque noeud est identifié par un entier unique :
#     id = (row - 1) * (n + 1) + col
#
# Contraintes respectées à chaque pas :
#   - Un segment ne peut être tracé qu'une seule fois
#   - Chaque noeud ne peut avoir au maximum que degré 2
#     (sinon la boucle se brancherait ou se croiserait)
#   - La boucle ne se ferme que si sa longueur dépasse
#     un seuil minimum (évite les micro-boucles triviales)
# =============================================================


# -------------------------------------------------------------
# Étape 1 — Calcul du degré d'un noeud
# -------------------------------------------------------------
#' Compte combien de segments actifs sont connectés au noeud (row, col).
#' Un noeud peut avoir degré 0 (isolé), 1 (dead-end) ou 2 (dans une boucle).
#' Le degré 3 ou 4 est interdit dans Slitherlink.
#'
#' @param row  ligne   du noeud (1 à n+1)
#' @param col  colonne du noeud (1 à n+1)
#' @param h    matrice (n+1) x n  — segments horizontaux
#' @param v    matrice n x (n+1)  — segments verticaux
#' @param n    taille de la grille
#' @return     entier : nombre de segments actifs touchant ce noeud (0 à 4)
#' @export
get_node_degree <- function(row, col, h, v, n) {

  deg <- 0L

  # Segment horizontal à gauche : relie (row, col-1) à (row, col)
  # Existe si col > 1  et  h[row, col-1] == 1
  if (col > 1    && h[row, col - 1] == 1L) deg <- deg + 1L

  # Segment horizontal à droite : relie (row, col) à (row, col+1)
  # Existe si col <= n  et  h[row, col] == 1
  if (col <= n   && h[row, col]     == 1L) deg <- deg + 1L

  # Segment vertical au-dessus : relie (row-1, col) à (row, col)
  # Existe si row > 1  et  v[row-1, col] == 1
  if (row > 1    && v[row - 1, col] == 1L) deg <- deg + 1L

  # Segment vertical en-dessous : relie (row, col) à (row+1, col)
  # Existe si row <= n  et  v[row, col] == 1
  if (row <= n   && v[row, col]     == 1L) deg <- deg + 1L

  deg
}


# -------------------------------------------------------------
# Étape 2 — Liste des voisins directs d'un noeud
# -------------------------------------------------------------
#' Retourne les quatre voisins potentiels du noeud (row, col)
#' qui sont accessibles par un segment (H ou V) non encore tracé
#' ET dont le degré actuel est strictement inférieur à 2.
#'
#' Ces deux conditions garantissent que :
#'   - on ne trace pas deux fois le même segment
#'   - on ne crée pas d'embranchement dans la boucle
#'
#' @param row      ligne   du noeud courant (1 à n+1)
#' @param col      colonne du noeud courant (1 à n+1)
#' @param h        matrice (n+1) x n  — segments horizontaux
#' @param v        matrice n x (n+1)  — segments verticaux
#' @param n        taille de la grille
#' @param visited  matrice (n+1) x (n+1) booléenne — noeuds déjà dans le chemin
#' @return         liste de voisins valides, chacun sous la forme list(row, col)
#' @export
get_neighbors <- function(row, col, h, v, n, visited) {

  candidates <- list()   # liste des voisins valides trouvés

  # --- Voisin à gauche : (row, col-1) ---
  # Conditions : col > 1 (pas sur le bord gauche)
  #              segment H[row, col-1] pas encore tracé
  #              voisin pas encore dans le chemin (visited)
  #              degré du voisin < 2 (peut encore recevoir un segment)
  if (col > 1 &&
      h[row, col - 1] == 0L &&
      !visited[row, col - 1] &&
      get_node_degree(row, col - 1, h, v, n) < 2L) {
    candidates <- c(candidates, list(list(row = row, col = col - 1L)))
  }

  # --- Voisin à droite : (row, col+1) ---
  if (col <= n &&
      h[row, col] == 0L &&
      !visited[row, col + 1] &&
      get_node_degree(row, col + 1, h, v, n) < 2L) {
    candidates <- c(candidates, list(list(row = row, col = col + 1L)))
  }

  # --- Voisin en haut : (row-1, col) ---
  if (row > 1 &&
      v[row - 1, col] == 0L &&
      !visited[row - 1, col] &&
      get_node_degree(row - 1, col, h, v, n) < 2L) {
    candidates <- c(candidates, list(list(row = row - 1L, col = col)))
  }

  # --- Voisin en bas : (row+1, col) ---
  if (row <= n &&
      v[row, col] == 0L &&
      !visited[row + 1, col] &&
      get_node_degree(row + 1, col, h, v, n) < 2L) {
    candidates <- c(candidates, list(list(row = row + 1L, col = col)))
  }

  candidates
}


# -------------------------------------------------------------
# Étape 3 — Tracer un segment entre deux noeuds adjacents
# -------------------------------------------------------------
#' Détermine si les deux noeuds sont voisins H ou V et active
#' le segment correspondant dans la bonne matrice.
#'
#' Les deux noeuds doivent être adjacents (distance 1 sur la grille).
#' Cette fonction est appelée après validation par get_neighbors().
#'
#' @param r1  ligne   du noeud source
#' @param c1  colonne du noeud source
#' @param r2  ligne   du noeud destination
#' @param c2  colonne du noeud destination
#' @param h   matrice (n+1) x n  — segments horizontaux (modifiée par référence <<-)
#' @param v   matrice n x (n+1)  — segments verticaux   (modifiée par référence <<-)
#' @return    aucune valeur (modifie h ou v dans l'environnement appelant)
#' @export
add_segment <- function(r1, c1, r2, c2, h, v) {

  if (r1 == r2) {
    # Même ligne -> segment HORIZONTAL
    # Le segment H[row, j] relie (row, j) à (row, j+1)
    # donc j = min(c1, c2)
    j        <- min(c1, c2)
    h[r1, j] <- 1L          # active le segment dans la matrice H
  } else {
    # Même colonne -> segment VERTICAL
    # Le segment V[i, col] relie (i, col) à (i+1, col)
    # donc i = min(r1, r2)
    i        <- min(r1, r2)
    v[i, c1] <- 1L          # active le segment dans la matrice V
  }

  list(h = h, v = v)        # retourne les deux matrices mises à jour
}


# -------------------------------------------------------------
# Étape 4 — Vérifier si la boucle peut être fermée
# -------------------------------------------------------------
#' Teste si un segment direct existe entre le noeud courant
#' et le noeud de départ, permettant de refermer la boucle.
#'
#' Conditions pour pouvoir fermer :
#'   1. Le segment reliant les deux noeuds n'est pas encore tracé
#'   2. Le noeud de départ a degré 1 (il attend exactement un segment)
#'      — il a reçu le premier segment au début du chemin
#'   3. Les deux noeuds sont adjacents (distance 1 sur la grille)
#'   4. La longueur du chemin dépasse le seuil minimum (min_length)
#'      — évite les boucles triviales de 3 ou 4 segments
#'
#' @param cur_row    ligne   du noeud courant
#' @param cur_col    colonne du noeud courant
#' @param start_row  ligne   du noeud de départ
#' @param start_col  colonne du noeud de départ
#' @param h          matrice (n+1) x n  — segments horizontaux
#' @param v          matrice n x (n+1)  — segments verticaux
#' @param n          taille de la grille
#' @param path_len   nombre de segments déjà tracés dans le chemin
#' @param min_length longueur minimale requise avant de fermer (défaut : 2*n)
#' @return           TRUE si la boucle peut être fermée, FALSE sinon
#' @export
can_close_loop <- function(cur_row, cur_col,
                           start_row, start_col,
                           h, v, n,
                           path_len, min_length) {

  # Condition 4 : chemin trop court, on interdit la fermeture
  if (path_len < min_length) return(FALSE)

  # Condition 3 : les deux noeuds doivent être adjacents (distance de Manhattan = 1)
  d_row <- abs(cur_row - start_row)
  d_col <- abs(cur_col - start_col)
  if (d_row + d_col != 1L) return(FALSE)   # pas adjacents : impossible de fermer

  # Condition 2 : le noeud de départ doit avoir degré 1
  # (il a reçu le tout premier segment et attend le dernier pour se fermer)
  if (get_node_degree(start_row, start_col, h, v, n) != 1L) return(FALSE)

  # Condition 1 : le segment reliant les deux noeuds ne doit pas déjà exister
  if (cur_row == start_row) {
    # Segment horizontal : H[cur_row, min(cur_col, start_col)]
    j <- min(cur_col, start_col)
    if (h[cur_row, j] == 1L) return(FALSE)   # segment déjà tracé
  } else {
    # Segment vertical : V[min(cur_row, start_row), cur_col]
    i <- min(cur_row, start_row)
    if (v[i, cur_col] == 1L) return(FALSE)   # segment déjà tracé
  }

  TRUE   # toutes les conditions sont remplies : on peut fermer
}


# -------------------------------------------------------------
# Étape 5 — Algorithme principal : Growing Path
# -------------------------------------------------------------
#' Génère une boucle fermée unique et aléatoire sur une grille n x n.
#'
#' Algorithme :
#'   1. Choisir un noeud de départ aléatoire
#'   2. Marquer ce noeud comme visité, l'ajouter au chemin
#'   3. Depuis le noeud courant :
#'      a. Si on peut fermer la boucle (can_close_loop) -> fermer et terminer
#'      b. Sinon, lister les voisins valides (get_neighbors)
#'      c. Si aucun voisin disponible -> ÉCHEC, recommencer (retry)
#'      d. Choisir un voisin au hasard, tracer le segment, avancer
#'   4. Recommencer jusqu'au succès (max_tries tentatives)
#'
#' @param n          taille de la grille (n x n cellules)
#' @param min_length longueur minimale de la boucle (défaut : 2*n)
#'                   Une valeur plus élevée produit des boucles plus longues
#'                   et donc des puzzles plus intéressants
#' @param max_tries  nombre maximal de tentatives avant d'abandonner (défaut : 1000)
#' @return           list(h, v) — matrices de la solution, ou erreur si échec
#' @export
generate_random_loop <- function(n, min_length = 2 * n, max_tries = 1000L) {

  for (attempt in seq_len(max_tries)) {

    # ----------------------------------------------------------
    # Initialisation d'une tentative
    # ----------------------------------------------------------

    # Matrices de segments toutes à zéro au départ
    h <- matrix(0L, nrow = n + 1, ncol = n)
    v <- matrix(0L, nrow = n, ncol = n + 1)

    # Matrice des noeuds visités : (n+1) x (n+1), tout à FALSE
    # Un noeud visité fait partie du chemin en cours de construction
    visited <- matrix(FALSE, nrow = n + 1, ncol = n + 1)

    # Choix aléatoire du noeud de départ parmi tous les noeuds de la grille
    start_row <- sample(seq_len(n + 1), 1L)
    start_col <- sample(seq_len(n + 1), 1L)

    # Initialisation du chemin : le noeud de départ est visité
    cur_row <- start_row
    cur_col <- start_col
    visited[cur_row, cur_col] <- TRUE
    path_len <- 0L   # nombre de segments tracés jusqu'ici

    success  <- FALSE   # deviendra TRUE si la boucle est fermée proprement

    # ----------------------------------------------------------
    # Boucle principale de construction du chemin
    # ----------------------------------------------------------
    repeat {

      # --- Tentative de fermeture de la boucle ---
      # On vérifie en priorité si on peut revenir au départ
      if (can_close_loop(cur_row, cur_col,
                         start_row, start_col,
                         h, v, n, path_len, min_length)) {

        # Fermeture : on trace le dernier segment vers le noeud de départ
        mats    <- add_segment(cur_row, cur_col, start_row, start_col, h, v)
        h       <- mats$h
        v       <- mats$v
        success <- TRUE
        break   # boucle fermée, on sort de la boucle repeat
      }

      # --- Recherche des voisins valides depuis le noeud courant ---
      neighbors <- get_neighbors(cur_row, cur_col, h, v, n, visited)

      # Si aucun voisin disponible : le chemin est bloqué, cette tentative échoue
      if (length(neighbors) == 0L) break   # on sortira avec success == FALSE

      # --- Choix aléatoire d'un voisin parmi les candidats ---
      idx  <- sample(seq_along(neighbors), 1L)   # indice aléatoire
      next_node <- neighbors[[idx]]              # noeud choisi

      # --- Tracé du segment vers le voisin choisi ---
      mats    <- add_segment(cur_row, cur_col,
                             next_node$row, next_node$col,
                             h, v)
      h       <- mats$h
      v       <- mats$v
      path_len <- path_len + 1L   # un segment de plus dans le chemin

      # --- Avancement vers le noeud suivant ---
      visited[next_node$row, next_node$col] <- TRUE   # marquer comme visité
      cur_row <- next_node$row
      cur_col <- next_node$col
    }

    # Si cette tentative a réussi, on retourne la solution
    if (success) return(list(h = h, v = v))
  }

  # Si toutes les tentatives ont échoué (rare), on lève une erreur
  stop(sprintf("generate_random_loop : echec apres %d tentatives.", max_tries))
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
      clues[i, j] <- h[i,      j    ] +   # segment horizontal du dessus
    h[i + 1L, j    ] +   # segment horizontal du dessous
    v[i,      j    ] +   # segment vertical de gauche
    v[i,      j + 1L]    # segment vertical de droite
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
# Calcul du seuil de longueur minimale de la boucle
# -------------------------------------------------------------
#' Détermine la longueur minimale requise pour la boucle générée,
#' en fonction de la taille de la grille.
#'
#' Objectif : forcer des boucles suffisamment grandes pour que le puzzle
#' soit intéressant. Le périmètre complet d'une grille n x n est 4*n segments.
#' On vise environ 60% de ce périmètre comme longueur minimale, avec un
#' plancher à 12 pour les petites grilles.
#'
#'   n =  5  ->  min_length = max(12, round(0.6 * 20)) = max(12, 12) = 12  (trop court)
#'            => on utilise directement les valeurs calibrées ci-dessous
#'
#' Valeurs calibrées :
#'   n =  5  ->  20  (périmètre = 20, on exige la totalité)
#'   n =  7  ->  30  (périmètre = 28, on exige légèrement plus que le périmètre)
#'   n = 10  ->  50  (périmètre = 40, on exige 125% du périmètre)
#'
#' Pour une valeur de n non listée, on utilise la formule : round(n * 4 * 0.8)
#'
#' @param n  taille de la grille
#' @return   entier : longueur minimale de la boucle
#' @export
compute_min_length <- function(n) {
  # Valeurs calibrées pour les trois tailles proposées dans l'UI
  switch(as.character(n),
         "5"  = 20L,    # 100% du périmètre : force une boucle couvrant toute la grille
         "7"  = 30L,    # ~107% du périmètre : boucle complexe et sinueuse
         "10" = 50L,    # 125% du périmètre : boucle longue et riche en indices
         round(n * 4L * 0.8)   # formule générique pour toute autre taille (80% du périmètre)
  )
}


# -------------------------------------------------------------
# Pipeline complet : génère un puzzle prêt à jouer
# -------------------------------------------------------------
#' Enchaîne generate_random_loop -> compute_clues -> mask_clues
#' en traduisant d'abord la difficulté en probabilité.
#'
#' Le seuil de longueur minimale est calculé par compute_min_length(n)
#' pour garantir des boucles suffisamment grandes selon la taille de grille.
#'
#' @param n           taille de la grille (n x n cellules)
#' @param difficulty  "facile", "moyen" ou "difficile" (défaut "moyen")
#' @return            list(h, v, clues)
#'                      $h, $v   : matrices de la solution aléatoire
#'                      $clues   : indices partiels visibles pour le joueur
#' @export
generate_puzzle <- function(n, difficulty = "moyen") {
  prob       <- difficulty_to_prob(difficulty)          # conversion difficulté -> probabilité
  min_length <- compute_min_length(n)                   # seuil calibré selon la taille
  sol        <- generate_random_loop(n, min_length)     # génération avec le bon seuil
  clues      <- compute_clues(sol$h, sol$v)             # calcul de tous les indices
  clues      <- mask_clues(clues, prob)                 # masquage selon la difficulté
  list(h = sol$h, v = sol$v, clues = clues)
}
