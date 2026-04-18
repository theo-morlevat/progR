# =============================================================
# slitherlink_functions.R
# Contient : toute la logique métier du jeu Slitherlink
#   - Dessin de la grille         draw_grid()
#   - Initialisation joueur       init_player_state()
#   - Détection du segment cliqué nearest_segment()
#   - Toggle d'un segment         toggle_segment()
#   - Description d'un segment    describe_segment()
#   - Vérification de la solution check_solution()
#   - Génération du puzzle        generate_puzzle() et ses sous-fonctions
# =============================================================


# -------------------------------------------------------------
# Dessin de la grille
# -------------------------------------------------------------
#' @param h_mat  matrice (n+1) x n  — segments horizontaux (0 = absent, 1 = tracé)
#' @param v_mat  matrice n x (n+1)  — segments verticaux   (0 = absent, 1 = tracé)
#' @param clues  matrice n x n      — indices par cellule (NA = caché), NULL par défaut
#' @return       aucune valeur (effet de bord : dessin)
#' @export
draw_grid <- function(h_mat, v_mat, clues = NULL) {

  n_row <- nrow(v_mat)
  n_col <- ncol(h_mat)

  plot(0, 0, type = "n",
       xlim = c(0, n_col), ylim = c(0, n_row),
       xlab = "", ylab = "", axes = FALSE, asp = 1)

  # Points (noeuds)
  for (i in 0:n_col)
    for (j in 0:n_row)
      points(i, j, pch = 16, cex = 0.5)

  # Segments horizontaux : H[i,j] relie (j-1, n_row-(i-1)) à (j, n_row-(i-1))
  for (i in 1:nrow(h_mat))
    for (j in 1:ncol(h_mat))
      if (h_mat[i, j] == 1)
        segments(j - 1, n_row - (i - 1), j, n_row - (i - 1), lwd = 2)

  # Segments verticaux : V[i,j] relie (j-1, n_row-i) à (j-1, n_row-(i-1))
  for (i in 1:nrow(v_mat))
    for (j in 1:ncol(v_mat))
      if (v_mat[i, j] == 1)
        segments(j - 1, n_row - i, j - 1, n_row - (i - 1), lwd = 2)

  # Indices dans les cellules
  if (!is.null(clues))
    for (i in 1:nrow(clues))
      for (j in 1:ncol(clues))
        if (!is.na(clues[i, j]))
          text(j - 0.5, n_row - i + 0.5, clues[i, j], cex = 1.2)
}


# -------------------------------------------------------------
# Initialisation de l'état joueur
# -------------------------------------------------------------
#' Crée deux matrices de zéros représentant une grille vierge côté joueur
#'
#' @param n  taille de la grille (n x n cellules)
#' @return   list(h, v) avec h matrice (n+1) x n et v matrice n x (n+1)
#' @export
init_player_state <- function(n) {
  list(
    h = matrix(0L, nrow = n + 1, ncol = n),     # segments horizontaux joueur
    v = matrix(0L, nrow = n, ncol = n + 1)      # segments verticaux joueur
  )
}


# -------------------------------------------------------------
# Détection du segment le plus proche d'un clic
# -------------------------------------------------------------
#' Trouve le segment H ou V dont le milieu est le plus proche de (x, y)
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

  best_dist <- Inf
  best_seg  <- NULL

  # Candidats horizontaux
  for (i in 1:(n + 1)) {
    for (j in 1:n) {
      d <- sqrt((x - (j - 0.5))^2 + (y - (n - (i - 1)))^2)
      if (d < best_dist) {
        best_dist <- d
        best_seg  <- list(type = "h", i = i, j = j)
      }
    }
  }

  # Candidats verticaux
  for (i in 1:n) {
    for (j in 1:(n + 1)) {
      d <- sqrt((x - (j - 1))^2 + (y - (n - i + 0.5))^2)
      if (d < best_dist) {
        best_dist <- d
        best_seg  <- list(type = "v", i = i, j = j)
      }
    }
  }

  if (best_dist > tol) return(NULL)
  best_seg
}


# -------------------------------------------------------------
# Toggle d'un segment dans l'état joueur
# -------------------------------------------------------------
#' Bascule l'état (0 -> 1 ou 1 -> 0) du segment identifié par seg
#'
#' @param state  list(h, v) — état courant du joueur
#' @param seg    list(type, i, j) — segment à basculer (résultat de nearest_segment)
#' @return       list(h, v) mis à jour
#' @export
toggle_segment <- function(state, seg) {
  if (seg$type == "h") {
    state$h[seg$i, seg$j] <- 1L - state$h[seg$i, seg$j]
  } else {
    state$v[seg$i, seg$j] <- 1L - state$v[seg$i, seg$j]
  }
  state
}


# -------------------------------------------------------------
# Description lisible du dernier segment cliqué (pour l'UI)
# -------------------------------------------------------------
#' @param seg    list(type, i, j) — segment ciblé
#' @param state  list(h, v) — état joueur après toggle
#' @return       chaîne de caractères descriptive
#' @export
describe_segment <- function(seg, state) {
  if (seg$type == "h") {
    val <- state$h[seg$i, seg$j]
    sprintf("Horizontal  ligne=%d  col=%d  etat=%d", seg$i, seg$j, val)
  } else {
    val <- state$v[seg$i, seg$j]
    sprintf("Vertical    ligne=%d  col=%d  etat=%d", seg$i, seg$j, val)
  }
}


# -------------------------------------------------------------
# Vérification de la solution
# -------------------------------------------------------------
#' Compare l'état joueur à la solution du puzzle
#'
#' @param player  list(h, v) — état courant du joueur
#' @param puzzle  list(h, v, clues) — puzzle généré (h et v = solution)
#' @return        chaîne de caractères indiquant succès ou échec
#' @export
check_solution <- function(player, puzzle) {
  if (identical(player$h, puzzle$h) && identical(player$v, puzzle$v)) {
    "Felicitations, la grille est correcte !"
  } else {
    "La grille n'est pas encore correcte."
  }
}


# -------------------------------------------------------------
# Génération d'une solution simple (boucle sur le contour)
# -------------------------------------------------------------
#' @param n  taille de la grille
#' @return   list(h, v)
#' @export
generate_border_loop <- function(n) {
  h <- matrix(0L, nrow = n + 1, ncol = n)
  v <- matrix(0L, nrow = n, ncol = n + 1)
  h[1,     ] <- 1L
  h[n + 1, ] <- 1L
  v[, 1    ] <- 1L
  v[, n + 1] <- 1L
  list(h = h, v = v)
}


# -------------------------------------------------------------
# Calcul des indices (nombre de segments autour de chaque cellule)
# -------------------------------------------------------------
#' @param h  matrice (n+1) x n
#' @param v  matrice n x (n+1)
#' @return   matrice n x n des indices (0 à 4)
#' @export
compute_clues <- function(h, v) {
  n     <- nrow(v)
  m     <- ncol(h)
  clues <- matrix(0L, nrow = n, ncol = m)
  for (i in 1:n)
    for (j in 1:m)
      clues[i, j] <- h[i, j] + h[i + 1, j] + v[i, j] + v[i, j + 1]
  clues
}


# -------------------------------------------------------------
# Masquage aléatoire des indices
# -------------------------------------------------------------
#' @param clues  matrice n x n
#' @param prob   probabilité de masquer chaque indice (défaut 0.5)
#' @return       même matrice avec certaines valeurs remplacées par NA
#' @export
mask_clues <- function(clues, prob = 0.5) {
  mask        <- matrix(runif(length(clues)) < prob, nrow = nrow(clues))
  clues[mask] <- NA
  clues
}


# -------------------------------------------------------------
# Pipeline complet : génère un puzzle prêt à jouer
# -------------------------------------------------------------
#' @param n  taille de la grille
#' @return   list(h, v, clues)
#'             $h, $v   : matrices de la solution
#'             $clues   : indices partiels visibles pour le joueur
#' @export
generate_puzzle <- function(n, prob) {
  sol   <- generate_border_loop(n)
  clues <- compute_clues(sol$h, sol$v)
  clues <- mask_clues(clues, prob)
  list(h = sol$h, v = sol$v, clues = clues)
}
