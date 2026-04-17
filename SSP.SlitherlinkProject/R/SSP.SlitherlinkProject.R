#' Dessine la grille Slitherlink
#'
#' @param h_mat  matrice (n+1) x n des segments horizontaux (0 = absent, 1 = tracé)
#' @param v_mat  matrice n x (n+1) des segments verticaux  (0 = absent, 1 = tracé)
#' @param clues  matrice n x n des indices par cellule (NA = caché), NULL par défaut
#' @return       aucune valeur de retour (effet de bord : dessin)
#' @export
draw_grid <- function(h_mat, v_mat, clues = NULL) {

  n_row <- nrow(v_mat)      # nombre de lignes   de cellules  (= n)
  n_col <- ncol(h_mat)      # nombre de colonnes de cellules  (= n)

  plot(0, 0, type = "n",
       xlim  = c(0, n_col),
       ylim  = c(0, n_row),
       xlab  = "", ylab = "",
       axes  = FALSE, asp = 1)

  # --- Points (noeuds de la grille) ---
  for (i in 0:n_col) {
    for (j in 0:n_row) {
      points(i, j, pch = 16, cex = 0.5)
    }
  }

  # --- Segments horizontaux ---
  # h_mat est de dimension (n+1) x n
  # h_mat[i, j] == 1 => segment entre le point (j-1, n_row-(i-1)) et (j, n_row-(i-1))
  for (i in 1:nrow(h_mat)) {
    for (j in 1:ncol(h_mat)) {
      if (h_mat[i, j] == 1) {
        segments(j - 1, n_row - (i - 1),
                 j,     n_row - (i - 1),
                 lwd = 2)
      }
    }
  }

  # --- Segments verticaux ---
  # v_mat est de dimension n x (n+1)
  # v_mat[i, j] == 1 => segment entre le point (j-1, n_row-i) et (j-1, n_row-(i-1))
  for (i in 1:nrow(v_mat)) {
    for (j in 1:ncol(v_mat)) {
      if (v_mat[i, j] == 1) {
        segments(j - 1, n_row - i,
                 j - 1, n_row - (i - 1),
                 lwd = 2)
      }
    }
  }

  # --- Indices dans les cellules ---
  if (!is.null(clues)) {
    for (i in 1:nrow(clues)) {
      for (j in 1:ncol(clues)) {
        if (!is.na(clues[i, j])) {
          text(j - 0.5, n_row - i + 0.5, clues[i, j], cex = 1.2)
        }
      }
    }
  }
}


# -------------------------------------------------------------
# Génération d'une solution simple (boucle sur le contour)
# -------------------------------------------------------------
#' @param n taille de la grille (n x n cellules)
#' @return  liste avec $h (matrice (n+1) x n) et $v (matrice n x (n+1))
#' @export
generate_border_loop <- function(n) {
  h <- matrix(0, nrow = n + 1, ncol = n)
  v <- matrix(0, nrow = n, ncol = n + 1)

  # Lignes du haut et du bas
  h[1,   ] <- 1
  h[n + 1, ] <- 1

  # Colonnes de gauche et de droite
  v[, 1    ] <- 1
  v[, n + 1] <- 1

  list(h = h, v = v)
}


# -------------------------------------------------------------
# Calcul des indices (nombre de segments autour de chaque cellule)
# -------------------------------------------------------------
#' @param h matrice (n+1) x n des segments horizontaux
#' @param v matrice n x (n+1) des segments verticaux
#' @return  matrice n x n des indices (valeurs de 0 à 4)
#' @export
compute_clues <- function(h, v) {
  n <- nrow(v)   # nombre de lignes   de cellules
  m <- ncol(h)   # nombre de colonnes de cellules

  clues <- matrix(0, nrow = n, ncol = m)

  for (i in 1:n) {
    for (j in 1:m) {
      clues[i, j] <-
        h[i,     j  ] +   # segment horizontal du dessus
        h[i + 1, j  ] +   # segment horizontal du dessous
        v[i,     j  ] +   # segment vertical de gauche
        v[i,     j + 1]   # segment vertical de droite
    }
  }
  clues
}


# -------------------------------------------------------------
# Masquer aléatoirement une partie des indices
# -------------------------------------------------------------
#' @param clues matrice n x n des indices
#' @param prob  probabilité de masquer chaque indice (défaut 0.5)
#' @return      même matrice avec certaines valeurs remplacées par NA
#' @export
mask_clues <- function(clues, prob = 0.5) {
  mask         <- matrix(runif(length(clues)) < prob, nrow = nrow(clues))
  clues[mask]  <- NA
  clues
}


# -------------------------------------------------------------
# Pipeline complet : génère un puzzle prêt à jouer
# -------------------------------------------------------------
#' @param n taille de la grille (n x n cellules)
#' @return  liste avec $h, $v (matrices de la solution) et $clues (indices partiels)
#' @export
generate_puzzle <- function(n) {
  sol   <- generate_border_loop(n)
  clues <- compute_clues(sol$h, sol$v)
  clues <- mask_clues(clues, prob = 0.5)

  list(
    h      = sol$h,     # matrice (n+1) x n  — segments horizontaux de la solution
    v      = sol$v,     # matrice n x (n+1)  — segments verticaux   de la solution
    clues  = clues      # matrice n x n       — indices visibles pour le joueur
  )
}
