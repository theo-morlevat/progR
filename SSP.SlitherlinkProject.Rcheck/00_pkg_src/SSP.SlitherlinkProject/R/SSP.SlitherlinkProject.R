#' Addition de deux nombres
#'
#' @param h_mat matrice des vecteurs horizontaux
#' @param v_mat
#' @return
#' @export

# Fonction pour dessiner la grille
draw_grid <- function(h_mat, v_mat, clues = NULL) {

  n_row <- nrow(v_mat)
  n_col <- ncol(h_mat)

  plot(0, 0, type = "n",
       xlim = c(0, n_col),
       ylim = c(0, n_row),
       xlab = "", ylab = "",
       axes = FALSE, asp = 1)

  # Points
  for (i in 0:n_col) {
    for (j in 0:n_row) {
      points(i, j, pch = 16, cex = 0.5)
    }
  }

  # Segments horizontaux
  for (i in 1:nrow(h_mat)) {
    for (j in 1:ncol(h_mat)) {
      if (h_mat[i, j] == 1) {
        segments(j-1, n_row - (i-1), j, n_row - (i-1), lwd = 2)
      }
    }
  }

  # Segments verticaux
  for (i in 1:nrow(v_mat)) {
    for (j in 1:ncol(v_mat)) {
      if (v_mat[i, j] == 1) {
        segments(j-1, n_row - i, j-1, n_row - (i-1), lwd = 2)
      }
    }
  }

  # Indices
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
