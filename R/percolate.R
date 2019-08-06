#' percolate_top
#'
#' @param mat board
#' @param n num rows of board
#' @param start row number
#'
#' @return percolate the current row from the top down
#' @export
percolate_top <- function(mat, n, start) {
  for (i in seq(start, n^2, n)) {
    if (mat[i - 1] == 2 && mat[i] == 1) mat[i] = 2
  }
  return(mat)
}

#' percolate_side
#'
#' @param mat board
#' @param n num rows of board
#' @param start row number
#'
#' @return percolate all squares around the current square
#' @export
percolate_side <- function(mat, n, start) {
  for (i in seq(start, n^2, n)) {
    if (mat[i] == 2) {
      if (i - n >= 1 && mat[i-n] == 1) mat[i-n] = 2 #if filled in, fill in left
      if (i + n <= n^2 & mat[i+n] == 1) mat[i+n] = 2 #if filled in, fill in right
      if (mat[i-1] == 1) mat[i-1] = 2
    }
  }
  return(mat)
}

#' percolate
#'
#' @param mat board
#' @param n num rows of the board
#'
#' @return does a percolation iteration of the board
#' @export
percolate <- function(mat, n) {
  #top row fill water in all empty boxes
  for (i in seq(1, n^2, n)) if (mat[i] == 1) mat[i] = 2
  #rest of board - top/down percolation followed by side/side percolation
  for (i in 2:n) {
    mat <- percolate_top(mat, n, i)
    mat <- percolate_side(mat, n, i)
  }
  return(mat)
}

#' percolate_board
#'
#' @param start_board board
#'
#' @return list: result board after percolation attempt, T if percolation successful, else F
#' @export
percolate.board <- function(start_board) {
  mat <- unclass(start_board)
  is_valid(mat)
  n <- attr(start_board, "n")
  old_board <- mat
  new_board <- percolate(mat,n)
  while(!all(old_board == new_board)) {
    old_board = new_board
    new_board <- percolate(old_board, n)
  }
  percolates <- F
  for (i in seq(n, n^2, n)) if (new_board[i] == 2) percolates <- T
  return(list(result_board = board(new_board), result = percolates))
}
