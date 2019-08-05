#' is_valid
#'
#' @param mat matrix represening the board
#'
#' @return TRUE if proper board
is_valid <- function(mat) {
  assert_that(sum(mat == 0 | mat == 1 | mat == 2) == length(mat))
  assert_that(nrow(mat) == ncol(mat))
  return(TRUE)
}

#' board
#'
#' @param mat board matrix
#' @param n number of rows and columns of desired board
#' @param p proportion of blocked squares in board
#'
#' @return board object
board <- function(mat = NULL, n = 5, p = 0.25) {
  if (!is.null(mat)) {
    if(is_valid(mat)) board <- mat
  }
  else board <- generate_board_mat(n, p)
  class(board) <- c("board", "matrix")
  attr(board, "board") <- board
  attr(board, "n") <- nrow(board)
  attr(board, "p") <- if (!is.null(mat)) round(sum(board == 0) / length(board), digits=2) else p
  return(board)
}
