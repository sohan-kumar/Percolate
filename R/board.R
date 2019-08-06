#' is_valid
#'
#' @param mat matrix represening the board
#' @import assertthat
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

convert_to_df <- function(mat, n) {
  board_y <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5))
  board_df <- data.frame(x = board_y, y = rev((1:n^2 %% n)), z=mat[1:n^2])
  board_df$x <- ifelse(board_df$x == 0, 5, board_df$x)
  board_df$y <- ifelse(board_df$y == 0, 5, board_df$y)
  return(board_df)
}
#' plot_board
#'
#' @param x board
#' @import ggplot2
#' @import tidyr
#'
#' @return ggplot graphic of the inputted board (water, empty blocks, and blocked blocks)
plot_board <- function(x) {
  is_valid(unclass(x))
  n <- attr(x, "n")
  board_df <- convert_to_df(unclass(x), n)
  ggplot(board_df) + geom_tile(aes(x=x, y=y, fill=factor(z))) +
    scale_fill_manual(values = c("0" = "black", "1" = "white", "2" = "lightblue3")) +
    labs(title = paste("Size: ", n)) +  theme(legend.position = "none") +
    theme_void() + coord_equal()
}
