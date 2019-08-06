library(assertthat)
library(testthat)

#' generate_board_mat()
#'
#' @param n number of rows and columns
#' @param p proportion of blocked out squares
#'
#' @return n x n board with the required number of blocked out units
#' @export
generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that(n >= 0)
  assert_that(n %% 1 == 0)
  assert_that(p >= 0 & p <= 1)
  board <- sample(c(0,1), prob = c(p, 1 - p), size = n^2, replace = T)
  board <- matrix(board, nrow = n, ncol = n)
  return(board)
}
generate_board_mat()
generate_board_mat(n = 8, p = 0.75)

#' is_valid
#'
#' @param mat matrix represening the board
#'
#' @return TRUE if proper board
#' @export
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
#' @export
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

#' conver_to_matrix
#'
#' @param txt_mat matrix in text form
#' @param n nrow of matrix
#'
#' @return matrix in numeric form
#' @export
convert_to_matrix <- function(txt_mat, n) {
  mat <- vector(length = n ^ 2)
  for (i in 1:n) {
    for (j in 1:n) {
      sym <- substr(txt_mat[i], 2 * j - 1, 2 * j - 1)
      if (sym == "*") {
        mat[n * (i-1) + j] <- 0
      }
      else if (sym == ".") {
        mat[n * (i-1) + j] <- 1
      }
      else {
        return(NA)
      }
    }
  }
  return(matrix(mat, nrow=n, ncol=n, byrow=T))
}
#' read_board
#'
#' @param txt lines of matrix symbols
#' @param n nrow of matrix
#'
#' @return calls error if improper formatting, otherwise return conversion of matrix
#' @export
read_board <- function(txt, n) {
  if(txt[1] != "----" & txt[1 + n + 2] != "----") stop("file not properly formatted")
  return(convert_to_matrix(txt[3:(3+n-1)], n))
}
#' read_boards
#'
#' @param file file path
#'
#' @return list of boards designated from file path
#' @export
read_boards <- function(file) {
  board_lines <- readLines(file)
  board_start <- head(which(board_lines == "----"), -1)
  n <- as.numeric(board_lines[board_start + 1])
  boards <- list()
  for (i in 1:length(board_start)) {
    res <- read_board(board_lines[board_start[i]:(board_start[i] + n[i] + 2)], n[i])
    #print(res)
    if (sum(is.na(res)) != 0) boards[[i]] = NA #
    else  boards[[i]] = board(res)
  }
  return(boards)
}
