is_valid(generate_board_mat())
is_valid(generate_board_mat(n=1))
test_that("not all 0's, 1's, 2's",
          expect_error(is_valid(matrix(c(0,0,4), nrow=1, ncol=3))))
test_that("not square matrix",
          expect_error(is_valid(matrix(c(0,0,0), nrow=1, ncol=3))))
test_that("not a matrix",
          expect_error(is_valid(c(0,0,0))))

test_mat <- matrix(c(0,1,1,0,1,1,0,1,1), nrow = 3, ncol=3)
test_board <- board(test_mat)
expect_equivalent(test_mat, unclass(test_board))
expect_equivalent(nrow(test_mat), attr(test_board, "n"))
expect_equivalent(0.33, attr(test_board, "p"))

test_mat <- matrix(c(0,1,1,1,1), nrow = 2, ncol = 3)
expect_error(board(matrix(c(0,0,4), nrow=1, ncol=3)))
expect_error((board(matrix(c(0,0,0), nrow=1, ncol=3))))
expect_error(board(c(0,0,0)))
