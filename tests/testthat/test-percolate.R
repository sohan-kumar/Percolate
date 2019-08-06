test_mat <- matrix(rep(0, 100), nrow=10, ncol=10)
test_board <- board(test_mat)
percolate_test <- percolate(test_board)
assert_that(all(unclass(percolate_test$result_board) == matrix(rep(0,100), nrow = 10, ncol = 10)))
assert_that(!percolate_test$result)

test_mat <- matrix(rep(1, 100), nrow=10, ncol=10)
test_board <- board(test_mat)
percolate_test <- percolate(test_board)
assert_that(all(unclass(percolate_test$result_board) == matrix(rep(2,100), nrow = 10, ncol = 10)))
assert_that(percolate_test$result)

test_mat <- matrix(c(0,1,0,1), nrow=2, ncol=2)
test_board <- board(test_mat)
percolate_test <- percolate(test_board)
assert_that(all(unclass(percolate_test$result_board) == matrix(c(0,1,0,1), nrow = 2, ncol = 2)))
assert_that(!percolate_test$result)

test_mat <- matrix(c(1,0,1,0), nrow=2, ncol=2)
test_board <- board(test_mat)
percolate_test <- percolate(test_board)
assert_that(all(unclass(percolate_test$result_board) == matrix(c(2,0,2,0), nrow = 2, ncol = 2)))
assert_that(!percolate_test$result)

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    all(unclass(your_board) == unclass(result_board)) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })


  expect_true(all(bool_vec))
})
