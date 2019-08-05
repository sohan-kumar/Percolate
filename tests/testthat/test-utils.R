generate_board_mat()
generate_board_mat(n = 8, p = 0.75)

is_valid(generate_board_mat())
is_valid(generate_board_mat(n=1))
test_that("not all 0's, 1's, 2's",
          expect_error(is_valid(matrix(c(0,0,4), nrow=1, ncol=3))))
test_that("not square matrix",
          expect_error(is_valid(matrix(c(0,0,0), nrow=1, ncol=3))))
test_that("not a matrix",
          expect_error(is_valid(c(0,0,0))))

read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")

test_that("n doesn't match",is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")))

test_that("incorrect input",is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")))

test_that("no n", expect_error(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt")))

test_that("improper format",is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")))

test_that("board not filled in",
          is.na(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")))

test_that("n doesn't match",
          expect_error(read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")))
