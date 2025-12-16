test_that("Game detects winner on Right Column (3, 6, 9)", {
  # Симулируем твою ситуацию
  # X: 1, 3, 6, 9
  # O: 2, 4, 5

  board <- rep(NA_character_, 9)
  board[c(1, 3, 6, 9)] <- "X"
  board[c(2, 4, 5)] <- "O"

  # Проверяем, что функция возвращает "X"
  expect_equal(check_winner_vectorized(board), "X")
})

test_that("Game detects winner on Diagonal (1, 5, 9)", {
  board <- rep(NA_character_, 9)
  board[c(1, 5, 9)] <- "O"

  expect_equal(check_winner_vectorized(board), "O")
})

test_that("Game detects winner on Top Row (1, 2, 3)", {
  board <- rep(NA_character_, 9)
  board[c(1, 2, 3)] <- "X"

  expect_equal(check_winner_vectorized(board), "X")
})
test_that("Scenario: X wins top row (1,2,3), O has scattered moves", {
  board <- rep(NA_character_, 9)
  # Крестики на 1, 2, 3
  board[c(1, 2, 3)] <- "X"
  # Нолики на 5, 6, 8
  board[c(5, 6, 8)] <- "O"

  # Должен выиграть X
  expect_equal(check_winner_vectorized(board), "X")
})
