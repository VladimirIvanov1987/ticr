#' Check for a winner in Tic-Tac-Toe
#'
#' @param board A character vector of length 9 containing "X", "O", or NA.
#' @return "X", "O", or NULL if no winner yet.
#' @export
check_winner_vectorized <- function(board) {
  # Матрица всех выигрышных комбинаций (индексы ячеек)
  # Строки 1-3, Столбцы 4-6, Диагонали 7-8
  winning_combinations <- matrix(c(
    1, 2, 3,  # Row 1
    4, 5, 6,  # Row 2
    7, 8, 9,  # Row 3
    1, 4, 7,  # Col 1
    2, 5, 8,  # Col 2
    3, 6, 9,  # Col 3
    1, 5, 9,  # Diag 1
    3, 5, 7   # Diag 2
  ), nrow = 8, byrow = TRUE)

  # Проверяем каждого игрока
  for (player in c("X", "O")) {
    # 1. Создаем карту ходов игрока: TRUE там, где стоит его знак
    # Важно: заменяем NA на FALSE, чтобы не ломать логику
    is_player_move <- (board == player)
    is_player_move[is.na(is_player_move)] <- FALSE

    # 2. Накладываем карту ходов на выигрышные линии
    # Получаем матрицу 8x3, где TRUE/FALSE
    line_results <- matrix(is_player_move[winning_combinations], nrow = 8, ncol = 3)

    # 3. Считаем количество TRUE в каждой строке
    # rowSums превращает TRUE в 1, FALSE в 0.
    # Если сумма равна 3 — значит вся линия заполнена этим игроком
    if (any(rowSums(line_results) == 3)) {
      return(player)
    }
  }

  return(NULL)
}
