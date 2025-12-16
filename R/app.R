#' Play Tic-Tac-Toe Game
#'
#' Launches a Shiny app to play Tic-Tac-Toe.
#' @import shiny
#' @import bslib
#' @export
play_ticr <- function() {
  # Используем классический fluidPage вместо page_fluid
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "minty"),
    titlePanel("Tic-Tac-Toe"),

    # Классическая сетка: отступ-игра-отступ
    fluidRow(
      column(width = 4), # Пусто слева
      column(
        width = 4,       # Центр
        # Обычная HTML верстка карточки (работает везде)
        div(class = "card border-primary mb-3",
            div(class = "card-header bg-primary text-white text-center", "Game Board"),
            div(class = "card-body",
                # Сетка для кнопок
                div(
                  style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 5px;",
                  lapply(1:9, function(i) {
                    actionButton(
                      inputId = as.character(i),
                      label = "\u00A0", # Неразрывный пробел
                      class = "btn-light",
                      style = "height: 80px; font-size: 24px; font-weight: bold; width: 100%;"
                    )
                  })
                ),
                hr(),
                div(class = "text-center",
                    h4(textOutput("status_msg")),
                    actionButton("restart", "Restart Game", class = "btn-warning w-100 mt-2")
                )
            )
        )
      ),
      column(width = 4) # Пусто справа
    )
  )

  server <- function(input, output, session) {
    # Состояние игры
    game <- reactiveValues(
      board = rep(NA_character_, 9),
      turn = "X",
      winner = NULL,
      finished = FALSE
    )

    # Обработка кликов
    lapply(1:9, function(i) {
      observeEvent(input[[as.character(i)]], {

        if (game$finished || !is.na(game$board[i])) {
          return()
        }

        # Ход
        game$board[i] <- game$turn
        message(sprintf("Move: Cell %d -> %s", i, game$turn))

        # Проверка победителя (твоя исправленная логика)
        res <- check_winner_vectorized(game$board)

        if (!is.null(res)) {
          game$winner <- res
          game$finished <- TRUE
          message(sprintf("WINNER: %s", res))

          showModal(modalDialog(
            title = "Game Over!",
            h3(paste("Winner is:", res)),
            footer = actionButton("modal_restart", "Play Again")
          ))
        } else if (all(!is.na(game$board))) {
          game$finished <- TRUE
          showModal(modalDialog(
            title = "Draw!",
            h3("No moves left."),
            footer = actionButton("modal_restart", "Play Again")
          ))
        } else {
          game$turn <- if (game$turn == "X") "O" else "X"
        }
      })
    })

    # Обновление кнопок
    observe({
      lapply(1:9, function(i) {
        val <- game$board[i]
        label <- if (is.na(val)) "\u00A0" else val
        updateActionButton(session, as.character(i), label = label)
      })
    })

    output$status_msg <- renderText({
      if (game$finished) {
        if (!is.null(game$winner)) paste("Winner:", game$winner) else "It's a Draw!"
      } else {
        paste("Current turn:", game$turn)
      }
    })

    reset_game <- function() {
      game$board <- rep(NA_character_, 9)
      game$turn <- "X"
      game$winner <- NULL
      game$finished <- FALSE
      removeModal()
      message("--- New Game ---")
    }

    observeEvent(input$restart, { reset_game() })
    observeEvent(input$modal_restart, { reset_game() })
  }

  shinyApp(ui, server)
}
