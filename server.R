server <- function(input, output) {
  observeEvent(input$predict_btn, {
    req(input$text)
    words <- strsplit(tolower(input$text), "\\s+")[[1]]
    len <- length(words)

    w1 <- if (len >= 2) words[len - 1] else words[len]
    w2 <- if (len >= 2) words[len] else NULL

    result <- predict_next_word(w1, w2, n = 1)

    output$prediction <- renderText(result$word[1])
    output$source <- renderText(result$source)
  })
}
