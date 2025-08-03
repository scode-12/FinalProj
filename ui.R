library(shiny)
library(data.table)

load("ngrams.RData")

setDT(trigrams)
setDT(bigrams)
setDT(unigrams)

trigrams[, prefix := as.character(prefix)]
trigrams[, next_word := as.character(next_word)]
bigrams[, prefix := as.character(prefix)]
bigrams[, next_word := as.character(next_word)]
unigrams[, word := as.character(word)]


custom_stopwords <- c("the", "of", "to", "a", "in", "and", "is", "was", "are")
predict_next_word <- function(w1, w2 = NULL, n = 3) {
  prefix_str <- if (!is.null(w2)) paste(w1, w2) else w1
  trig <- trigrams[prefix == prefix_str]

  if (nrow(trig) > 0) {
    trig[, prob := freq / sum(freq)]
    trig[, prob := ifelse(next_word %in% custom_stopwords, prob * 0.1, prob)]
    setorder(trig, -prob)
    return(list(source = "trigram", word = head(trig$next_word, n), prob = head(trig$prob, n)))
  }

  bi <- bigrams[prefix == prefix_str]

  if (nrow(bi) > 0) {
    bi[, prob := 0.4 * freq / sum(freq)]
    bi[, prob := ifelse(next_word %in% custom_stopwords, prob * 0.1, prob)]
    setorder(bi, -prob)
    return(list(source = "bigram", word = head(bi$next_word, n), prob = head(bi$prob, n)))
  }

  uni <- unigrams[!word %in% custom_stopwords]
  uni[, prob := 0.4^2 * freq / sum(freq)]
  setorder(uni, -prob)
  return(list(source = "unigram", word = head(uni$word, n), prob = head(uni$prob, n)))
}

ui <- fluidPage(
  titlePanel("Next Word Prediction App"),

  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a phrase:", value = "i love"),
      actionButton("predict_btn", "Predict Next Word")
    ),

    mainPanel(
      h4("Prediction:"),
      verbatimTextOutput("prediction"),
      h5("Source:"),
      verbatimTextOutput("source")
    )
  )
)


