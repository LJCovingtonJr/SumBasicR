#' SumBasic: A Frequency-Based Summarization System
#'
#' Takes a string of text and a length.  Returns a summary of the text of the given length via SumBasic.
#' @param text A lengthy string of text
#' @param len Length (in sentences) of the desired summary
#' @return The summary, generated via SumBasic
#' @import tokenizers
#' @export
#'
sumbasic <- function(text, len = 3){
  text <- as.character(text)
  dist <- compute_probability_dist(text)
  weights <- sentence_weights(text, dist)

  result <- character()

  while(length(unlist(tokenize_sentences(result))) < len){
    highest_prob_word <- dist[which.max(dist$probs),]$word
    best_score_sentence <- character()
    best_score_weight <- 0

    for(i in 1:nrow(weights)){
      sentence_tok <- unlist(tokenize_words(weights[i,]$sentences))
      if(highest_prob_word %in% sentence_tok & weights[i,]$weights > best_score_weight){
        best_score_sentence <- paste(weights[i,]$sentences, ".", sep="")
        best_score_weight <- weights[i,]$weights

        best_score_tok <- unlist(tokenize_words(best_score_sentence))
        for(j in best_score_tok){
          dist[which(dist$words == j),]$probs <- dist[which(dist$words == j),]$probs^2
        }
      }
    }

    result <- paste(result, best_score_sentence)
    weights <- sentence_weights(text, dist)
  }

  result <- trimws(result)
  return(result)
}
