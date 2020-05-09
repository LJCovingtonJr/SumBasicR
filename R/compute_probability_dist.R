#' Compute probability distribution of a lengthy text
#'
#' Takes a string of text, returns a data frame of the unique words, their frequency, and their probability (n/N)
#' @param text A lengthy string of text
#' @return  a data frame of the unique words, their frequency, and their probability (n/N)
#' @import tokenizers
#' @import stopwords
#' @export
compute_probability_dist <- function(text){
  # For a given text, computes the probability distribution for every word as n/N,
  # where "n" is the number of times the word appeared in the input and "N" is the total
  # number of content word tokens in the input

  stopwords <- stopwords()
  clean <- gsub("[[:punct:]]", "", text)

  words <- unlist(tokenize_words(clean))

  counts <- as.data.frame(table(words), stringsAsFactors = FALSE)
  counts$probs <- ifelse(counts$words %in% stopwords, 0, counts$Freq/sum(counts$Freq))

  result <- counts
  return(result)
}
