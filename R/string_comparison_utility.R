get_similarity_score <- function(a, b){
  #' Gets a similarity score between two strings
  #' 
  #' @param a string 1
  #' @param b string 2
  #' @return Returns a similarity score between 0.0 (completely identical) and 1.0 (completely different)
  #'
  init <- fuzzywuzzyR::SequenceMatcher$new(a, b)
  1 - init$ratio()
}
  
