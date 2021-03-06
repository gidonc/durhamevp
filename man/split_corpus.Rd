% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_analysis.R
\name{split_corpus}
\alias{split_corpus}
\title{Preprocess a text corpus and divide it into training and testing sets based on number of training items.
Note: it is more efficient to preprocess and split separately, especially if running in a loop.}
\usage{
split_corpus(
  the_corpus,
  n_train,
  min_termfreq = 2,
  min_docfreq = 2,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_hyphens = TRUE,
  dfm_tfidf = FALSE,
  stem = TRUE
)
}
\description{
Preprocess a text corpus and divide it into training and testing sets based on number of training items.
Note: it is more efficient to preprocess and split separately, especially if running in a loop.
}
