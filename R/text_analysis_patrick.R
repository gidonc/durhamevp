create_sets<-function(the_corpus, size, seed){
  #' Create training and test set from a corpus
  #' @param the_corpus The text corpus to split
  #' @param size Size of the training set (must be smaller than number of documents in the corpus)
  #' @param seed Seed for randomization to ensure replicability
  #' @export
  set.seed(seed)
  id_train<-sample(1:quanteda::ndoc(the_corpus), size, replace=FALSE)
  training<-quanteda::corpus_subset(the_corpus, fakeid %in% id_train)
  testing<-quanteda::corpus_subset(the_corpus, !fakeid %in% id_train)
  return(list(training=training, testing=testing))
}

preprocess_ngrams<-function(the_corpus, n, min_termfreq=2, min_docfreq=2, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus including the creation of n-grams and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @param n Upper-bound of n-grams to be included. E.g., entering 2 would mean that uni-grams and bi-grams are included
  #' @export
  toks<-quanteda::tokens(the_corpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                         remove_separators = TRUE, remove_hyphens = TRUE, ngrams=1:n)
  the_dfm <- quanteda::dfm(toks, stem=TRUE, remove=quanteda::stopwords("english"))
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

check_sngrams<-function(toks, wseq, wseqc){
  #' Check whether a tokenized corpus contains specific n-grams. The function prints a list of the specific n-gram in all texts of the corpus
  #' @param toks Tokonized corpus
  #' @param wseq Pre-specified word sequence as list on which n-grams should be created; may contain wildcard combinations using '*'
  #' @param wseqc Pre-specified word sequence concatinated
  #' @export
  grams<-quanteda::tokens_compound(toks,phrase(wseq))
  grams<-quanteda::tokens_select(grams,phrase(wseqc))
  print(grams)
}

nb_test<-function(training, testing, classvar, distribution="Bernoulli"){
  #' Train a naive bayes classifier on a training dfm and asses its performance on a test dfm reporting test statistics (wrapper function aroudn Quanteda commands)
  #' @param training Training data frequency matrix
  #' @param testing Testing data frequency matrix
  #' @param classvar Classification variable name, entered as string
  #' @export
  nb<-quanteda::textmodel_nb(training,quanteda::docvars(training, classvar))
  testing<-quanteda::dfm_select(testing,training)
  actual_class<-quanteda::docvars(testing,classvar)
  predicted_class<-predict(nb,testing)
  nb_res<-caret::confusionMatrix(table(actual_class,predicted_class),mode="everything")
  return(nb_res)
}
