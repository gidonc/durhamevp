#This code contains the key functions for the first stage

create_docsets<-function(alldocs, size, seed){
  #' Create training and test set document matrices
  #' @param alldocs The document data.frame to be split
  #' @param size Size of the training set (must be smaller than number of documents in the data.frame)
  #' @param seed Seed for randomization to ensure replicability
  #' @export
  set.seed(seed)
  id_train<-sample(1:nrow(alldocs), size, replace=FALSE)
  training<-subset(alldocs, fakeid %in% id_train)
  testing<-subset(alldocs, !fakeid %in% id_train)
  return(list(training=training, testing=testing))
}

preprocess_corpus<-function(the_corpus, stem=TRUE, min_termfreq=2, min_docfreq=2, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @param stem default TRUE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 2
  #' @param min_docfreq default 2
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @export
  the_dfm <- quanteda::dfm(the_corpus, stem=stem, remove=quanteda::stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

preprocess_sgrams<-function(the_corpus, wseq, stem=TRUE, min_termfreq=2, min_docfreq=2, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus including the creation of n-grams for specific words and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @param wseq Pre-specified word sequence as list on which n-grams idenfied and added to the dfm
  #' @param stem default TRUE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 2
  #' @param min_docfreq default 2
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @export
  toks<-quanteda::tokens(the_corpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                         remove_separators = TRUE, remove_hyphens = TRUE)
  gramtoks<-quanteda::tokens_compound(toks,quanteda::phrase(wseq))
  the_dfm <- quanteda::dfm(gramtoks, stem=stem, remove=quanteda::stopwords("english"))
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

nb_keywords<-function(training, classvar){
  #' Train a naive bayes classifier and extract keywords as data.frame object ordered by most predictive word of class on top (wrapper function around Quanteda commands). The returned data.frame has three columns: word, 0(=posterior prob of word indicating that document in classvar==0), 1(=posterior prob of word predicting word indicating that document in classvar==1), and id.
  #' @param training Training data feature matrix
  #' @param classvar Classification variable name, entered as string
  #' @export
  nb<-quanteda::textmodel_nb(training,quanteda::docvars(training, classvar))
  post<-as.data.frame(t(nb$PcGw))
  post<-tibble::rownames_to_column(post)
  post$id<-seq.int(nrow(post))
  post<-post[order(post[,3], decreasing=TRUE),]
  return(post)
}
