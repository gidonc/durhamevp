status_to_text<-function(status){
  #' Convert candidate document status to text
  #' @param status status as recorded in ev database
  #' @export
  status <- as.numeric(status)
  case_when(
    status==0 ~ "irrelevant",
    status==1 ~ "relevant",
    status==2 ~ "cannot decide",
    status==3 ~ "ev verbatim repeat",
    status==4 ~ "ev but Ireland",
    status==5 ~ "ev but Scotland",
    status==6 ~ "ev abroad",
    status==7 ~ "election but not violence",
    status==8 ~ "violence but not election",
    status==9 ~ "EV but not timing",
    TRUE ~ "other code"
  )
}

preprocess_corpus<-function(the_corpus, min_termfreq=2, min_docfreq=2, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @export
  the_dfm <- quanteda::dfm(the_corpus, stem=TRUE, remove=quanteda::stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

split_dfm <- function(dfm, n_train){
  #' Divide a document feature matrix into training and testing sets based on number of training items
  #' @param dfm The document feature matrix to split.
  #' @param n_train The number of documents in the training set.
  #' @export
  training_set <- quanteda::dfm_sample(dfm, n_train)
  testing_set <- dfm[setdiff(quanteda::docnames(dfm), quanteda::docnames(training_set))]
  return(list(training_set=training_set, testing_set=testing_set))
}

split_corpus<-function(the_corpus, n_train, min_termfreq=2, min_docfreq=2,
                       remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE,
                       dfm_tfidf=FALSE){
  #' Preprocess a text corpus and divide it into training and testing sets based on number of training items.
  #' Note: it is more efficient to preprocess and split separately, especially if running in a loop.
  #' @export
  the_dfm <- quanteda::dfm(the_corpus, stem=TRUE, remove=quanteda::stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  training_set <- quanteda::dfm_sample(the_dfm, n_train)
  testing_set <- the_dfm[setdiff(quanteda::docnames(the_dfm), quanteda::docnames(training_set))]
  return(list(training_set=training_set, testing_set=testing_set))
}

my_naivebayes<-function(train, test){
  #' @export
  nb<-quanteda::textmodel_nb(train, y=quanteda::docvars(train, "EV_article"), prior="docfreq")
  prob_nb<-predict(nb, newdata = test, type="probability")
  pred_nb<-data.frame(predict(nb, newdata = test, type="class"))
  res<-data.frame(predict_nb=pred_nb, prob_nb)
  names(res)<-c("predict_nb", "prob_notev", "prob_ev")
  res
}

my_affinity<-function(train, test){
  #' @export
  aff<-quanteda::textmodel_affinity(train, y=quanteda::docvars(train, "EV_article"))
  pred_aff<-predict(aff, newdata = test)
}

my_wordscores<-function(train, test){
  #' @export
  this_wordscores<-quanteda::textmodel_wordscores(train, y=quanteda::docvars(train, "EV_article"))
  pred_wordscores<-quanteda::predict(this_wordscores, newdata = test, force=TRUE)
  pred_wordscores

}
organise_results <- function (testset, pred_nb, pred_aff=NULL, pred_wordscores=NULL){
  #' @export
  this_res<-dplyr::as.tbl(data.frame(actual=quanteda::docvars(testset, "EV_article"),
                              actual_f=factor(quanteda::docvars(testset, "EV_article")),
                              nb_ev_cat=pred_nb$predict_nb,
                              nb_ev_prob=pred_nb$prob_ev,
                              nb_notev_prob=pred_nb$prob_notev
                              ))
  if(!is.null(pred_aff)){
    this_res$aff_ev_prob=pred_aff$coefficients[,2]
    this_res$aff_notev_prob=pred_aff$coefficients[,1]
    this_res$aff_ev_cat=as.numeric(pred_aff$coefficients[,2]>pred_aff$coefficients[,1])
    this_res$aff_ev_cat_f=factor(as.numeric(pred_aff$coefficients[,2]>pred_aff$coefficients[,1]))
  }
  if(!is.null(pred_wordscores)){
    this_res$wordscore_prob<-pred_wordscores
  }

  this_res
}

assess_classification <- function(res_data){
  #' @export
  if("nb_ev_cat" %in% names(res_data)){
    nb_assess<-as.data.frame(caret::confusionMatrix(data=res_data$nb_ev_cat, reference=res_data$actual_f, mode="everything", positive="1")$byClass)
    names(nb_assess)<-"value"
    nb_assess <- tibble::rownames_to_column(nb_assess) %>%
      mutate(model="naive bayes")
  } else {
    nb_assess<-data.frame(matrix(nrow=0, ncol=3))
    names(nb_assess)<-c("rowname", "value", "model")
  }
  if("aff_ev_cat_f" %in% names(res_data)){
    aff_assess<-as.data.frame(caret::confusionMatrix(data=res_data$aff_ev_cat_f, reference=res_data$actual_f, mode="everything", positive="1")$byClass)
    names(aff_assess)<-"value"
    aff_assess <- tibble::rownames_to_column(aff_assess) %>%
      mutate(model="affinity")
  } else{
    aff_assess<-data.frame(matrix(nrow=0, ncol=3))
    names(aff_assess)<-c("rowname", "value", "model")
  }

  this_assess<- bind_rows(aff_assess, nb_assess)

  this_assess
}

get_classified_docs <- function (){
  #' Function to get documents from database which are classified for text analysis.
  #'
  #' \code{get_classified_docs} retrives the currently classified document set from the database. The documents have been classifed to enable machine learning.
  #' @return Returns a dataframe containing some general articles, some election but not violent articles and some election violence articles. The dataframe can be split into separate corpuses (if desired). The different types of article are distinguished using dummy variables: \code{EV_article} is \code{1} for election violence articles and \code{0} for all other articles. \code{election_article} is \code{1} for election articles (including election violence articles) and \code{0} for all other articles. (there is an unncessary \code{just_election} indicator for convenience which is \code{1} for election but not violence articles and \code{0} for election violence and general articles). The full ocr is in the field \code{ocr} and the short two line description is in the field \code{description}. The unique identifier column \code{fakeid} does not correspond to any id in the database because the data is aggregated from two different tables in the database (documents and candidate_documents).
  #' @export
  all_documents<-durhamevp::get_document("all")
  all_allocations<-durhamevp::get_allocation("all")
  some_documents<-dplyr::left_join(all_allocations, all_documents, by=c("document_id"="id"))
  ev_docs<-dplyr::filter(some_documents, violent_nature=="true", user_id>7)
  election_docs<-dplyr::filter(some_documents, violent_nature=="false", electoral_nature=="true", geo_relevant=="true", time_relevant=="true", user_id>7)
  ev_docs <- ev_docs[!duplicated(ev_docs$document_id), ]
  election_docs<-election_docs[!duplicated(election_docs$id), ]
  nothing_docs<-durhamevp::get_candidate_documents(cand_document_id = c(23058:23834))
  nothing_docs<-nothing_docs[nothing_docs$status=="0",]
  nothing_docs$EV_article<-election_docs$EV_article<-nothing_docs$election_article<-0
  ev_docs$EV_article<-ev_docs$election_article<-election_docs$election_article<-1
  sum(election_docs$id %in% ev_docs$id)

  election_docs$just_election<-1
  ev_docs$just_election<-nothing_docs$just_election <- 0
  nothing_docs$corpus_election<-0
  ev_docs$corpus_election<-election_docs$corpus_election<-1
  the_corpus<-dplyr::bind_rows(nothing_docs,
                        election_docs,
                        ev_docs)
  the_corpus$general_corpus<-1

  the_corpus<-the_corpus[,c("id", "title", "description", "type", "page", "publication_date", "word_count", "ocr", "election_article", "EV_article", "corpus_election", "electoral_nature", "violent_nature", "candidate_document_id")]
  the_corpus <- tibble::rowid_to_column(the_corpus, "fakeid")

  the_corpus
}

contains_words <- function(the_dataframe, contains_words, text_col="ocr", results_col="EV_article"){
  #' Function to tally cases with and without a particular words
  #' @param the_dataframe the dataframe with a text column and a results column
  #' @param contains_words the word (or other regex) to search for
  #' @param text_col the column containing the text
  #' @param results_col the column containing the classification

  have_word<-grepl(contains_words, the_dataframe[,text_col])
  dont_have_word<-!have_word

  res_have_word<-the_corpus[,results_col][have_word]
  res_dont_have_word<-the_corpus[,results_col][dont_have_word]

  print(sum(res_have_word)/length(res_have_word))
  print(sum(res_dont_have_word)/length(res_dont_have_word))

}
