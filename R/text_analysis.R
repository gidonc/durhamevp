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

preprocess_corpus<-function(the_corpus, min_termfreq=2, min_docfreq=2,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE,
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to split.
  #' @export
  the_dfm <- dfm(the_corpus, stem=TRUE, remove=stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq)
  if(dfm_tfidf){
    the_dfm<-dfm_tfidf(the_dfm)
  }
  the_dfm
}

split_dfm <- function(dfm, n_train){
  #' Divide a document feature matrix into training and testing sets based on number of training items
  #' @param dfm The document feature matrix to split.
  #' @param n_train The number of documents in the training set.
  #' @export
  training_set <- quanteda::dfm_sample(dfm, n_train)
  testing_set <- dfm[setdiff(docnames(dfm), docnames(training_set))]
  return(list(training_set=training_set, testing_set=testing_set))
}

split_corpus<-function(the_corpus, n_train, min_termfreq=2, min_docfreq=2,
                       remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE,
                       dfm_tfidf=FALSE){
  #' Preprocess a text corpus and divide it into training and testing sets based on number of training items.
  #' Note: it is more efficient to preprocess and split separately, especially if running in a loop.
  #' @export
  the_dfm <- dfm(the_corpus, stem=TRUE, remove=stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq)
  if(dfm_tfidf){
    the_dfm<-dfm_tfidf(the_dfm)
  }
  training_set <- dfm_sample(the_dfm, n_train)
  testing_set <- the_dfm[setdiff(docnames(the_dfm), docnames(training_set))]
  return(list(training_set=training_set, testing_set=testing_set))
}
my_naivebayes<-function(train, test){
  #' @export
  nb<-textmodel_nb(train, y=docvars(train, "EV_article"), prior="docfreq")
  prob_nb<-predict(nb, newdata = test, type="probability")
  pred_nb<-data.frame(predict(nb, newdata = test, type="class"))
  res<-data.frame(predict_nb=pred_nb, prob_nb)
  names(res)<-c("predict_nb", "prob_notev", "prob_ev")
  res
}

my_affinity<-function(train, test){
  #' @export
  aff<-textmodel_affinity(train, y=docvars(train, "EV_article"))
  pred_aff<-predict(aff, newdata = test)
}

my_wordscores<-function(train, test){
  #' @export
  this_wordscores<-textmodel_wordscores(train, y=docvars(train, "EV_article"))
  pred_wordscores<-predict(this_wordscores, newdata = test, force=TRUE)
  pred_wordscores

}
organise_results <- function (testset, pred_nb, pred_aff=NULL, pred_wordscores=NULL){
  #' @export
  this_res<-as.tbl(data.frame(actual=docvars(testset, "EV_article"),
                              actual_f=factor(docvars(testset, "EV_article")),
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
    nb_assess<-as.data.frame(caret::confusionMatrix(data=this_res$nb_ev_cat, reference=this_res$actual_f, mode="everything", positive="1")$byClass)
    names(nb_assess)<-"value"
    nb_assess <- tibble::rownames_to_column(nb_assess) %>%
      mutate(model="naive bayes")
  } else {
    nb_assess<-data.frame(matrix(nrow=0, ncol=3))
    names(nb_assess)<-c("rowname", "value", "model")
  }
  if("aff_ev_cat_f" %in% names(res_data)){
    aff_assess<-as.data.frame(caret::confusionMatrix(data=this_res$aff_ev_cat_f, reference=this_res$actual_f, mode="everything", positive="1")$byClass)
    names(aff_assess)<-"value"
    aff_assess <- rownames_to_column(aff_assess) %>%
      mutate(model="affinity")
  } else{
    aff_assess<-data.frame(matrix(nrow=0, ncol=3))
    names(aff_assess)<-c("rowname", "value", "model")
  }

  this_assess<- bind_rows(aff_assess, nb_assess)

  this_assess
}
