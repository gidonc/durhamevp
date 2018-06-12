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

split_corpus<-function(the_corpus, n_train){
  the_dfm <- dfm(the_corpus, stem=TRUE, remove=stopwords("english"), remove_punct=TRUE)
  training_set <- dfm_sample(the_dfm, n_train)
  testing_set <- the_dfm[setdiff(docnames(the_dfm), docnames(training_set))]
  return(list(training_set=training_set, testing_set=testing_set))
}
my_naivebayes<-function(train, test){
  nb<-textmodel_nb(train, y=docvars(train, "EV_article"), prior="docfreq")
  prob_nb<-predict(nb, newdata = test, type="probability")
  pred_nb<-data.frame(predict(nb, newdata = test, type="class"))
  res<-data.frame(predict_nb=pred_nb, prob_nb)
  names(res)<-c("predict_nb", "prob_notev", "prob_ev")
  res
}

my_affinity<-function(train, test){
  aff<-textmodel_affinity(train, y=docvars(train, "EV_article"))
  pred_aff<-predict(aff, newdata = test)
}

my_wordscores<-function(train, test){
  this_wordscores<-textmodel_wordscores(train, y=docvars(train, "EV_article"))
  pred_wordscores<-predict(this_wordscores, newdata = test, force=TRUE)
  pred_wordscores

}
organise_results <- function (testset, pred_nb, pred_aff, pred_wordscores=NULL){
  this_res<-as.tbl(data.frame(actual=docvars(testset, "EV_article"),
                              nb_ev_cat=pred_nb$predict_nb,
                              nb_ev_prob=pred_nb$prob_ev,
                              nb_notev_prob=pred_nb$prob_notev,
                              aff_ev_prob=pred_aff$coefficients[,2],
                              aff_notev_prob=pred_aff$coefficients[,1],
                              aff_ev_cat=as.numeric(pred_aff$coefficients[,2]>pred_aff$coefficients[,1])))
  if(!is.null(pred_wordscores)){
    this_res$wordscore_prob<-pred_wordscores
  }

  this_res
}
