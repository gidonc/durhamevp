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
                       dfm_tfidf=FALSE, stem=TRUE){
  #' Preprocess a text corpus and divide it into training and testing sets based on number of training items.
  #' Note: it is more efficient to preprocess and split separately, especially if running in a loop.
  #' @export
  the_dfm <- quanteda::dfm(the_corpus, stem=stem, remove=quanteda::stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
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
organise_results <- function (testset, pred_nb, pred_aff=NULL, pred_wordscores=NULL, res_col="EV_article"){
  #' @export
  this_res<-dplyr::as.tbl(data.frame(actual=quanteda::docvars(testset, res_col),
                              actual_f=factor(quanteda::docvars(testset, res_col)),
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
      dplyr::mutate(model="naive bayes")
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

  this_assess<- dplyr::bind_rows(aff_assess, nb_assess)

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
  ev_docs<-dplyr::filter(some_documents, violent_nature=="true", violent_nature=="true", electoral_nature=="true", geo_relevant=="true", time_relevant=="true", user_id>7)
  election_docs<-dplyr::filter(some_documents, violent_nature=="false", electoral_nature=="true", geo_relevant=="true", time_relevant=="true", user_id>7)
  ev_docs <- ev_docs[!duplicated(ev_docs$document_id), ]
  election_docs<-election_docs[!duplicated(election_docs$id), ]
  nothing_docs<-durhamevp::get_candidate_documents(cand_document_id = c(23058:23834))
  more_election_docs<-durhamevp::get_candidate_documents(cand_document_id = c(12244:14140))
  more_election_docs<-more_election_docs[more_election_docs$status==77,]
  nothing_docs<-nothing_docs[nothing_docs$status=="0",]
  election_docs<-dplyr::bind_rows(election_docs,
                                 more_election_docs)
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

  the_corpus<-the_corpus[,c("id", "title", "url", "description", "type", "page", "publication_date", "word_count", "ocr", "election_article", "EV_article", "just_election", "corpus_election", "electoral_nature", "violent_nature", "candidate_document_id")]
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

  res_set<-the_dataframe[have_word, ]

  res_have_word<-the_dataframe[,results_col][have_word]
  res_dont_have_word<-(the_dataframe[,results_col][dont_have_word]-1)^2

  print(sum(res_have_word)/length(res_have_word))
  print(sum(res_dont_have_word)/length(res_dont_have_word))

  res_set
}

has_word <- function(the_dataframe, contains_words, text_col="ocr", results_col="EV_article"){
  #' Function to tally cases with and without a particular words
  #' @param the_dataframe the dataframe with a text column and a results column
  #' @param contains_words the word (or other regex) to search for
  #' @param text_col the column containing the text
  #' @param results_col the column containing the classification

  have_word<-grepl(contains_words, the_dataframe[,text_col])
  dont_have_word<-!have_word

  res_set<-the_dataframe[have_word,]
  res_setEV<-subset(res_set, res_set[,results_col]==1)
  res_setNEV<-subset(res_set, res_set[,results_col]==0)

  resall<-nrow(res_set)/nrow(the_dataframe)
  resEV<-nrow(res_setEV)/nrow(subset(the_dataframe,the_dataframe[,results_col]==1))
  resNEV<-nrow(res_setNEV)/nrow(subset(the_dataframe,the_dataframe[,results_col]==0))

  res<-c(resall, resEV, resNEV)
  return(res)
}

classifier_select_docs <- function(classifier, new_docs, text_field="description", return_logical=FALSE, logical_to_prob=FALSE, class_to_keep=1, boolean=FALSE, xgb.cutpoint=.5, stem=FALSE, ...){
  #' Subsets a dataframe of documents based on a classifier.
  #'
  #' @param classifier A classifier to perform the classification: either a naive bayes (quanteda) or xgboost (xgboost)
  #' @param new_docs A data frame or dfm containing the documents to classify
  #' @param text_field The field containing the text to classify
  #' @param return_logical Should the function return the subset of documents (FALSE) or a logical vector indicating the subset of document (TRUE).
  #' @param logical_to_prob return_logical == TRUE class probabilities can be returned instead of class categories (TRUE).
  #' @param class_to_keep The classifier class to keep
  #' @param stem stem words (in preprocessing)
  #' @param ... other arguments to be passed to \code{preprocess_corpus}
  #' @return Either the subset of the \code{docs_df} which is classified as \code{class_to_keep} or a logical vector indicating this subset (depending on value of \code{return_logical}).
  #' @export

  is.nb<-function(x){
    "textmodel_nb" %in% class(x)
  }

  is.xgb<-function(x){
    "xgb.Booster" %in% class(x)
  }
  if(quanteda::is.dfm(new_docs)){
    the_dfm <- new_docs

  } else if(is.data.frame(new_docs)){
    the_corpus <- quanteda::corpus(new_docs[,c(text_field), drop=FALSE], text_field = text_field)
    the_dfm<-durhamevp::preprocess_corpus(the_corpus, stem=stem,...)
  } else {
    stop(paste("new_docs type not suported [class ", class(new_docs), "]"))
  }

  if(boolean){
    the_dfm<-quanteda::dfm_weight(the_dfm, scheme="boolean")
  }


  if(is.nb(classifier)){

    # code for quanteda native bayes models
    the_dfm<-quanteda::dfm_select(the_dfm, classifier$x)
    want_these<-predict(classifier, newdata = the_dfm, type="class")==class_to_keep
  } else if (is.xgb(classifier)){

    # code for xgboost models
    the_dfm<-quanteda::dfm_select(the_dfm, classifier$feature_names)
    the_matrix<-as(the_dfm, "dgCMatrix")
    missing_cols<- classifier$feature_names[!classifier$feature_names %in% colnames(the_matrix)]
    missing_dat<-Matrix::Matrix(rep(0, nrow(the_matrix)*length(missing_cols)), nrow=nrow(the_matrix), ncol=length(missing_cols))
    colnames(missing_dat)<-missing_cols
    the_matrix<-cbind(the_matrix, missing_dat)
    the_matrix<-the_matrix[,match(classifier$feature_names, colnames(the_matrix))]
    probs<-predict(classifier, the_matrix, outputmargin = FALSE)
    the_class<- as.numeric(probs > xgb.cutpoint)
    want_these <- the_class==class_to_keep
  } else {
    stop(paste("Classifier type not suported [class ", class(classifier), "]"))
  }

  if(return_logical){
    if (logical_to_prob){
      return(probs)
    } else {
      return(want_these)
    }
  }

  new_docs[want_these,]
}


classifier_selection_description<-function(train, new_docs, text_field="description", class_to_keep=1, training_classify_var="EV_article", prior="uniform", classifier_type="xgboost", stem_dfm=FALSE, return_logical=FALSE, logical_to_prob=FALSE, ...){
  #' Classifies new documents on a labeled training set (description).
  #' @param train a data frame with the training documents.
  #' @param new_docs the documents to classify.
  #' @param text_field the text field (must be the same in the training documents and the documents to classify).
  #' @param class_to_keep the class (0 or 1) to keep.
  #' @param training_classify_var the variable containing the labels in the training set.
  #' @param prior for naive bayes classifier only.
  #' @param classifier_type which classifier to use (xgboost or nb (naive Bayes))
  #' @param stem for preprocessing
  #' @param return_logical return the subset of documents or a logical vector indicating that subset.
  #' @param ... other arguments to be passed to \code{preprocess_corpus}.
  #' @export

  train_corpus <- quanteda::corpus(train[,c(text_field, training_classify_var)], text_field = text_field)
  train_dfm <- durhamevp::preprocess_corpus(train_corpus, stem=stem_dfm, ...)
  classifier <- evp_classifiers(train_dfm=train_dfm, classifier_type=classifier_type, training_classify_var=training_classify_var, prior=prior)


  new_docs_subset<-durhamevp::classifier_select_docs(classifier=classifier, new_docs=new_docs, text_field = text_field, class_to_keep = class_to_keep, stem=stem_dfm, return_logical=return_logical, logical_to_prob=logical_to_prob, ...)

  new_docs_subset
}

classifier_selection_ocr<-function(train, new_docs, text_field="ocr", class_to_keep=1, training_classify_var="EV_article", prior="uniform", return_logical=FALSE, logical_to_prob=FALSE, classifier_type="xgboost", ...){

  #' Classifies new documents on a labeled training set (description)
  #' @export
  train_corpus<-quanteda::corpus(train[,c(text_field, training_classify_var)], text_field = text_field)
  train_dfm<-durhamevp::preprocess_corpus(train_corpus, ...)
  classifier<-evp_classifiers(train_dfm=train_dfm, classifier_type=classifier_type, training_classify_var=training_classify_var, prior=prior)
  new_docs_subset<-durhamevp::classifier_select_docs(classifier=classifier, new_docs=new_docs, text_field = text_field, class_to_keep = class_to_keep, return_logical = return_logical, logical_to_prob=logical_to_prob)

  new_docs_subset
}

get_candidates_fromarchivesearchresults<-function(archivesearchresults, include_ocr=FALSE, restrict_EW=TRUE, restrict_classified=TRUE){
  #' Gets candidate documents from archivesearchresults.
  #' This is somewhat challenging because sometimes the url has 'download' in sometimes it doesn't
  #' This function includes a hack to find either case.
  #' By default it excludes articles from publications in Ireland and Scotland, and documents already classified
  #' as 1 (already downloaded), 3 (verbatim repeat), 4 (Ireland), 5 (Scotland), 6 (Abroad)

  #' @param archivesearchresults The archive search results (including a url column)
  #' @param include the ocr in the download (will slow down query)
  #' @param restrict_EW remove results published in Republic of Ireland and Scotland
  #' @param restrict_classified remove results already classified as 1, 3, 4, 5 or 6
  #' @return Candidate documents with urls matching the urls in archivesearchresults
  #' @export

  archivesearchresults$url_std<-sub("/viewer/download", "/viewer", archivesearchresults$url)
  the_candidates<-get_candidate_documents(cand_document_id = "all", c(archivesearchresults$url, archivesearchresults$url_std), include_ocr=include_ocr)

  if(restrict_EW){
    the_candidates <- dplyr::filter(the_candidates, !grepl('Republic of Ireland|Scotland', publication_location))
  }
  if(restrict_classified){
    the_candidates <- dplyr::filter(the_candidates, !status %in% c("1", "3", "4", "5", "6"))
  }

  the_candidates
}

searches_to_dfm<- function(archivesearches){
  #' Convert a set of search results to a dfm.
  #' @param archivesearches the results of archivesearches
  #' @export
  searches_to_string<-archivesearches %>%
    dplyr::mutate(std_url = sub("download/", "", url)) %>%
    group_by(std_url) %>%
    dplyr::summarise(search_string=paste(search_text, collapse = " "))
  searchstring_corpus <- quanteda::corpus(searches_to_string, text_field="search_string")
  searchstring_dfm<-quanteda::dfm(searchstring_corpus)

  searchstring_dfm
}

classifier_selection_keywords<-function(train, archivesearchresults, class_to_keep=1, training_classify_var="EV_article", prior="docfreq", text_field="ocr", classifier_type="xgboost", mode="select",
                                        eval_options=list(keywords=c("candidate","poll","election", "stone","riot", "mob", "husting", "disturbance", "rough", "incident"),
                                                          text_field="ocr",
                                                          eval_classify_var="EV_article",
                                                          eval_dfm_classifications="foo")){
  #'Classify documents based on keywords.
  #'
  #'\code{classifier_selection_keyword} uses a classifier to select document from keywords.
  #'In 'select' mode the keywords are constructed from the archivesearchresults. In 'eval' mode the keywords are taken from the eval_options and a binary dfm is constructed from the document text. In 'eval_dfm' mode the keywords are taken from the dfm columns, and the eval_classify_var should be a docvar of the dfm.
  #'@param train the training set of documents
  #'@param classifier_type The type of classifer to use ("nb" = naive bayes, "xgboost"=xgboost)
  #'@param mode Should the documents be selected ("select") or the document selection be evaluated from text field("eval"), or evaluated from a dfm ("eval_dfm") (evaluation assumes search results have been classified)
  #'@export
  #'

  # This code does work but should be changed to run through evp_classifiers function


  if (mode=="select"){
    search_dfm<-searches_to_dfm(archivesearchresults)
    train_corpus<-quanteda::corpus(train[,c(text_field, training_classify_var)], text_field = text_field)
    train_dfm<-quanteda::dfm(train_corpus, select=colnames(search_dfm))
    search_dfm<-quanteda::dfm_select(search_dfm, train_dfm)
  } else if(mode=="eval"){
    search_corpus<-quanteda::corpus(archivesearchresults, text_field=eval_options$text_field)
    search_dfm<-quanteda::dfm(search_corpus, select=eval_options$keywords)
    train_corpus<-quanteda::corpus(train[,c(text_field, training_classify_var)], text_field = text_field)
    train_dfm<-quanteda::dfm(train_corpus, select=colnames(search_dfm))
    search_dfm<-quanteda::dfm_select(search_dfm, train_dfm)
  } else if (mode=="eval_dfm"){
    search_dfm<-searches_to_dfm(archivesearchresults)
    docvars(search_dfm, "EV_article")<-eval_options$eval_dfm_classifications
    train_dfm<-search_dfm

  } else {
    stop("Unsupported mode.")
  }
  classifier<-durhamevp::evp_classifiers(train_dfm, classifier_type, training_classify_var, prior)

  if(mode %in% c("eval")){
    archivesearchresults$selected<-classifier_select_docs(classifier, search_dfm, boolean = TRUE, return_logical=TRUE)
    return(archivesearchresults)
  }
  if(mode %in% c("eval_dfm")){
    docvars(search_dfm, "selected")<-classifier_select_docs(classifier, search_dfm, boolean = TRUE, return_logical=TRUE)
    return(search_dfm)
  }
  want_these_std_url<-docvars(classifier_select_docs(classifier, search_dfm, boolean = TRUE), "std_url")

  archivesearchresults[archivesearchresults$std_url %in% want_these_std_url,]
}


evp_classifiers<-function(train_dfm, classifier_type, training_classify_var, prior){
  #' wrappers round classifiers
  #'
  #' @export
  if(classifier_type=="nb"){
    classifier<-quanteda::textmodel_nb(train_dfm, y=quanteda::docvars(train_dfm, training_classify_var), prior=prior)

  } else if (classifier_type=="xgboost"){
    train_dfms <- durhamevp::split_dfm(train_dfm, n_train=floor(nrow(train_dfm)*.8))
    dtrain <- durhamevp::dfm_to_dgCMatrix(train_dfms$training_set, training_classify_var = training_classify_var)
    dval <- durhamevp::dfm_to_dgCMatrix(train_dfms$testing_set, training_classify_var = training_classify_var)
    classifier<-xgboost::xgb.train(data=dtrain,
                                   nrounds=1000,
                                   print_every_n = 20,
                                   early_stopping_rounds = 10,
                                   maximize = F,
                                   eval_metric="logloss",
                                   verbose = 1,
                                   watchlist=list(train=dtrain, val=dval),
                                   eta=.03,
                                   max_depth=6,
                                   gamma=1,
                                   n_estimators=100,
                                   objective="binary:logistic",
                                   booster="dart")
    #dtrain <- durhamevp::dfm_to_dgCMatrix(train_dfm, training_classify_var = training_classify_var)
    #classifier<-xgboost::xgboost(data=dtrain, nrounds=1000, print_every_n = 20, early_stopping_rounds = 10, objective="binary:logistic")
    } else if (classifier_type=="xgboost.cv"){
      train_dfms <- durhamevp::split_dfm(train_dfm, n_train=floor(nrow(train_dfm)*.8))
      dtrain <- durhamevp::dfm_to_dgCMatrix(train_dfms$training_set, training_classify_var = training_classify_var)
      dval <- durhamevp::dfm_to_dgCMatrix(train_dfms$testing_set, training_classify_var = training_classify_var)
      classifier<-xgboost::xgb.cv(
                                  data=dtrain,
                                  nrounds=300,
                                  nfold=5,
                                  early_stopping_rounds = 10,
                                     metrics="logloss",
                                     watchlist=list(train=dtrain, val=dval),
                                     objective="binary:logistic")
      print(classifier)
      #dtrain <- durhamevp::dfm_to_dgCMatrix(train_dfm, training_classify_var = training_classify_var)
      #classifier<-xgboost::xgboost(data=dtrain, nrounds=1000, print_every_n = 20, early_stopping_rounds = 10, objective="binary:logistic")
    } else {
    stop(paste("Classifier type", classifier_type, "not supported."))
  }

  classifier
}
dfm_to_dgCMatrix<-function(the_dfm, boolean=FALSE, training_classify_var="EV_article"){
  #'@export

  if(boolean){
    the_dfm<-quanteda::dfm_weight(the_dfm, scheme="boolean")
  }
  the_dgCMatrix<-as(the_dfm, "dgCMatrix")

  the_labels<-quanteda::docvars(the_dfm, training_classify_var)

  the_dgCMatrix<-xgboost::xgb.DMatrix(data=the_dgCMatrix, label=the_labels)

  the_dgCMatrix
}

documents_to_latex<-function(out_docs, include_description=TRUE, include_ocr=TRUE, pandoc_output=FALSE){
  #' Convert dataframe of documents to form suitable for asis printing in knitr or Rmarkdown documents.
  #'
  #' @param out_docs data frame containing the following fields: id, title, description (optional), ocr (optional)
  #' @param include_description include description field in output
  #' @param include_ocr include ocr field in output
  #' @param pandoc_output should function produce pandoc output for Rmarkdown (if FALSE the output is LaTex output for knitr).
  #' @export
  for (doc in 1:nrow(out_docs)){
    if(pandoc_output){
      cat("\n\n")
      cat(paste0("## ", reportRx::sanitizestr(out_docs[doc, "title"]), " (id: ", out_docs[doc, "id"], ")"))
      cat("\n\n")
    } else {
      cat(paste0("\\subsection{", reportRx::sanitizestr(out_docs[doc, "title"]), " (id: ", out_docs[doc, "id"], ")}"))
    }
    cat(paste0("  \n"))
    if(include_description){
      if(pandoc_output){
        cat("\n\n")
        cat(paste0("### description"))
        cat("\n\n")
        cat("  \n")
      } else {
        cat(paste0("\\subsubsection{description}"))
        cat(paste0("  \n"))
      }
      #cat(Hmisc::latexTranslate(out_docs[doc, "description"]))
      #knitr::knit_print(out_docs[doc, "description"])
      cat(reportRx::sanitizestr(out_docs[doc, "description"]))
    }
    if(include_ocr){
      if(pandoc_output){
        cat("\n\n")
        cat(paste0("### OCR"))
        cat(paste0("  \n"))
      } else {
        cat(paste0("\\subsubsection{OCR}"))
        cat(paste0("  \n"))
      }

      #knitr::knit_print(out_docs[doc, "ocr"])
      cat(reportRx::sanitizestr(out_docs[doc, "ocr"]))
      cat("\n\n")
      cat("\n\n")
      cat("  \n")
    }
  }
}


## just testing
