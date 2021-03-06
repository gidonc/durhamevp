## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval=FALSE,
  comment = "#>"
)

documents_to_latex<-function(out_docs, include_description=TRUE, include_ocr=TRUE, pandoc_output=FALSE){
  for (doc in 1:nrow(out_docs)){
    if(pandoc_output){
      cat("\n\n")
      cat(paste0("## ", reportRx::sanitizestr(out_docs[doc, "title"]), " (id: ", out_docs[doc, "id"], ")"))
      cat("\n\n")
    } else {
      cat(paste0("\\subsection{", reportRx::sanitizestr(out_docs[doc, "title"]), " (id: ", out_docs[doc, "id"], ")}"))
      cat(paste0("  \n"))
    }
    if(include_description){
      if(pandoc_output){      
        cat("\n\n")
        cat(paste0("### description"))
        cat("\n\n")
      } else {
        cat(paste0("\\subsubsection{description}"))
        cat(paste0("  \n"))
      }
      #cat(Hmisc::latexTranslate(out_docs[doc, "description"]))
      #knitr::knit_print(out_docs[doc, "description"])
      cat(reportRx::sanitizestr(out_docs[doc, "description"]))
      cat("\n\n")
    }
    if(include_ocr){
      if(pandoc_output){
        cat("\n\n")
        cat(paste0("### OCR"))
        cat("\n\n")
      } else {
        cat(paste0("\\subsubsection{OCR}"))
        cat(paste0("  \n"))
      }

      #knitr::knit_print(out_docs[doc, "ocr"])
      cat(reportRx::sanitizestr(out_docs[doc, "ocr"]))
      if(pandoc_output){
        cat("  ")
      } else {
        cat("   \n")
      }
    }
  }
}


## ----message=FALSE, warning=FALSE, echo=FALSE----------------------------
#  library(durhamevp)
#  
#  ## also using tidyverse functions
#  library(tidyverse)

## ----getdocsets----------------------------------------------------------
#  
#  
#  alldocs<-get_document("all")
#  alldocs<-alldocs %>%
#    dplyr::mutate(std_url = sub("download/", "", url))
#  
#  docallocs<-get_allocation("all", allocation_type = "coding")
#  
#  docallocs<-left_join(docallocs, alldocs, by=c("document_id"="id"))
#  
#  classdocs<-filter(docallocs, coding_complete==1, electoralviolence_nature %in% c("false", "true"))
#  classdocs<-classdocs %>%
#    dplyr::mutate(std_url = sub("download/", "", url)) %>%
#    dplyr::mutate(unclass=0, classified=1) %>%
#    dplyr::mutate(EV_article=ifelse(electoralviolence_nature=="true", 1, 0))
#  
#  
#  systematic_search_terms<-c("incident",
#                             "riot",
#                             "mob",
#                             "rough",
#                             "disturbance",
#                             "husting",
#                             "adjourn",
#                             "magistrate",
#                             "police",
#                             "prison",
#                             "candidate",
#                             "election",
#                             "party")
#  all_searches<-get_archivesearches()
#  systematic_searches<-all_searches%>%
#    dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
#    filter(search_text %in% systematic_search_terms)
#  
#  results_systematic_searches<-get_archivesearchresults(archive_search_id = systematic_searches$id) %>%
#    left_join(all_searches, by=c("archive_search_id"="id")) %>%
#    mutate(std_url = sub("download/", "", url))
#  
#  
#  is_classified <-results_systematic_searches$std_url %in% classdocs$std_url
#  
#  classified_results<-results_systematic_searches[is_classified,]
#  
#  
#  

## ----trainkeywordclass---------------------------------------------------
#  class_res_dfm<-durhamevp::searches_to_dfm(classified_results)
#  the_urls<-quanteda::docvars(class_res_dfm, "std_url")
#  ordered_cr<-classified_results[match(quanteda::docvars(class_res_dfm, "std_url"), classified_results$std_url),]
#  
#  summary_class <- classdocs %>%
#    group_by(std_url, document_id) %>%
#    mutate(EV_article=electoralviolence_nature=="true") %>%
#    summarize(EV_article=max(EV_article), sum_EV_article=sum(EV_article), n=n())
#  
#  ordered_classified<-summary_class[match(quanteda::docvars(class_res_dfm, "std_url"), summary_class$std_url),]
#  
#  quanteda::docvars(class_res_dfm, "EV_article")<-ordered_classified$EV_article
#  
#  # use 85 per cent of the classified data to train classifier
#  # the other 15 per cent are used to as the testing set
#  n_train <- round(nrow(class_res_dfm)*.85)
#  the_sets<-split_dfm(class_res_dfm, n_train = n_train)
#  testing_urls<-quanteda::docvars(the_sets$testing_set, "std_url")
#  training_urls<-quanteda::docvars(the_sets$training_set, "std_url")
#  
#  # train xgboost keyword classifier
#  classifier<-durhamevp::evp_classifiers(the_sets$training_set, "xgboost", "EV_article", "uniform")
#  
#  # train naive bayes keyword classifier
#  classifier_nb<-durhamevp::evp_classifiers(the_sets$training_set, "nb", "EV_article", "uniform")

## ------------------------------------------------------------------------
#  # add predictions to the training and testing sets
#  quanteda::docvars(the_sets$training_set, "predicted_keywords")<-predict(classifier, newdata = the_sets$training_set, type="class")
#  quanteda::docvars(the_sets$testing_set, "predicted_keywords")<-predict(classifier, newdata = the_sets$testing_set, type="class")
#  quanteda::docvars(the_sets$testing_set, "predicted_keywords_nb")<-predict(classifier_nb, newdata = the_sets$testing_set, type="prob")[,2]
#  
#  quanteda::docvars(the_sets$testing_set, "pred_class_keywords")<-factor(as.numeric(quanteda::docvars(the_sets$testing_set, "predicted_keywords")>.5))
#  
#  results_table<-as.data.frame.matrix(caret::confusionMatrix(data=quanteda::docvars(the_sets$testing_set, "pred_class_keywords"), reference=factor(quanteda::docvars(the_sets$testing_set, "EV_article")), mode="prec_recall", positive="1")$table)
#  
#  ## **** probably delete****
#  tmp_train<-quanteda::docvars(the_sets$training_set)
#  p1<-ggplot(tmp_train, aes(predicted_keywords, EV_article))+
#    geom_point(position=position_jitter(height=.1))+
#    stat_smooth(method="glm", method.args = list(family="binomial"))+
#    ggtitle("performance of the xgboost keyword classifier on the training set")
#  
#  tmp_test<-quanteda::docvars(the_sets$testing_set)
#  p2<-ggplot(tmp_test, aes(predicted_keywords, EV_article))+
#    geom_point(position=position_jitter(height=.1))+
#    stat_smooth(method="glm", method.args = list(family="binomial"))+
#    ggtitle("performance of the xgboost keyword classifier on the testing set")
#  p2
#  
#  p3<-ggplot(tmp_test, aes(predicted_keywords_nb, EV_article))+
#    geom_point(position=position_jitter(height=.1))+
#    stat_smooth(method="glm", method.args = list(family="binomial"))+
#    ggtitle("performance of the naive bayes keyword classifier on the testing set")
#  print(p3)
#  p4<-ggplot(tmp_test, aes(y=EV_article))+
#    geom_point(colour="red", position=position_jitter(height=.1), aes(x=predicted_keywords_nb))+
#    stat_smooth(method="glm",colour="red",  method.args = list(family="binomial"), aes(x=predicted_keywords_nb))+
#    geom_point(colour="green", position=position_jitter(height=.1), aes(x=predicted_keywords))+
#    stat_smooth(method="glm",colour="green",  method.args = list(family="binomial"), aes(x=predicted_keywords))+
#    ggtitle("performance of the naive bayes keyword classifier on the testing set")
#  

## ------------------------------------------------------------------------
#  undownloaded_candidates<-get_candidate_documents(status =c("0", "2", "4", "5", "6", "7", "8", ""), include_ocr=FALSE)
#  undownloaded_candidates<- undownloaded_candidates %>%
#    dplyr::mutate(std_url = sub("download/", "", url))
#  
#  need_classifying <-results_systematic_searches$std_url %in% undownloaded_candidates$std_url
#  
#  toclassify_results<-results_systematic_searches[need_classifying,]
#  
#  toclass_res_dfm<-durhamevp::searches_to_dfm(toclassify_results)
#  classify_urls<-quanteda::docvars(toclass_res_dfm, "std_url")
#  
#  toclass_othervars <- toclassify_results %>%
#    select(newspaper_title, publication_date, publication_location, type, std_url) %>%
#    group_by(std_url,newspaper_title, publication_date, publication_location, type) %>%
#    summarize ()
#  
#  ordered_tcov<-toclass_othervars[match(quanteda::docvars(toclass_res_dfm, "std_url"), toclass_othervars$std_url),]
#  
#  quanteda::docvars(toclass_res_dfm, "publication_date")<-ordered_tcov$publication_date
#  quanteda::docvars(toclass_res_dfm, "publication_location")<-ordered_tcov$publication_location
#  quanteda::docvars(toclass_res_dfm, "newspaper_title")<-ordered_tcov$newspaper_title
#  quanteda::docvars(toclass_res_dfm, "type")<-ordered_tcov$type
#  
#  # this step reorders the dfm to have the columns in the same order as the training_set dfm
#  toclass_res_dfm<-quanteda::dfm_select(toclass_res_dfm, the_sets$training_set)
#  
#  # this step actually implements the prediction
#  quanteda::docvars(toclass_res_dfm, "predicted_keywords")<-predict(classifier, newdata = toclass_res_dfm, type="class")
#  

## ------------------------------------------------------------------------
#  undownloaded_candidates_with_kwpred<-left_join(undownloaded_candidates, quanteda::docvars(toclass_res_dfm), by=c("std_url", "publication_date", "publication_location", "type"))
#  
#  # add election name
#  undownloaded_candidates_with_kwpred<-undownloaded_candidates_with_kwpred %>%
#    group_by(publication_date) %>%
#    summarise() %>%
#    mutate(election_name=date_to_election(publication_date)) %>%
#    right_join(undownloaded_candidates_with_kwpred, by="publication_date")
#  
#  ##  If working by election subset to 1874 (or other desired election)
#  
#  kwclass_1874<-undownloaded_candidates_with_kwpred %>%
#    filter(election_name=="1874") %>%
#    arrange(-predicted_keywords)
#  
#  hist(kwclass_1874$predicted_keywords)
#  
#  
#  kwclass_1874_top5000<-kwclass_1874 %>%
#    slice(1:5000) %>%
#    select(id, url, publication_title, description, status, g_status, title, status_writer, predicted_keywords)
#  
#  kwclass_1874_top5000$status<-1
#  kwclass_1874_top5000$status_writer<-"xgboost_keyword"
#  

## ---- eval=FALSE---------------------------------------------------------
#  to_csv<-kwclass_1874_top5000[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer", "predicted_keywords")]
#  
#  csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".csv"))
#  
#  write.csv(to_csv, file=csv_filename, row.names = FALSE)

## ------------------------------------------------------------------------
#  
#  top_ones<-quanteda::docvars(toclass_res_dfm, "predicted_keywords")>.9
#  quanteda::docvars(toclass_res_dfm, "std_url")[top_ones][1:5]
#  
#  as_df<-as.tbl(quanteda::docvars(toclass_res_dfm)) %>%
#    mutate(pubyear=lubridate::year(publication_date))
#  
#  #system.time(as_df <- as_df %>%
#  #  mutate(election=date_to_election(publication_date)))
#  
#  as_df<-as_df %>%
#    group_by(publication_date) %>%
#    summarise() %>%
#    mutate(election_name=date_to_election(publication_date)) %>%
#    right_join(as_df, by="publication_date")
#  pp1<-as_df %>%
#    left_join(election_dates, by="election_name") %>%
#    filter(keywords_13_complete=="Y") %>%
#    ggplot(aes(election_name, predicted_keywords, group=election_name)) +
#    geom_boxplot()
#  
#  pp1<-as_df %>%
#    left_join(election_dates, by="election_name") %>%
#    filter(keywords_13_complete=="Y") %>%
#    ggplot(aes(election_name, predicted_keywords, group=election_name)) +
#    geom_violin(trim=FALSE)
#  
#  as_df <- as_df %>%
#    mutate(electionn=as.numeric(election))
#  
#  prob_class<-function(x){
#    case_when(
#      x>.75 ~ "highly likely (>.75)",
#      x>.5  ~ "likely (>.5 & <.75)",
#      x>.25 ~ "unlikely (<.5 & >.25)",
#      x<.25 ~ "very unlikely(<.25)",
#      TRUE ~ NA_character_
#    )
#  }
#  pp4<-as_df %>%
#    mutate(prob_EV_keywords=prob_class(predicted_keywords)) %>%
#    mutate(electionn=as.numeric(election_name)) %>%
#    filter(electionn<1875) %>%
#    group_by(election_name, electionn, prob_EV_keywords) %>%
#    tally() %>%
#    ggplot(aes(electionn, n))+
#    facet_wrap(~prob_EV_keywords, scales="free")+
#    geom_point()+
#    geom_line()+
#    ylim(0, NA) +
#    ggtitle("Number of Currently Undownloaded Articles in Each Election by EV_Article Likelihood (Keyword Classifier)")
#  
#  pp2<-as_df %>%
#    group_by(electionn) %>%
#    tally()%>%
#    ggplot(aes(electionn, n)) +
#    geom_point() +
#    geom_line()
#  pp2
#  
#  dim(results_systematic_searches)
#  rss<-results_systematic_searches %>%
#    group_by(publication_date) %>%
#    summarise() %>%
#    mutate(election_name=date_to_election(publication_date)) %>%
#    right_join(results_systematic_searches, by="publication_date")
#  pp3<-rss %>%
#    group_by(election_name, search_text) %>%
#    tally()%>%
#    mutate(electionn=as.numeric(election_name)) %>%
#    left_join(durhamevp::election_dates, by="election_name") %>%
#    mutate(articles_per_day=n/monthsearch_duration) %>%
#    filter(keywords_13_complete=="Y") %>%
#    ggplot(aes(electionn, articles_per_day)) +
#    facet_wrap(~search_text, scales="free")+
#    geom_point() +
#    geom_line() +
#    ylim(0, NA) +
#    ggtitle("Articles (per day) Containing 13 Systematic Search Terms During Month Around Elections")+
#    theme_bw()
#  pp3
#  #link to searchers
#  
#  candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)
#  
#  candocs<-candocs %>%
#    dplyr::mutate(std_url = sub("download/", "", url))
#  candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)
#  
#  
#  candocs_training_set<-candocs[!candocs$std_url %in% testing_urls, ]
#  candocs_testing_set<-candocs[candocs$std_url %in% testing_urls, ]
#  descript_test<-classified_results[classified_results$std_url %in% testing_urls,]
#  
#  select_descript_xgb<-classifier_selection_description(candocs_training_set, descript_test, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=20, min_termfreq=20)
#  
#  descript_test <- descript_test %>%
#    mutate(xgb_select=select_descript_xgb)
#  
#  compare_docs<-descript_test%>%
#    inner_join(candocs_testing_set, by="description")
#  
#  table(compare_docs$xgb_select>.5, compare_docs$g_status)
#  
#  ggplot(compare_docs, aes(xgb_select, as.numeric(g_status==1)))+
#    geom_point(position="jitter")+
#    stat_smooth(method="glm", method.args = list(family="binomial"))+
#    ggtitle("Performance of the xgboost descrition classifier on the testing set")
#  
#  compare_docs<-descript_test%>%
#    group_by(std_url) %>%
#    summarise(maxselect=max(xgb_select)) %>%
#    left_join(candocs_testing_set, by="std_url")
#  
#  check_descript_xgb<-classifier_selection_description(candocs_training_set, candocs_training_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=50, min_termfreq=50)
#  
#  descript_test <- candocs_training_set %>%
#    mutate(xgb_select=check_descript_xgb)
#  
#  compare_docs<-descript_test%>%
#    inner_join(candocs_training_set, by="description")
#  
#  table(compare_docs$xgb_select>.5, compare_docs$g_status.x)
#  
#  ggplot(compare_docs, aes(xgb_select, as.numeric(g_status.x=="1")))+
#    geom_point(position="jitter")+
#    stat_smooth(method="glm", method.args = list(family="binomial"))+
#    ggtitle("Performance of the xgboost descrition classifier on the training set")
#  

## ---- results='asis'-----------------------------------------------------
#  documents_to_latex(sample_n(select_descript_xgb, 30), include_ocr=FALSE, pandoc_output=TRUE)
#  

