##----pilot.packages----
library(reportRx) # note using this just for sanitizing LaTex output, not included in durhamevp package requirements
library(tidyverse)
library(quanteda)
library(durhamevp)
library(RTextTools) # not sure if this is included as part of the durhamevp package


##----selection.pilot.custom.functions----
documents_to_latex<-function(out_docs, include_description=TRUE, include_ocr=TRUE, pandoc_output=FALSE){
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



get_random_candidates<-function(archivesearchresults, n_random){
  the_random<-dplyr::sample_n(archivesearchresults, n_random)
  the_candidates<-durhamevp::get_candidates_fromarchivesearchresults(the_random)

  the_candidates
}

switch_url_format<-Vectorize(function(url){
  if(grepl("viewer/download", url)){
    switch_url<-sub("viewer/download", "viewer", url)
  } else if(grepl("viewer/", url)){
    switch_url<-sub("viewer/", "viewer/download/", url)
  }
  switch_url
})
url_to_id<-function(url){
  str_split(url, "bl/", simplify=TRUE)[,2]
}
##----initial.keywords----
# election riot, election incident, election disturbance

##----download.R.set----
# In fact we have some classified docs non-EV cases here King's R set doesn't contain any non-cases
# I think we should use our more extensive information here

alldocs<-get_document("all")
alldocs<-alldocs %>%
  dplyr::mutate(std_url = sub("download/", "", url))

docallocs<-get_allocation("all", allocation_type = "coding")

docallocs<-left_join(docallocs, alldocs, by=c("document_id"="id"))

classdocs<-filter(docallocs, coding_complete==1, electoralviolence_nature %in% c("false", "true"))
classdocs<-classdocs %>%
  dplyr::mutate(std_url = sub("download/", "", url)) %>%
  dplyr::mutate(unclass=0, classified=1) %>%
  dplyr::mutate(EV_article=ifelse(electoralviolence_nature=="true", 1, 0))


##----dataprep----
systematic_search_terms<-c("election",
                           "riot",
                           "disturbance",
                           "incident",
                           "police",
                           "husting",
                           "magistrate",
                           "party",
                           "rough",
                           "stone")
all_searches<-get_archivesearches()
systematic_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(search_text %in% systematic_search_terms)

results_systematic_searches<-get_archivesearchresults(archive_search_id = systematic_searches$id) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))


is_classified <-results_systematic_searches$std_url %in% classdocs$std_url

classified_results<-results_systematic_searches[is_classified,]


##----stage1----

class_res_dfm<-durhamevp::searches_to_dfm(classified_results)
the_urls<-docvars(class_res_dfm, "std_url")
ordered_cr<-classified_results[match(docvars(class_res_dfm, "std_url"), classified_results$std_url),]

summary_class <- classdocs %>%
  group_by(std_url, document_id) %>%
  mutate(EV_article=electoralviolence_nature=="true") %>%
  summarize(EV_article=max(EV_article), sum_EV_article=sum(EV_article), n=n())

ordered_classified<-summary_class[match(docvars(class_res_dfm, "std_url"), summary_class$std_url),]

docvars(class_res_dfm, "EV_article")<-ordered_classified$EV_article


the_sets<-split_dfm(class_res_dfm, n_train = 3000)
testing_urls<-docvars(the_sets$testing_set, "std_url")
training_urls<-docvars(the_sets$training_set, "std_url")

classifier<-durhamevp::evp_classifiers(the_sets$training_set, "xgboost", "EV_article", "uniform")
classifier_nb<-durhamevp::evp_classifiers(the_sets$training_set, "nb", "EV_article", "uniform")

quanteda::docvars(the_sets$training_set, "predicted_keywords")<-predict(classifier, newdata = the_sets$training_set, type="class")
quanteda::docvars(the_sets$testing_set, "predicted_keywords")<-predict(classifier, newdata = the_sets$testing_set, type="class")
quanteda::docvars(the_sets$testing_set, "predicted_keywords_nb")<-predict(classifier_nb, newdata = the_sets$testing_set, type="prob")[,2]

quanteda::docvars(the_sets$testing_set, "pred_class_keywords")<-factor(as.numeric(quanteda::docvars(the_sets$testing_set, "predicted_keywords")>.5))

as.data.frame.matrix(caret::confusionMatrix(data=quanteda::docvars(the_sets$testing_set, "pred_class_keywords"), reference=factor(quanteda::docvars(the_sets$testing_set, "EV_article")), mode="prec_recall", positive="1")$table)

ggplot(docvars(the_sets$training_set), aes(predicted_keywords, EV_article))+
  geom_point(position=position_jitter(height=.1))+
  stat_smooth(method="glm", method.args = list(family="binomial"))+
  ggtitle("performance of the xgboost keyword classifier on the training set")

ggplot(docvars(the_sets$testing_set), aes(predicted_keywords, EV_article))+
  geom_point(position=position_jitter(height=.1))+
  stat_smooth(method="glm", method.args = list(family="binomial"))+
  ggtitle("performance of the xgboost keyword classifier on the testing set")
ggplot(docvars(the_sets$testing_set), aes(predicted_nb, EV_article))+
  geom_point(position=position_jitter(height=.1))+
  stat_smooth(method="glm", method.args = list(family="binomial"))+
  ggtitle("performance of the naive bayes keyword classifier on the testing set")

ggplot(docvars(the_sets$testing_set), aes(y=EV_article))+
  geom_point(colour="red", position=position_jitter(height=.1), aes(x=arm::logit(predicted_nb)))+
  stat_smooth(method="glm",colour="red",  method.args = list(family="binomial"), aes(x=arm::logit(predicted_nb)))+
  geom_point(colour="green", position=position_jitter(height=.1), aes(x=arm::logit(predicted_keywords)))+
  stat_smooth(method="glm",colour="green",  method.args = list(family="binomial"), aes(x=arm::logit(predicted_keywords)))+
  ggtitle("performance of the naive bayes keyword classifier on the testing set")

##----stage2----
candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs<-candocs %>%
  dplyr::mutate(std_url = sub("download/", "", url))
candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)


candocs_training_set<-candocs[!candocs$std_url %in% testing_urls, ]
candocs_testing_set<-candocs[candocs$std_url %in% testing_urls, ]
descript_test<-classified_results[classified_results$std_url %in% testing_urls,]

select_descript_xgb<-classifier_selection_description(candocs_training_set, descript_test, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=20, min_termfreq=20)

descript_test <- descript_test %>%
  mutate(xgb_select=select_descript_xgb)

compare_docs<-descript_test%>%
  inner_join(candocs_testing_set, by="description")

table(compare_docs$xgb_select>.5, compare_docs$g_status)

ggplot(compare_docs, aes(xgb_select, as.numeric(g_status==1)))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args = list(family="binomial"))+
  ggtitle("Performance of the xgboost descrition classifier on the testing set")

compare_docs<-descript_test%>%
  group_by(std_url) %>%
  summarise(maxselect=max(xgb_select)) %>%
  left_join(candocs_testing_set, by="std_url")

check_descript_xgb<-classifier_selection_description(candocs_training_set, candocs_training_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=50, min_termfreq=50)

descript_test <- candocs_training_set %>%
  mutate(xgb_select=check_descript_xgb)

compare_docs<-descript_test%>%
  inner_join(candocs_training_set, by="description")

table(compare_docs$xgb_select>.5, compare_docs$g_status.x)

ggplot(compare_docs, aes(xgb_select, as.numeric(g_status.x=="1")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args = list(family="binomial"))+
  ggtitle("Performance of the xgboost descrition classifier on the training set")

##----stage3----
classdocs_testing_set<-classdocs[classdocs$std_url %in% testing_urls, ]
classdocs_training_set<-classdocs[classdocs$std_url %in% training_urls, ]

select_ocr_xgb<-classifier_selection_ocr(classdocs_training_set, classdocs_testing_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob = TRUE, min_termfreq=20, min_docfreq=20, stem=FALSE)

select_ocr_xgb<-classifier_selection_ocr(classdocs_training_set, classdocs_testing_set, classifier_type="xgboost.cv", return_logical = TRUE, logical_to_prob = TRUE, min_termfreq=20, min_docfreq=20, stem=FALSE)

ocr_test <- classdocs_testing_set %>%
  mutate(xgb_select=select_ocr_xgb) %>%
  mutate(xgb_class=select_ocr_xgb>.5)
compare_docs<-ocr_test%>%
  inner_join(classdocs_testing_set)
compare_docs<-mutate(compare_docs, EV_article=as.numeric(electoralviolence_nature=="true"))
f1<-lm(EV_article~xgb_select, compare_docs)
f2<-glm(EV_article~xgb_select, compare_docs, family="binomial")
stargazer::stargazer(f1, f2, type="text")

ggplot(compare_docs, aes(xgb_select, as.numeric(factor(electoralviolence_nature)=="true")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args=list(family=binomial))+
  ggtitle("Performance of xgboost on ocr on Testing Set")

#xtable::xtable(table(compare_docs$xgb_class, compare_docs$EV_article))
caret::confusionMatrix(factor(compare_docs$xgb_class), factor(compare_docs$electoralviolence_nature=="true"), mode="prec_recall")
select_ocr_xgb<-classifier_selection_ocr(classdocs_training_set, classdocs_training_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob = TRUE, min_termfreq=20, min_docfreq=20, stem=FALSE)
ocr_test <- classdocs_training_set %>%
  mutate(xgb_select=select_ocr_xgb)
compare_docs<-ocr_test%>%
  inner_join(classdocs_training_set)

f1<-lm(electoralviolence_nature=="true"~xgb_select, compare_docs)
f2<-glm(electoralviolence_nature=="true"~xgb_select, compare_docs, family="binomial")
stargazer::stargazer(f1, f2, type="text")

ggplot(compare_docs, aes(xgb_select, as.numeric(factor(electoralviolence_nature)=="true")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args=list(family=binomial))+
  ggtitle("Performance of xgboost on ocr on Training Set")


##----firstgostage1----
systematic_search_terms<-c("election",
                           "riot",
                           "disturbance",
                           "incident",
                           "police",
                           "husting",
                           "magistrate",
                           "party",
                           "rough",
                           "stone")
all_searches<-get_archivesearches()
systematic_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(search_text %in% systematic_search_terms)

results_systematic_searches<-get_archivesearchresults(archive_search_id = systematic_searches$id) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))


is_classified <-results_systematic_searches$std_url %in% classdocs$std_url

classified_results<-results_systematic_searches[is_classified,]

class_res_dfm<-durhamevp::searches_to_dfm(classified_results)

the_urls<-docvars(class_res_dfm, "std_url")
ordered_cr<-classified_results[match(docvars(class_res_dfm, "std_url"), classified_results$std_url),]
summary_class <- classdocs %>%
  group_by(std_url, document_id) %>%
  mutate(EV_article=electoralviolence_nature=="true") %>%
  summarize(EV_article=max(EV_article), sum_EV_article=sum(EV_article), n=n())

ordered_classified<-summary_class[match(docvars(class_res_dfm, "std_url"), summary_class$std_url),]

docvars(class_res_dfm, "EV_article")<-ordered_classified$EV_article


doit<-classifier_selection_keywords(classifed_results, classified_results, mode="eval_dfm", eval_options=list(eval_classify_var="EV_article", eval_dfm_classifications=ordered_classified$EV_article))

the_sets_4<-split_dfm(class_res_dfm, n_train = 3000)

classifier<-durhamevp::evp_classifiers(the_sets_4$training_set, "xgboost", "EV_article", "uniform")
classifier<-durhamevp::evp_classifiers(the_sets_4$training_set, "nb", "EV_article", "uniform")
quanteda::docvars(the_sets_4$training_set, "predicted")<-predict(classifier, newdata = the_sets_4$training_set, type="prob")[,2]
quanteda::docvars(the_sets_4$training_set, "predicted")<-predict(classifier, newdata = the_sets_4$training_set, type="prob")

ggplot(docvars(the_sets_4$training_set), aes(arm::logit(predicted), EV_article))+
  geom_point(position=position_jitter())+
  stat_smooth(method="glm", method.args = list(family="binomial"))

quanteda::docvars(the_sets_4$testing_set, "predicted")<-predict(classifier, newdata = the_sets_4$testing_set, type="class")
quanteda::docvars(the_sets_4$testing_set, "predicted")<-predict(classifier, newdata = the_sets_4$testing_set, type="prob")[,2]

quanteda::docvars(the_sets_4$testing_set, "pred_class")<-factor(as.numeric(quanteda::docvars(the_sets_4$testing_set, "predicted")>.5))

caret::confusionMatrix(data=quanteda::docvars(the_sets_4$testing_set, "pred_class"), reference=factor(quanteda::docvars(the_sets_4$testing_set, "EV_article")), mode="prec_recall", positive="1")


ggplot(docvars(the_sets_4$testing_set), aes(arm::logit(predicted), EV_article))+
  geom_point(position=position_jitter(height=.1))+
  stat_smooth(method="glm", method.args = list(family="binomial"))



##----firstgostage2----

candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs<-candocs %>%
  dplyr::mutate(std_url = sub("download/", "", url))
candocs$EV_article<-ifelse(candocs$g_status %in% c("1", "3"), 1, 0)

testing_urls<-docvars(the_sets_4$testing_set, "std_url")
training_urls<-docvars(the_sets_4$testing_set, "std_url")

candocs_training_set<-candocs[!candocs$std_url %in% testing_urls, ]
candocs_testing_set<-candocs[candocs$std_url %in% testing_urls, ]
descript_test<-classified_results[classified_results$std_url %in% testing_urls,]
candocs_training_set$EV_article
select_descript_xgb<-classifier_selection_description(candocs_training_set, descript_test, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=50, min_termfreq=50)

descript_test <- descript_test %>%
  mutate(xgb_select=select_descript_xgb)

compare_docs<-descript_test%>%
  inner_join(candocs_testing_set, by="description")

table(compare_docs$xgb_select, compare_docs$EV_article)

ggplot(compare_docs, aes(xgb_select, EV_article))+
  geom_point()+
  stat_smooth(method="glm", method.args = list(family="binomial"))

compare_docs<-descript_test%>%
  group_by(std_url) %>%
  summarise(maxselect=max(xgb_select)) %>%
  left_join(candocs_testing_set, by="std_url")

check_descript_xgb<-classifier_selection_description(candocs_training_set, candocs_training_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob=TRUE, stem=FALSE, min_docfreq=50, min_termfreq=50)

descript_test <- candocs_training_set %>%
  mutate(xgb_select=check_descript_xgb)

compare_docs<-descript_test%>%
  inner_join(candocs_training_set, by="description")

table(compare_docs$xgb_select, compare_docs$g_status.x)

ggplot(compare_docs, aes(xgb_select, as.numeric(g_status.x=="1")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args = list(family="binomial"))


classdocs_testing_set<-classdocs[classdocs$std_url %in% testing_urls, ]
aa<-classifier_selection_ocr(classdocs_training_set, classdocs_testing_set)

select_ocr_xgb<-classifier_selection_ocr(classdocs_training_set, classdocs_testing_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob = TRUE, min_termfreq=10, min_docfreq=10, stem=FALSE)

ocr_test <- classdocs_testing_set %>%
  mutate(xgb_select=select_ocr_xgb) %>%
  mutate(xgb_class=select_ocr_xgb>.5)
compare_docs<-ocr_test%>%
  inner_join(classdocs_testing_set)

f1<-lm(electoralviolence_nature=="true"~xgb_select, compare_docs)
f2<-glm(electoralviolence_nature=="true"~xgb_select, compare_docs, family="binomial")
stargazer::stargazer(f1, f2, type="text")

ggplot(compare_docs, aes(arm::logit(xgb_select), as.numeric(factor(electoralviolence_nature)=="true")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args=list(family=binomial))
ggplot(compare_docs, aes(xgb_select, as.numeric(factor(electoralviolence_nature)=="true")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args=list(family=binomial))

table(compare_docs$xgb_class, compare_docs$electoralviolence_nature)
select_ocr_xgb<-classifier_selection_ocr(classdocs_training_set, classdocs_training_set, classifier_type="xgboost", return_logical = TRUE, logical_to_prob = TRUE, min_termfreq=100, min_docfreq=100, stem=FALSE)
ocr_test <- classdocs_training_set %>%
  mutate(xgb_select=select_ocr_xgb)
compare_docs<-ocr_test%>%
  inner_join(classdocs_training_set)

f1<-lm(electoralviolence_nature=="true"~xgb_select, compare_docs)
f2<-glm(electoralviolence_nature=="true"~xgb_select, compare_docs, family="binomial")
stargazer::stargazer(f1, f2, type="text")

ggplot(compare_docs, aes(arm::logit(xgb_select), as.numeric(factor(electoralviolence_nature)=="true")))+
  geom_point(position="jitter")+
  stat_smooth(method="glm", method.args=list(family=binomial))
##----download.candocs----
# also get candidate documents for description classification purposes
candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)

candocs$EV_article<-ifelse(candocs$status %in% c("1", "3"), 1, 0)

candocs.r<-candocs[candocs$status %in% c("1", "3", "7", "8", "0"),]


##----initial.1832.results----
all_searches<-get_archivesearches()
initial_1832_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1832-01-01"), archive_date_start<lubridate::ymd("1832-12-31")) %>%
  filter(search_text %in% c("election riot", "election incident", "election disturbance"))

# a number of precisely duplicated searches here which return precisely duplicated results - we could handle this later on but here probably simplest to just use three distinct ones
# election riot - id 73
# election disturbance - id 81
# election incident - id 85
res_i_1832<-get_archivesearchresults(archive_search_id = c(73, 81, 85)) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

is_classified <-res_i_1832$std_url %in% classdocs$std_url

unclass_i_1832 <- res_i_1832[!is_classified,] %>%
  mutate(unclass=1, classified=0)

all_docs<- bind_rows(classdocs,
                     unclass_i_1832)
all_docs$fakeid<-1:nrow(all_docs)

##----descriptives.1832----
res_32_des<-res_i_1832 %>%
  group_by(url) %>%
  mutate(search_text=sub(" ", "_", search_text)) %>%
  select(url, search_text) %>%
  mutate(value=1) %>%
  spread(search_text, value, fill=0) %>%
  mutate(tot_art=nrow(.))

knitr::kable(res_32_des %>%
  group_by(election_riot, election_incident, election_disturbance) %>%
  summarize(n=length(tot_art), prop=n/tot_art[1])%>%
    arrange(-prop), digits = 2)

knitr::kable(res_32_des %>%
               group_by(election_riot) %>%
               summarize(n=length(tot_art), prop=n/tot_art[1]) %>%
               arrange(-election_riot),
             digits = 2)

knitr::kable(res_32_des %>%
               group_by(election_incident) %>%
               summarize(n=length(tot_art), prop=n/tot_art[1])%>%
               arrange(-election_incident),
             digits = 2)

knitr::kable(res_32_des %>%
               group_by(election_disturbance) %>%
               summarize(n=length(tot_art), prop=n/tot_art[1])%>%
               arrange(-election_disturbance),
             digits = 2)


##----first.stage.classification----
all_corpus_d<-corpus(all_docs[c("fakeid", "description", "EV_article", "classified")], text_field="description")

all_dfm_d <- preprocess_corpus(all_corpus_d, stem=TRUE, min_termfreq=20, min_docfreq = 20)

class_dfm_d<-quanteda::dfm_subset(all_dfm_d, quanteda::docvars(all_dfm_d, "classified")==1)
class_nb <- quanteda::textmodel_nb(class_dfm_d, y=quanteda::docvars(class_dfm_d, "EV_article"), prior="uniform")
first_stage_keywords<-nb_keywords(class_dfm_d, "EV_article")

##----second.stage.classification----
S_dfm <- quanteda::dfm_subset(all_dfm_d, quanteda::docvars(all_dfm_d, "classified")==0)
quanteda::docvars(S_dfm, "T")<-predict(class_nb, newdata = S_dfm, type="class")
second_stage_keywords<-nb_keywords(S_dfm, "T")


##----join.stages----
names(second_stage_keywords)<-c("keyword", "stg2_0", "stg2_1", "id")
names(first_stage_keywords)<-c("keyword", "stg1_0", "stg1_1", "id")
p_change<-left_join(first_stage_keywords, second_stage_keywords, by=c("keyword", "id")) %>%
  mutate(lgt_stg1 =log(stg1_1/stg1_0), lgt_stg2=log(stg2_1/stg2_0)) %>%
  mutate(abs_lgt_chng=abs(lgt_stg1-lgt_stg2)) %>%
  mutate(lgt_chng=lgt_stg2-lgt_stg1) %>%
  arrange(lgt_chng)

p_change%>%
  ggplot(aes(lgt_stg1, lgt_stg2))+
  geom_point() +
  ggtitle("Classifier Word-Level Classification Probability (logit transformed) in \n Stage 1 v Stage 2 Classifier")

##----classification.results----
knitr::kable(head(first_stage_keywords, 30),
             digits = 2)
knitr::kable(head(second_stage_keywords, 30),
             digits=2)
become_predictive<- p_change %>%
  filter(stg2_1>.8) %>%
  arrange(-lgt_chng)

become_less_predictive<- p_change %>%
  filter(stg1_1>.8) %>%
  arrange(lgt_chng)
knitr::kable(head(become_predictive, 30),
             digits = 2)

knitr::kable(head(become_less_predictive, 30),
             digits=2)


##----first.stage.classification.ocr----
all_corpus_o<-corpus(all_docs[c("fakeid", "ocr", "EV_article", "classified")], text_field = "ocr")
all_dfm_o <- preprocess_corpus(all_corpus_o, stem=TRUE, min_termfreq=20, min_docfreq = 20)
class_dfm_o<-quanteda::dfm_subset(all_dfm_o, quanteda::docvars(all_dfm_d, "classified")==1)
class_nb_ocr <- quanteda::textmodel_nb(class_dfm_o, y=quanteda::docvars(class_dfm_o, "EV_article"), prior="uniform")
first_stage_keywords_ocr<-nb_keywords(class_dfm_o, "EV_article")

knitr::kable(head(first_stage_keywords_ocr, 30), digits = 2)


##----secondround.1832.results----
all_searches<-get_archivesearches()
initial_1832_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1832-01-01"), archive_date_start<lubridate::ymd("1832-12-31")) %>%
  filter(search_text %in% c("election riot", "election incident", "election disturbance", "rough", "stone", "mob"))

# a number of precisely duplicated searches here which return precisely duplicated results - we could handle this later on but here probably simplest to just use three distinct ones
# election riot - id 73
# election disturbance - id 81
# election incident - id 85
# rough - id 170
# stone - id 171
# mob - id 172

res_2_1832<-get_archivesearchresults(archive_search_id = c(73, 81, 85, 170, 171, 172)) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))
res_2_1832 <- res_2_1832[!duplicated(paste(res_2_1832$search_text, res_2_1832$description)),]
is_classified <-res_2_1832$std_url %in% classdocs$std_url

unclass_2_1832 <- res_2_1832[!is_classified,] %>%
  mutate(unclass=1, classified=0)

all_docs2<- bind_rows(classdocs,
                     unclass_2_1832)
all_docs2$fakeid<-1:nrow(all_docs)

##----descriptives.1832----

res_32_des<-res_2_1832[!duplicated(paste(res_2_1832$search_text, res_2_1832$description)),] %>%
  group_by(url) %>%
  mutate(search_text=sub(" ", "_", search_text)) %>%
  dplyr::select(url, search_text) %>%
  mutate(value=1) %>%
  tidyr::spread(search_text, value, fill=0) %>%
  mutate(tot_art=nrow(.))

knitr::kable(res_32_des %>%
               group_by(election_riot, election_incident, election_disturbance, mob, rough, stone) %>%
               summarize(n=length(tot_art), prop=n/tot_art[1])%>%
               arrange(-prop), digits = 2)

all_corpus_d2<-corpus(all_docs2[c("fakeid", "description", "EV_article", "classified")], text_field="description")

all_dfm_d2 <- preprocess_corpus(all_corpus_d2, stem=TRUE, min_termfreq=20, min_docfreq = 20)

class_dfm_d2<-quanteda::dfm_subset(all_dfm_d2, quanteda::docvars(all_dfm_d2, "classified")==1)
class_nb2 <- quanteda::textmodel_nb(class_dfm_d2, y=quanteda::docvars(class_dfm_d2, "EV_article"), prior="uniform")
first_stage_keywords2<-nb_keywords(class_dfm_d2, "EV_article")

##----second.stage.classification2----
S_dfm2 <- quanteda::dfm_subset(all_dfm_d2, quanteda::docvars(all_dfm_d2, "classified")==0)
quanteda::docvars(S_dfm2, "T")<-predict(class_nb2, newdata = S_dfm2, type="class")
second_stage_keywords<-nb_keywords(S_dfm2, "T")

##----subset.on.description----
classified_boolean_returns<-classdocs[classdocs$election_article==1,]
download_these <- classifier_selection_description(classified_boolean_returns, unclass_i_1832)
do_not_download_these <- unclass_i_1832[unclass_i_1832$url %in% download_these$url,]

# the unclassified archive search results don't have ocr column
download_these_ocr<-get_candidates_fromarchivesearchresults(download_these)

##----subset.on.ocr----
code_these_ocr <- classifier_selection_ocr(classified_boolean_returns, download_these_ocr)
code_these <- unclass_i_1832[unclass_i_1832$url %in% c(code_these_ocr$url, switch_url_format(code_these_ocr$url)),]
do_not_code_these <- unclass_i_1832[!unclass_i_1832$url %in% code_these$url,]


##----print.random.not.code.cases----
not_code_random_candidate_docs<-get_random_candidates(do_not_code_these, 10)

documents_to_latex(not_code_random_candidate_docs)

##----print.random.code.cases----
code_random<-dplyr::sample_n(code_these, 10)
code_random$url_std<-sub("/viewer/download", "/viewer", download_random$url)
code_random_candidate_docs<-get_candidate_documents(cand_document_id = "all", c(code_random$url, code_random$url_std))

documents_to_latex(code_random_candidate_docs)

##----subset.on.keywords----
# here need a join between archivesearches and archivesearch
all_searches<-get_archivesearches()
initial_1832_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1832-01-01"), archive_date_start<lubridate::ymd("1832-12-31")) %>%
  filter(search_text %in% c("election riot", "election incident", "election disturbance"))

# a number of precisely duplicated searches here which return precisely duplicated results - we could handle this later on but here probably simplest to just use three distinct ones
# election riot - id 73
# election disturbance - id 81
# election incident - id 85
res_i_1832<-get_archivesearchresults(archive_search_id = c(73, 81, 85)) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

download_these_fromkeywords<-classifier_selection_keywords(classdocs[classdocs$election_article==1,], res_i_1832)
# the unclassified archive search results don't have ocr column
download_these_fromkeywords_ocr<-get_candidates_fromarchivesearchresults(download_these_fromkeywords)

##----subset.on.description.1841.data----
all_searches<-get_archivesearches()
initial_1841_searches<-all_searches%>%
  dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
  filter(archive_date_start>lubridate::ymd("1841-01-01"), archive_date_start<lubridate::ymd("1841-12-31")) %>%
  filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
                            "riot", "disturbance", "incident", "mob", "rough", "adjourn",
                            "prison", "police"))


res_i_1841<-get_archivesearchresults(archive_search_id = initial_1841_searches$id) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

select_descript_xgb<-classifier_selection_description(candocs, res_i_1841, classifier_type = "xgboost")
select_descript_nb<-classifier_selection_description(candocs, res_i_1841, classifier_type = "nb")

#select_descript_xgb$description
#select_descript_nb$description

##----subset.on.description.1841.link.cand.doc----
xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb)
xgb_cand_docs$status<-1


dim(select_descript_xgb)
select_descript_xgb %>%
  group_by(bna_id) %>%
  summarize(status=max(EV_article))
aa<-left_join(select_descript_xgb, xgb_cand_docs, by="bna_id")
#nb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_nb)

##----subset.on.ocr.after.keyword----
code_these_fromkeywords_ocr <- classifier_selection_ocr(classified_boolean_returns, download_these_fromkeywords_ocr)
code_these_fromkeywords <- unclass_i_1832[unclass_i_1832$url %in% c(code_these_fromkeywords_ocr$url, switch_url_format(code_these_fromkeywords_ocr$url)),]
do_not_code_these_fromkeywords <- unclass_i_1832[!unclass_i_1832$url %in% code_these_fromkeywords$url,]

##----subset.on.keywords.eval.four.words----
cd<-classdocs
cd<-cd[sample(nrow(cd)),] # random order cases
class_corpus<-quanteda::corpus(cd, text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance")
class_dfm_4<-quanteda::dfm(class_corpus, select=keywords)
#class_dfm<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20)

the_sets_4<-split_dfm(class_dfm_4, n_train = 1000)
classifier<-quanteda::textmodel_nb(the_sets_4$training_set, y=quanteda::docvars(the_sets_4$training_set, "EV_article"), prior="docfreq", distribution="Bernoulli")
quanteda::docvars(the_sets_4$testing_set, "predicted")<-predict(classifier, newdata = the_sets_4$testing_set, type="class")
caret::confusionMatrix(data=quanteda::docvars(the_sets_4$testing_set, "predicted"), reference=factor(quanteda::docvars(the_sets_4$testing_set, "EV_article")), mode="prec_recall", positive="1")

##----subset.on.keywords.eval.ten.words----
#cd<-classdocs[classdocs$election_article==1,]
cd<-classdocs
cd<-cd[sample(nrow(cd)),]
class_corpus<-quanteda::corpus(cd, text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
class_dfm_10<-quanteda::dfm(class_corpus, select=keywords)
#class_dfm<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20)

the_sets_10<-split_dfm(class_dfm_10, n_train = 1000)
classifier_10<-quanteda::textmodel_nb(the_sets_10$training_set, y=quanteda::docvars(the_sets_10$training_set, "EV_article"), prior="docfreq", distribution="Bernoulli")
quanteda::docvars(the_sets_10$testing_set, "predicted")<-predict(classifier_10, newdata = the_sets_10$testing_set, type="class")
caret::confusionMatrix(data=quanteda::docvars(the_sets_10$testing_set, "predicted"), reference=factor(quanteda::docvars(the_sets_10$testing_set, "EV_article")), mode="prec_recall", positive="1")

##----subset.on.keywords.eval.full.words----
cd<-classdocs[classdocs$election_article==1,]
cd<-classdocs
cd<-cd[sample(nrow(cd)),]
class_corpus<-quanteda::corpus(cd, text_field="ocr")
class_dfm_full<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20, min_docfreq = 20)

the_sets_full<-split_dfm(class_dfm_full, n_train = 1000)
classifier_full<-quanteda::textmodel_nb(the_sets_full$training_set, y=quanteda::docvars(the_sets_full$training_set, "EV_article"), prior="docfreq", distribution="Bernoulli")
quanteda::docvars(the_sets_full$testing_set, "predicted")<-predict(classifier_full, newdata = the_sets_full$testing_set, type="class")
caret::confusionMatrix(data=quanteda::docvars(the_sets_full$testing_set, "predicted"), reference=factor(quanteda::docvars(the_sets_full$testing_set, "EV_article")), mode="prec_recall", positive="1")

##----subset.on.keywords.eval.other.methods.four.words----
class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance")
class_dfm<-quanteda::dfm(class_corpus, select=keywords)
class_dfm_bin<-quanteda::dfm_weight(class_dfm, scheme="boolean")
doc_matrix<-quanteda::convert(class_dfm_bin, "tm")
training_nos<-which(1:nrow(class_dfm_bin) %in% sample(1:nrow(class_dfm_bin), 1000))
testing_nos<-which(!1:nrow(class_dfm_bin) %in% training_nos)
container <- RTextTools::create_container(doc_matrix, quanteda::docvars(class_dfm, "EV_article"), trainSize = training_nos, testSize = testing_nos, virgin=FALSE)

models_res<-RTextTools::train_models(container, algorithms=c("MAXENT", "SVM", "BOOSTING", "BAGGING", "RF", "TREE"))
results_res<-RTextTools::classify_models(container, models_res)
analytics_res<-RTextTools::create_analytics(container, results_res)

class_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("CODE")) %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)


class_summary_by_classifier<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2=="LABEL") %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)

knitr::knit_print(knitr::kable(class_summary))
knitr::knit_print(knitr::kable(class_summary_by_classifier))

prob_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("LABEL", "PROB")) %>%
  spread(var2, value) %>%
  group_by(variable, MANUAL_CODE, LABEL)



prob_summary%>%
  ggplot(aes(x=as.numeric(as.character(PROB)), fill=factor(MANUAL_CODE)))+
  facet_wrap(~variable)+
  geom_histogram()

t(analytics_res@algorithm_summary)
t(analytics_res@label_summary)
analytics_res@ensemble_summary
summary(analytics_res)


##----subset.on.keywords.eval.other.methods.ten.words----
class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
cd<-classdocs[classdocs$election_article==1,]
cd<-cd[sample(nrow(cd)),]
#class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
class_corpus<-quanteda::corpus(cd, text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
class_dfm<-quanteda::dfm(class_corpus, select=keywords)
class_dfm_bin<-quanteda::dfm_weight(class_dfm, scheme="boolean")
doc_matrix<-quanteda::convert(class_dfm_bin, "tm")
training_nos<-which(1:nrow(class_dfm_bin) %in% sample(1:nrow(class_dfm_bin), 1000))
testing_nos<-which(!1:nrow(class_dfm_bin) %in% training_nos)
container <- RTextTools::create_container(doc_matrix, quanteda::docvars(class_dfm_bin, "EV_article"), trainSize = training_nos, testSize = testing_nos, virgin=FALSE)

models_res<-RTextTools::train_models(container, algorithms=c("MAXENT", "SVM", "BOOSTING", "BAGGING", "RF", "TREE"))
results_res<-RTextTools::classify_models(container, models_res)
analytics_res<-RTextTools::create_analytics(container, results_res)

class_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("CODE")) %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)


class_summary_by_classifier<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2=="LABEL") %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)

knitr::knit_print(knitr::kable(class_summary))
knitr::knit_print(knitr::kable(class_summary_by_classifier))

prob_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("LABEL", "PROB")) %>%
  spread(var2, value) %>%
  group_by(variable, MANUAL_CODE, LABEL)



prob_summary%>%
  ggplot(aes(x=as.numeric(as.character(PROB)), fill=factor(MANUAL_CODE)))+
  facet_wrap(~variable)+
  geom_histogram()

t(analytics_res@algorithm_summary)
t(analytics_res@label_summary)
analytics_res@ensemble_summary
summary(analytics_res)


##----print.random.not.code.fromkeywords.cases----
not_code_fromkeywords_random_candidate_docs<-get_random_candidates(do_not_code_these_fromkeywords, 10)

documents_to_latex(not_code_fromkeywords_random_candidate_docs)


##----subset.on.keywords.eval.other.methods.ten.words.loop----
cd<-classdocs[classdocs$election_article==1,]
cd<-cd[sample(nrow(cd)),]
#class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
class_corpus<-quanteda::corpus(cd, text_field="ocr")
#class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="description")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
#keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window")
#class_dfm<-preprocess_corpus(class_corpus, min_termfreq=50, min_docfreq = 50)
class_dfm<-quanteda::dfm(class_corpus, select=keywords)

class_dfm_bin<-quanteda::dfm_weight(class_dfm, scheme="boolean")
doc_matrix<-quanteda::convert(class_dfm_bin, "tm")
#doc_matrix<-quanteda::convert(class_dfm, "tm")

n_sims<-3
results_label_summary<-vector("list", n_sims)
results_ensemble<-vector("list", n_sims)
for (sim in 1:n_sims){
  training_nos<-which(1:nrow(class_dfm_bin) %in% sample(1:nrow(class_dfm_bin), 1000))
  testing_nos<-which(!1:nrow(class_dfm_bin) %in% training_nos)
  #training_nos<-1:999
  #testing_nos<-1000:nrow(class_dfm_bin)
  container <- RTextTools::create_container(doc_matrix, quanteda::docvars(class_dfm_bin, "EV_article"), trainSize = training_nos, testSize = testing_nos, virgin=FALSE)

  models_res<-RTextTools::train_models(container, algorithms=c("MAXENT", "SVM", "BOOSTING", "BAGGING", "RF", "TREE"))
  results_res<-RTextTools::classify_models(container, models_res)
  analytics_res<-RTextTools::create_analytics(container, results_res)

  results_label_summary[[sim]]<-tibble::rownames_to_column(data.frame(t(analytics_res@label_summary), sim=sim))
  results_ensemble[[sim]]<-analytics_res@ensemble_summary


}

do.call("rbind", results_label_summary) %>%
  group_by(rowname) %>%
  summarize(mean(X0), mean(X1))
class_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("CODE")) %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)
class_summary

class_summary_by_classifier<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2=="LABEL") %>%
  unite(value, var2, value) %>%
  group_by(variable, MANUAL_CODE, value) %>%
  tally() %>%
  spread(value, n)
class_summary_by_classifier
knitr::knit_print(knitr::kable(class_summary))
knitr::knit_print(knitr::kable(class_summary_by_classifier))

prob_summary<-analytics_res@document_summary %>%
  tibble::rowid_to_column("document_id") %>%
  tidyr::gather(variable, value, -MANUAL_CODE, -document_id) %>%
  separate(variable, c("variable", "var2")) %>%
  filter(var2 %in% c("LABEL", "PROB")) %>%
  spread(var2, value) %>%
  group_by(variable, MANUAL_CODE, LABEL)



prob_summary%>%
  ggplot(aes(x=as.numeric(as.character(PROB)), fill=factor(MANUAL_CODE)))+
  facet_wrap(~variable)+
  geom_histogram()

t(analytics_res@algorithm_summary)
t(analytics_res@label_summary)
analytics_res@ensemble_summary
summary(analytics_res)


##----bin----
class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
class_dfm<-quanteda::dfm(class_corpus, select=keywords)
#class_dfm<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20)

the_sets<-split_dfm(class_dfm, n_train = 1000)
classifier<-quanteda::textmodel_nb(the_sets$training_set, y=quanteda::docvars(the_sets$training_set, "EV_article"), prior="uniform", distribution="Bernoulli")
quanteda::docvars(the_sets$testing_set, "predicted")<-predict(classifier, newdata = the_sets$testing_set, type="class")==class_to_keep
aa<-nb_keywords(the_sets$training_set, "EV_article")
table(quanteda::docvars(the_sets$testing_set, c("predicted", "EV_article")))
chisq.test(table(quanteda::docvars(the_sets$testing_set, c("predicted", "EV_article"))))


##----tree----
library(rpart)

class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
class_dfm<-quanteda::dfm(class_corpus, select=keywords)
the_sets<-split_dfm(class_dfm, n_train = 1000)
train_df<-convert(dfm_weight(the_sets$training_set, scheme="boolean"), "data.frame")
train_df$EV_article<-docvars(the_sets$training_set, "EV_article")
cols<-(names(train_df))
#train_df<-train_df %>%
#  mutate_at(cols, factor)
test_df<-convert(dfm_weight(the_sets$testing_set, scheme="boolean"), "data.frame")
test_df$EV_article<-docvars(the_sets$testing_set, "EV_article")
#test_df<-test_df %>%
#  mutate_at(cols, factor)


fit2<-rpart(EV_article~election+disturbance+riot+incident, train_df)
fit2<-rpart(EV_article~election+candidate+party+disturbance+riot+incident+window+stone+hustings+mob+magistrate,
            data=train_df,
            method="class",
            control=rpart.control(minsplit = 0, cp=.005)
            )
fancyRpartPlot(fit2)

test_df$predict<-predict(fit2, test_df, type="class")

table(test_df$EV_article, test_df$predict)

fit_rf<-randomForest(as.factor(EV_article)~document+election+candidate+party+disturbance+riot+incident+window+stone+hustings+mob+magistrate,
                  data=train_df,
                  method="class",
                  ntree=100)
test_df$predict<-predict(fit_rf, test_df)

table(test_df$EV_article, test_df$predict)

library(xgboost)

params <- list(booster = "gbtree",
               objective = "binary:logistic",
               eta=.5,
               gamma=0,
               max_depth=6,
               min_child_weight=1,
               subsample=1,
               colsample_bytree=1)

class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
class_corpus<-quanteda::corpus(classdocs, text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
#keywords<-c("election", "riot", "incident", "disturbance")
class_dfm<-quanteda::dfm(class_corpus, select=keywords)
class_dfm<-preprocess_corpus(class_corpus, min_termfreq = 20, min_docfreq = 10, stem=FALSE)
the_sets<-split_dfm(class_dfm, n_train = 1000)
#train_df<-convert(dfm_weight(the_sets$training_set, scheme="boolean"), "data.frame")
#train_labels<-docvars(the_sets$training_set, "election_article")
#cols<-(names(train_df))
#train_df<-train_df %>%
#  mutate_at(cols, factor)
#test_df<-convert(dfm_weight(the_sets$testing_set, scheme="boolean"), "data.frame")
#test_labels<-docvars(the_sets$testing_set, "election_article")
#test_df<-test_df %>%
#  mutate_at(cols, factor)
new_train<-as(the_sets$training_set, "dgCMatrix")
new_train<-as(dfm_weight(the_sets$training_set, scheme="boolean"), "dgCMatrix")
train_labels<-docvars(the_sets$training_set, "EV_article")
new_test<-as(the_sets$testing_set, "dgCMatrix")
new_test<-as(dfm_weight(the_sets$testing_set, scheme="boolean"), "dgCMatrix")
test_labels<-docvars(the_sets$testing_set, "EV_article")
#new_train<-model.matrix(~.+0, setDT(train_df)[,-"document"])
#new_test<-model.matrix(~.+0, setDT(test_df)[,-"document"])
dtrain<-xgb.DMatrix(data=new_train, label=train_labels)
dtest<-xgb.DMatrix(data=new_test, label=test_labels)

params <- list(booster = "gbtree",
               objective = "binary:logistic",
               eta=.03,
               gamma=0,
               max_depth=6,
               min_child_weight=1,
               subsample=.8,
               colsample_bytree=1)
xgbvc<-xgb.train(param=params, data=dtrain, nrounds=100, print_every_n = 10, early_stopping_rounds = 10, maximize = F, eval_metric="error", watchlist=list(val=dtest, train=dtrain), verbose = 1)

prob_xgb<-predict(xgbvc, dtest)
pred_xgb<-factor(ifelse(predict(xgbvc, dtest)>.5, 1, 0))
#hist(predict(xgbvc, dtest), col=pred_xgb==test_labels)
#hist(prob_xgb[pred_xgb!=test_labels])
#hist(prob_xgb[pred_xgb==test_labels])
caret::confusionMatrix(pred_xgb, factor(test_labels), positive="1", mode="prec_recall")
top.import<-xgb.importance(feature_names=colnames(new_train), model=xgbvc)[1:30]
xgb.plot.importance(top.import)

#caret::confusionMatrix(pred_xgb[prob_xgb<.2|prob_xgb>.8], factor(test_labels)[prob_xgb<.2|prob_xgb>.8], positive="1", mode="prec_recall")
#caret::confusionMatrix(pred_xgb[!(prob_xgb<.2|prob_xgb>.8)], factor(test_labels)[!(prob_xgb<.2|prob_xgb>.8)], positive="1", mode="prec_recall")

aa<-classifier_select_docs(xgbvc, classdocs, text_field = "ocr", xgb.cutpoint = .5, stem=FALSE)
sum(aa$EV_article==1)
sum(classdocs$EV_article==1)
dim(classdocs)
dim(aa)
table(aa$EV_article)


all_searches<-get_archivesearches()
res_oneday_1832<-get_archivesearchresults(archive_search_id = c(228:255)) %>%
  left_join(all_searches, by=c("archive_search_id"="id")) %>%
  mutate(std_url = sub("download/", "", url))

download_these_fromkeywords<-classifier_selection_keywords(classdocs, res_oneday_1832)
# the unclassified archive search results don't have ocr column
download_these_fromkeywords_ocr<-get_candidates_fromarchivesearchresults(download_these_fromkeywords)

select_nb<-classifier_selection_keywords(classdocs, res_oneday_1832, classifier_type = "nb")
select_xgboost<-classifier_selection_keywords(classdocs, res_oneday_1832, classifier_type = "xgboost")
dim(select_xgboost)
dim(select_nb)
oneday_dfm<-searches_to_dfm(res_oneday_1832)
reject_xgboost<-oneday_dfm[setdiff(quanteda::docnames(oneday_dfm), quanteda::docnames(select_xgboost))]
reject_nb<-oneday_dfm[setdiff(quanteda::docnames(oneday_dfm), quanteda::docnames(select_nb))]

reject_onlyxgboost<-reject_xgboost[setdiff(quanteda::docnames(reject_xgboost), quanteda::docnames(reject_nb))]
reject_onlynb<-reject_nb[setdiff(quanteda::docnames(reject_nb), quanteda::docnames(reject_xgboost))]
select_xgboost2<-classifier_selection_keywords(classdocs, res_oneday_1832, classifier_type = "xgboost")

sum(docnames(select_xgboost) %in% docnames(select_xgboost2))
sum(!docnames(select_xgboost) %in% docnames(select_xgboost2))

sum(docnames(select_xgboost2) %in% docnames(select_xgboost))
sum(!docnames(select_xgboost2) %in% docnames(select_xgboost))

bin_train<-dfm_weight(train_dfm, scheme="boolean")
nfeatures<-rowSums(bin_train)
that_pattern<-rowSums(dfm_select(bin_train, c("election", "candidate", "poll")))==3
that_pattern<-rowSums(dfm_select(bin_train, c("mob")))==1 & docvars(bin_train, "EV_article")==0
that_pattern==nfeatures
docvars(bin_train[that_pattern,], "EV_article")
bin_train[that_pattern,]

aa<-inner_join(select_nb, select_xgboost)

train <- classdocs %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(classdocs, train, by = 'id')

aa<-classifier_selection_keywords(train, test, mode="eval", classifier_type = "xgboost")

caret::confusionMatrix(factor(aa$EV_article), factor(as.numeric(aa$selected)))


candocs<-get_candidate_documents("all")
actdocs<-get_document("all")
candocs$status2<-candocs$status
candocs$status2[(candocs$id %in% actdocs$candidate_document_id)]<-"1"
summary(factor(candocs$status2[(candocs$id %in% actdocs$candidate_document_id)]))
candocs$EV_article<-ifelse(candocs$status2 %in% c("1", "3"), 1, 0)
candocs.r<-candocs[candocs$status2 %in% c("1", "3", "7", "8", "0"),]
nb_descript<-classifier_selection_description(candocs.r, res_oneday_1832, classifier_type = "nb", min_docfreq=2, min_termfreq=2)
xg_descript<-classifier_selection_description(candocs.r, res_oneday_1832, classifier_type = "xgboost", min_docfreq=2, min_termfreq=2)
xg_descript$description
nb_descript$description

bd<-inner_join(xg_descript, nb_descript)
aa$url
canddocs_xgb<-get_candidates_fromarchivesearchresults(xg_descript)

dim(download_these_fromkeywords_ocr)
paste(download_these_fromkeywords_ocr[, c("status", "description")], sep=" *** ")

download_these_fromkeywords_ocr %>%
  arrange(status) %>%
  unite(description2, status, description) %>%
  select(description2)
table(download_these_fromkeywords_ocr$status)
