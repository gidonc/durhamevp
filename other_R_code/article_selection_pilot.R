##----pilot.packages----
library(reportRx) # note using this just for sanitizing LaTex output, not included in durhamevp package requirements
library(tidyverse)
library(quanteda)
library(durhamevp)
library(RTextTools) # not sure if this is included as part of the durhamevp package


##----selection.pilot.custom.functions----
documents_to_latex<-function(out_docs){
  for (doc in 1:nrow(out_docs)){
    cat(paste0("\\subsection{", reportRx::sanitizestr(out_docs[doc, "title"]), " (candidate\\_document\\_id: ", out_docs[doc, "id"], ")}"))
    cat(paste0("  \n"))
    cat(paste0("\\subsubsection{description}"))
    cat(paste0("  \n"))
    #cat(Hmisc::latexTranslate(out_docs[doc, "description"]))
    #knitr::knit_print(out_docs[doc, "description"])
    cat(reportRx::sanitizestr(out_docs[doc, "description"]))
    cat(paste0("\\subsubsection{OCR}"))
    cat(paste0("  \n"))
    #knitr::knit_print(out_docs[doc, "ocr"])
    cat(reportRx::sanitizestr(out_docs[doc, "ocr"]))
    cat("  \n")
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
##----initial.keywords----
# election riot, election incident, election disturbance

##----download.R.set----
# In fact we have some classified docs non-EV cases here King's R set doesn't contain any non-cases
# I think we should use our more extensive information here

classdocs<-durhamevp::get_classified_docs()

classdocs<-classdocs %>%
  dplyr::mutate(std_url = sub("download/", "", url)) %>%
  dplyr::mutate(unclass=0, classified=1)

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


##----subset.on.ocr.after.keyword----
code_these_fromkeywords_ocr <- classifier_selection_ocr(classified_boolean_returns, download_these_fromkeywords_ocr)
code_these_fromkeywords <- unclass_i_1832[unclass_i_1832$url %in% c(code_these_fromkeywords_ocr$url, switch_url_format(code_these_fromkeywords_ocr$url)),]
do_not_code_these_fromkeywords <- unclass_i_1832[!unclass_i_1832$url %in% code_these_fromkeywords$url,]

##----subset.on.keywords.eval.four.words----
class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance")
class_dfm_4<-quanteda::dfm(class_corpus, select=keywords)
#class_dfm<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20)

the_sets_4<-split_dfm(class_dfm_4, n_train = 1000)
classifier<-quanteda::textmodel_nb(the_sets_4$training_set, y=quanteda::docvars(the_sets_4$training_set, "EV_article"), prior="docfreq", distribution="Bernoulli")
quanteda::docvars(the_sets_4$testing_set, "predicted")<-predict(classifier, newdata = the_sets_4$testing_set, type="class")
caret::confusionMatrix(data=quanteda::docvars(the_sets_4$testing_set, "predicted"), reference=factor(quanteda::docvars(the_sets_4$testing_set, "EV_article")), mode="prec_recall", positive="1")

##----subset.on.keywords.eval.ten.words----
class_corpus<-quanteda::corpus(classdocs[classdocs$election_article==1,], text_field="ocr")
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
class_dfm_10<-quanteda::dfm(class_corpus, select=keywords)
#class_dfm<-preprocess_corpus(class_corpus, stem=FALSE, min_termfreq = 20)

the_sets_10<-split_dfm(class_dfm_10, n_train = 1000)
classifier_10<-quanteda::textmodel_nb(the_sets_10$training_set, y=quanteda::docvars(the_sets_10$training_set, "EV_article"), prior="docfreq", distribution="Bernoulli")
quanteda::docvars(the_sets_10$testing_set, "predicted")<-predict(classifier_10, newdata = the_sets_10$testing_set, type="class")
caret::confusionMatrix(data=quanteda::docvars(the_sets_10$testing_set, "predicted"), reference=factor(quanteda::docvars(the_sets_10$testing_set, "EV_article")), mode="prec_recall", positive="1")

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
keywords<-c("election", "riot", "incident", "disturbance", "mob", "stone", "window", "candidate", "party", "hustings", "magistrate")
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


##----print.random.not.code.fromkeywords.cases----
not_code_fromkeywords_random_candidate_docs<-get_random_candidates(do_not_code_these_fromkeywords, 10)

documents_to_latex(not_code_fromkeywords_random_candidate_docs)

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

