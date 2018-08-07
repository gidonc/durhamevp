##----pilot.packages----

library(durhamevp)
library(tidyverse)
library(quanteda)

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