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
head(classdocs)
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

##----first.stage.classification----
all_corpus_d<-corpus(all_docs[c("fakeid", "description", "EV_article", "classified")], text_field="description")
all_corpus_o<-corpus(all_docs[c("fakeid", "ocr", "EV_article", "classified")], text_field = "ocr")

all_dfm_d <- preprocess_corpus(all_corpus_d, stem=TRUE, min_termfreq=20, min_docfreq = 20)
all_dfm_o <- preprocess_corpus(all_corpus_o, stem=TRUE, min_termfreq=20, min_docfreq = 20)

class_dfm_d<-quanteda::dfm_subset(all_dfm_d, quanteda::docvars(all_dfm_d, "classified")==1)
class_dfm_o<-quanteda::dfm_subset(all_dfm_o, quanteda::docvars(all_dfm_d, "classified")==1)
class_nb <- quanteda::textmodel_nb(class_dfm_d, y=quanteda::docvars(class_dfm_d, "EV_article"), prior="uniform")
keywords<-nb_keywords(class_dfm_d, "EV_article")
head(keywords, 20)
tail(keywords, 20)

##----second.stage.classification----
S_dfm <- quanteda::dfm_subset(all_dfm_d, quanteda::docvars(all_dfm_d, "classified")==0)
quanteda::docvars(S_dfm, "T")<-predict(class_nb, newdata = S_dfm, type="class")

king_stage2 <- quanteda::textmodel_nb(S_dfm, y=quanteda::docvars(S_dfm, "T"), distribution="Bernoulli")
king_stage2 <- nb_keywords(S_dfm, "T")
knitr::kable(head(king_stage2, 30))

names(king_stage2)<-c("rowname", "stage2_0", "stage2_1", "stage2_id")
names(keywords)<-c("rowname", "stage1_0", "stage1_1", "stage1_id")
p_change<-left_join(king_stage2, keywords, by="rowname") %>%
  mutate(logit_stage1 =log(stage1_1/stage1_0), logit_stage2=log(stage2_1/stage2_0)) %>%
  mutate(abs_logit_change=abs(logit_stage1-logit_stage2)) %>%
  mutate(logit_change=logit_stage2-logit_stage1) %>%
  arrange(logit_change)

p_change%>%
  ggplot(aes(logit_stage1, logit_stage2))+
  geom_point()

##----classification.results----
head(keywords, 30)
head(king_stage2, 30)
become_predictive<- p_change %>%
  filter(stage2_1>.8) %>%
  arrange(-logit_change)

become_less_predictive<- p_change %>%
  filter(stage1_1>.8) %>%
  arrange(logit_change)
head(become_predictive, 30)
head(become_less_predictive, 30)


#


##----First.stage.classifier----


##----Second.stage.classifier----
