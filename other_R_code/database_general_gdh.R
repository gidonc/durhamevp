# Initial Setup (DEFUNCT)
install.packages('data.table')
install.packages('knitr')
install.packages('miniUI')
install.packages('shiny')
install.packages("pool")
install.packages("magrittr")
install.packages("dplyr")
devtools::install_github("rstudio/pool")

# Redownload durhamevp
devtools::install_github("gidonc/durhamevp")

# Initial Setup (ACTIVE)
#keyring::key_set("evp", username="data_writer")
library(magrittr)
library(shiny)
library(pool)
library(tidyverse)
library(dplyr)
library(stringr)
library(durhamevp)
con1 <- evdb_connect(password_method = "keyring") # connect to database and save connection as 'con'
con1
.evp_db_pool <- NULL


# Get user details for user X
get_user(10)
userinfo<-get_user(10)
View(userinfo)


# To assign article id X to user Y
assign_article_to_user(688, 7, allocation_type = "training", allocated_by="testing assignment")


# View individual user allocations (doc numbers only)
user_allocated_fromset(9, set = define_testset(), not_allocated = FALSE)
user7allocations <- user_allocated_fromset(9, set = define_testset(), not_allocated = FALSE)
View(user7allocations)


# reassign userdoc allocations
reallocate_randomly(c(24, 44, 19, 22, 36, 39, 47, 48), 8258,
                    allocated_by = "random_reassignment", restrict_to_actual = TRUE,
                    make_assignments = TRUE, force = FALSE)


usergroupresults2 <- get_allocation("all", allocation_type = "coding", document_id = 5811:5850)
View(usergroupresults2)


# View individual user allocations (with scores)
usergroupresults <- get_allocation(26, allocation_type = "coding", document_id = "all")
View(usergroupresults %>% filter(status == "NEW"))
View(usergroupresults %>% filter(status == "COMPLETED"))
View(usergroupresults %>% filter(status == "SAVED"))
View(usergroupresults)


usergroupresults <- get_allocation(8:47, allocation_type = "coding", document_id = "all")
View(usergroupresults %>% filter(status == "NEW", allocated_by == "random_reassignment"))


# View Individual Event Reports
usereventreports <- get_event_report("all")
usereventreports2 <- usereventreports %>%
  filter(str_detect(summary, "Newcastle"))
View(usereventreports2)
View(usereventreports)


# View xgboost results
usergroupresults <- get_allocation("all", allocation_type = "coding", document_id = 4137:4468)
View(usergroupresults %>% filter(status == "COMPLETED"))

View(usergroupresults %>% filter(status == "NEW"))
View(usergroupresults)


# View Individual User Metadata
get_user(7)
user7 <- get_user(7)
View(user7)


# Switch coders from training-testing, testing-coding, coding-checking
set_user_mode (28,  new_mode = "training")


#Assign test set to user(s)
assign_testset_to_user(48, allocation_type = "testing",
                       allocated_by = "assign_testset_to_user")

#Assign training set to user(s)
assign_trainingset_to_user(48, allocation_type = "training",
                           allocated_by = "assign_testset_to_user")

# Give users individual articles
assign_article_to_user(19:47, allocation_type = "training", allocated_by = "GaryH", allocation_date = as.character(Sys.Date()), status = "NEW", coding_complete = 0)


# To see Gary's codings of all X's codings
jb_coding<-get_allocation(8, allocation_type="testing")
jb_doc_ids<-jb_coding$document_id
GaryVsX<-get_allocation(6, document_id = jb_doc_ids)
View(GaryVsX)


## To see comparison coding ids for test set results
View(GaryVsX[,c("id", "document_id")])


# See general times spent by coders
activity<-DBI::dbGetQuery(con, 'select *
                          from portal_useractivitylogentry')
activity %>%
  #group_by(user_id) %>%
  filter(user_id>7) %>%
  group_by(user_id, allocation_id) %>%
  summarize(start=min(date), end=max(date), time_take=difftime(end, start)) %>%
  ggplot(aes(time_take))+
  facet_wrap(~user_id)+
  geom_histogram()


# View Individual Documents
get_document(819)
indiv_doc <- get_document(819)
View(indiv_doc)


#View Individual Candidate Documents
candidate_docs<-get_candidate_documents(cand_document_id = "8135")
View(candidate_docs)


#View Individual Candidate Documents WITH STATUS
candidate_docs<-get_candidate_documents(cand_document_id = 1000:15000)
filter(candidate_docs, status == 10)
filter(id)
View(candidate_docs)


# How to see archive searches, various ways of pruning/combining results
get_archivesearches(703)
get_archivesearches("all")
aa<-get_archivesearches(675:687)
View(aa)
bb<-get_archivesearchresults(1:100)
View(bb)
View(head(bb))
cc<-left_join(bb, aa, by=c("archive_search_id"="id"))
View(cc)
View(cc[cc$archive_search_id==71,])
cc[5, "url"]
duplicated(cc$url)
duplicated(cc$url)


# See double-coded articles, allocation numbers
ee<-get_allocation("all")
ff<-get_event_report("all")
gg<- left_join(ee, ff)
View(gg)
hh<-gg  %>%
filter(status=="COMPLETED") %>%
  filter(allocation_type=="coding") %>%
  filter(geo_relevant=="true") %>%
  filter(time_relevant=="true") %>%
  filter(electoral_nature=="true") %>%
  filter(electoralviolence_nature=="true") %>%
  filter(violence_nature=="true") %>%
  filter(legibility=="true")
ii<-hh[hh$document_id %in% hh$document_id[duplicated(hh$document_id)],] %>%
  filter(str_detect(summary, "Durham"))
View(hh)

View(ii)

jj <- get_attribute("all")
View(jj)

kk <- get_tag("all")
View(kk)

# Sandbox

Attribute <- get_attribute("all")
View(Attribute)


Tag <- get_tag("all")
View(Tag)

allocations <- get_allocation
is_complete <- allocations %>%
  filter(status=="COMPLETED", user_id==7)
View(allocations)
with_score <- allocations %>%
  filter(score!=-1)
docs<-get_document(document_id="all")
View(docs)


eventreports <- get_event_report("all")
eventreports2 <- eventreports %>%
  filter(str_detect(event_start, "/10"))
View(eventreports2[c ("event_timeframe_quantifier", "event_start", "event_end", "summary")])

View(eventreports)




articlesleft <- get_allocation(8:48, allocation_type = "coding", document_id = "all")
articlesleft2 <- articlesleft %>%
  filter(status=="NEW") %>%
  group_by(user_id)
View(articlesleft2)

View(articlesleft2[,c("user_id")])



View(articlesleft2[,c("id", "document_id")])


View(usergroupresults %>%
       group_by(user_id, allocation_type, status)%>%
       tally(mean(score)))


activity %>%
  #group_by(user_id) %>%
  filter(user_id>7) %>%
  group_by(user_id, allocation_id) %>%
  summarize(start=min(date), end=max(date), time_take=difftime(end, start)) %>%
  ggplot(aes(time_take))+
  facet_wrap(~user_id)+
  geom_histogram()
