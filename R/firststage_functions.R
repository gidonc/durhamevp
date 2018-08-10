#This code contains the key functions for the first stage

create_docsets<-function(alldocs, size, seed){
  #' Create training and test set document matrices
  #' @param alldocs The document data.frame to be split
  #' @param size Size of the training set (must be smaller than number of documents in the data.frame)
  #' @param seed Seed for randomization to ensure replicability
  #' @export
  set.seed(seed)
  id_train<-sample(1:nrow(alldocs), size, replace=FALSE)
  training<-subset(alldocs, fakeid %in% id_train)
  testing<-subset(alldocs, !fakeid %in% id_train)
  return(list(training=training, testing=testing))
}

preprocess_corpus<-function(the_corpus, stem=TRUE, min_termfreq=20, min_docfreq=20, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @param stem default TRUE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 20
  #' @param min_docfreq default 20
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @export
  the_dfm <- quanteda::dfm(the_corpus, stem=stem, remove=quanteda::stopwords("english"), remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens)
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

preprocess_sgrams<-function(the_corpus, wseq, stem=TRUE, min_termfreq=2, min_docfreq=2, max_termfreq=NULL, max_docfreq=NULL,
                            remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                            dfm_tfidf=FALSE){
  #' Preprocess a text corpus including the creation of n-grams for specific words and return a document feature matrix (wrapper round quanteda functions).
  #' @param the_corpus The text corpus to be pre-processed.
  #' @param wseq Pre-specified word sequence as list on which n-grams idenfied and added to the dfm
  #' @param stem default TRUE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 2
  #' @param min_docfreq default 2
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @export
  toks<-quanteda::tokens(the_corpus,remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
                         remove_separators = TRUE, remove_hyphens = TRUE)
  gramtoks<-quanteda::tokens_compound(toks,quanteda::phrase(wseq))
  the_dfm <- quanteda::dfm(gramtoks, stem=stem, remove=quanteda::stopwords("english"))
  the_dfm <- quanteda::dfm_trim(the_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)
  if(dfm_tfidf){
    the_dfm<-quanteda::dfm_tfidf(the_dfm)
  }
  the_dfm
}

nb_keywords<-function(training, classvar, distribution="Bernoulli"){
  #' Train a naive bayes classifier and extract keywords as data.frame object ordered by most predictive word of class on top (wrapper function around Quanteda commands). The returned data.frame has three columns: word, 0(=posterior prob of word indicating that document in classvar==0), 1(=posterior prob of word predicting word indicating that document in classvar==1), and id.
  #' @param training Training data feature matrix
  #' @param classvar Classification variable name, entered as string
  #' @export

  nb<-quanteda::textmodel_nb(training,quanteda::docvars(training, classvar), distribution=distribution)
  post<-as.data.frame(t(nb$PcGw))
  post<-tibble::rownames_to_column(post)
  post$id<-seq.int(nrow(post))
  post<-post[order(post[,3], decreasing=TRUE),]
  return(post)
}

run_firststage_nbchng<-function(docs, docidvar="fakeid", classvar="classified", typevar="EV_article", textvar="description",
                         stem=TRUE, min_termfreq=20, min_docfreq=20, max_termfreq=NULL, max_docfreq=NULL,
                         remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                         dfm_tfidf=FALSE,
                         cpoint2=0.8, cpointchng=0.0){
  #' First stage function of the article selection process taking in a set of documents and returning a set of potential keywords
  #' @param docs Data frame of documents containing classified cases (R-set) and unclassified cases (S-set)
  #' @param docidvar Unique document id variable; default = "fakeid"
  #' @param classvar Indicator identifying classified documents; default = "classified"
  #' @param typevar Indicator identifying election violence articles; default = "EV_article"
  #' @param textvar Indicator identifying text field to classify on; default = "description"
  #' @param stem default TRUE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 20
  #' @param min_docfreq default 20
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @param cpoint2 Cutpoint on predicability of election violence in step 2; default = 0.8
  #' @param cpointchng Cutpoint on change in log predictability between step 1 and 2; default =0.0
  #' @export
  #Creating corpus and dfm
  full_corpus<-quanteda::corpus(docs[c(docidvar, classvar, typevar, textvar)], text_field=textvar)
  full_dfm <- durhamevp::preprocess_corpus(full_corpus, stem=stem, remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens,
                                           min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)

  #Training naive Bayes classifier on classified subset of documents and extract keywords from this stage
  class_dfm<-quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, classvar)==1)
  class_nb <- quanteda::textmodel_nb(class_dfm, y=quanteda::docvars(class_dfm, typevar), prior="uniform")
  keywords1<-durhamevp::nb_keywords(class_dfm, typevar)

  #Use trained classifier to predict election violence article from unclassified documents and extract keywords
  S_dfm <- quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, classvar)==0)
  quanteda::docvars(S_dfm, "T")<-predict(class_nb, newdata = S_dfm, type="class")
  keywords2<-nb_keywords(S_dfm, "T")

  #Combine keywords from classified and unclassified sets and identify those with largest change in predictive power between classified and unclassified documents
  names(keywords1)<-c("keyword", "stg1_0", "stg1_1", "id")
  names(keywords2)<-c("keyword", "stg2_0", "stg2_1", "id")
  change<-left_join(keywords1, keywords2, by=c("keyword", "id")) %>%
    mutate(lgt_stg1 =log(stg1_1/stg1_0), lgt_stg2=log(stg2_1/stg2_0)) %>%
    mutate(abs_lgt_chng=abs(lgt_stg1-lgt_stg2)) %>%
    mutate(lgt_chng=lgt_stg2-lgt_stg1)

  #Return ordered keyword suggestions from most to least difference in predictiveness
  return(change %>%
    filter(stg2_1>=cpoint2) %>%
    filter(lgt_chng>=cpointchng) %>%
    arrange(-lgt_chng))
}

run_firststage_fcm<-function(docs, docidvar="fakeid", classvar="classified", typevar="EV_article", textvar="description",
                         stem=TRUE, min_termfreq=20, min_docfreq=20, max_termfreq=NULL, max_docfreq=NULL,
                         remove_punct=TRUE, remove_numbers=TRUE, remove_hyphens=TRUE, termfreq_type="count", docfreq_type="count",
                         dfm_tfidf=FALSE,
                         initialkw=c("elect", "riot", "disturb", "incid"), cpoint2=0.9){
  #' First stage function of the article selection process taking in a set of documents and returning a set of potential keywords
  #' @param docs Data frame of documents containing classified cases (R-set) and unclassified cases (S-set)
  #' @param docidvar Unique document id variable; default = "fakeid"
  #' @param classvar Indicator identifying classified documents; default = "classified"
  #' @param typevar Indicator identifying election violence articles; default = "EV_article"
  #' @param textvar Indicator identifying text field to classify on; default = "description"
  #' @param stem default FALSE
  #' @param remove_punct default TRUE
  #' @param remove_numbers default TRUE
  #' @param remove_hyphens default TRUE
  #' @param min_termfreq default 20
  #' @param min_docfreq default 20
  #' @param max_termfreq default NULL
  #' @param max_docfreq default NULL
  #' @param termfreq_type default "count"
  #' @param docfreq_type default "count"
  #' @param dfm_tfidf default FALSE
  #' @param initialkw Initial keywords used to retrive classified documents; default = c("elect", "riot", "disturb", "incid")
  #' @param cpoint2 Cutpoint on predicability of keyword in step 2; default = 0.9
  #' @export
  #Creating corpus and dfm
  full_corpus<-quanteda::corpus(docs[c(docidvar, classvar, typevar, textvar)], text_field=textvar)
  full_dfm <- durhamevp::preprocess_corpus(full_corpus, stem=stem, remove_punct=remove_punct, remove_numbers=remove_numbers, remove_hyphens=remove_hyphens,
                                           min_termfreq=min_termfreq, min_docfreq = min_docfreq, termfreq_type=termfreq_type, docfreq_type=docfreq_type)

  #Training naive Bayes classifier on classified subset of documents and extract keywords from this stage
  class_dfm<-quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, classvar)==1)
  class_nb <- quanteda::textmodel_nb(class_dfm, y=quanteda::docvars(class_dfm, typevar), prior="uniform")

  #Use trained classifier to predict election violence article from unclassified documents and extract keywords
  S_dfm <- quanteda::dfm_subset(full_dfm, quanteda::docvars(full_dfm, classvar)==0)
  quanteda::docvars(S_dfm, "T")<-predict(class_nb, newdata = S_dfm, type="class")
  keywords2<-nb_keywords(S_dfm, "T")

  #Combine top keywords from both steps
  topkw2<-subset(keywords2[,1],keywords2[,3]>=cpoint2)
  full_fcm<-fcm(full_dfm)
  pred_fcm<-fcm_select(full_fcm, pattern=c(initialkw,topkw2), selection="keep", valuetyp="fixed")
  kw_fcm<-quanteda::convert(pred_fcm, to="matrix")
  kw_fcm<-kw_fcm[rownames(kw_fcm)%in%initialkw,]
  kw_fcm<-kw_fcm[,colnames(kw_fcm)%in%topkw2]

  kw_fcm<-sort(apply(kw_fcm, 2,sum))
  kw_fcm<-as.data.frame(kw_fcm)
  kw_fcm$rel_freq<-round(kw_fcm/length(docs[,classvar]==0),3)
  colnames(kw_fcm)[1] <- "abs_freq"
  colnames(kw_fcm)[2] <- "rel_freq"
  return(kw_fcm)
}

