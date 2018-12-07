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

## ---- eval=FALSE---------------------------------------------------------
#  candocs<-get_candidate_documents(status =c("0","1", "2", "4", "5", "6", "7", "8"), include_ocr=FALSE)
#  
#  candocs$EV_article<-ifelse(candocs$status %in% c("1", "3"), 1, 0)
#  

## ---- eval=FALSE---------------------------------------------------------
#  all_searches<-get_archivesearches()
#  

## ---- eval=FALSE---------------------------------------------------------
#  initial_1841_searches<-all_searches%>%
#    dplyr::select(id, search_text, archive_date_start, archive_date_end) %>%
#    filter(archive_date_start>lubridate::ymd("1841-01-01"), archive_date_start<lubridate::ymd("1841-12-31")) %>%
#    filter(search_text %in% c("election", "candidate", "party", "husting", "magistrate",
#                              "riot", "disturbance", "incident", "mob", "rough", "adjourn",
#                              "prison", "police"))
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  res_i_1841<-get_archivesearchresults(archive_search_id = initial_1841_searches$id)
#  dim(res_i_1841)

## ---- eval=FALSE---------------------------------------------------------
#  res_oneday_1832<-get_archivesearchresults(archive_search_id = c(228:255))
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  res_i_1841 <- res_i_1841 %>%
#    left_join(all_searches, by=c("archive_search_id"="id")) %>%
#    mutate(std_url = sub("download/", "", url))
#  
#  res_oneday_1832 <- res_oneday_1832 %>%
#    left_join(all_searches, by=c("archive_search_id"="id")) %>%
#    mutate(std_url = sub("download/", "", url))
#  

## ------------------------------------------------------------------------
#  
#  select_descript_xgb<-classifier_selection_description(candocs, res_i_1841, classifier_type = "xgboost")
#  
#  dim(select_descript_xgb)
#  

## ------------------------------------------------------------------------
#  select_descript_nb<-classifier_selection_description(candocs, res_i_1841, classifier_type = "nb")
#  dim(select_descript_nb)
#  

## ------------------------------------------------------------------------
#  xgb_cand_docs<-get_candidates_fromarchivesearchresults(select_descript_xgb)
#  knitr::kable(head(xgb_cand_docs[xgb_cand_docs$status!="1"|is.na(xgb_cand_docs$status), c("status", "description")]))
#  
#  xgb_cand_docs$g_status<-xgb_cand_docs$status
#  xgb_cand_docs$status<-1
#  
#  xgb_cand_docs$status_writer<-"xgboost_description"
#  
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  to_csv<-xgb_cand_docs[,c("id", "url", "publication_title", "description", "status", "g_status", "title", "status_writer")]
#  
#  csv_filename<-gsub(" ", "_", paste0("britishnewspaperarchive_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".csv"))
#  
#  write.csv(to_csv, file=csv_filename, row.names = FALSE)

## ---- results='asis'-----------------------------------------------------
#  documents_to_latex(sample_n(select_descript_xgb, 30), include_ocr=FALSE, pandoc_output=TRUE)
#  

