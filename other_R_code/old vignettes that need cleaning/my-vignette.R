## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE----------------------------------------
library(durhamevp)

## also using tidyverse functions
library(tidyverse)

## ----eval=FALSE----------------------------------------------------------
#  con <- evdb_connect()

## ------------------------------------------------------------------------
con <- evdb_connect(password_method="keyring")


## ------------------------------------------------------------------------
assign_initalsets_to_users(3)

## ------------------------------------------------------------------------
assign_initalsets_to_users(c(3, 4, 5, 6))

## ------------------------------------------------------------------------
get_allocation(user_id = 1)%>%
  dplyr::select(user_id, document_id, allocation_type, allocation_date) 


## ------------------------------------------------------------------------
get_allocation(user_id = 1, allocation_type="ideal") %>%
  dplyr::select(user_id, document_id, allocation_type, allocation_date) 

