## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message-FALSE, eval=FALSE-------------------------------------------
#  devtools::install_github("gidonc/durhamevp")
#  

## ----message=FALSE, warning=FALSE----------------------------------------
library(durhamevp)


## ----eval=FALSE----------------------------------------------------------
#  keyring::key_set(service="evp", user="data_writer")

## ----eval=FALSE----------------------------------------------------------
#  con <- evdb_connect()

## ------------------------------------------------------------------------
con <- evdb_connect(password_method="keyring")


