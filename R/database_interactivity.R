keyring_evp_available<-function(username="not implemented"){

  #' Checks if an evp password is available.
  #'
  #' Checks if an evp password is available on this computer.
  #' @export
  these_keys<-keyring::key_list()
  needed_key<- dplyr::filter(these_keys, service=="evp", username=="data_writer")
  if(nrow(needed_key)==0){
    available <- FALSE
  } else if(nrow(needed_key)>0){
    available <- TRUE
  } else {
    warning(paste0("There are an unexpected number of entries in the keyring: ", nrow(needed_key)))
    available <- FALSE
  }

  return(available)
}

manage_dbcons <- function(host="coders.victorianelectionviolence.uk", username="data_writer", dbname="evp", port=3306){
  #' Returns a connection to the election violence database
  #'
  #' This function is primarily for use within other functions.
  #' This is now a wrapper function around pool::dbPool to handle connections to the database.
  #' Database password is obtained from the keyring if there is a password for the database in the keyring. Otherwise the user is prompted for the database password.
  #' @export
  if(keyring_evp_available()){
    password_method <- "keyring"
  } else {
    password_method <- "ask"
  }

  if (password_method=="keyring"){
    password <- keyring::key_get(dbname, username)
  } else if (password_method=="config"){
    password<-config::get()$datawarehouse$pwd
  } else{
    password<- rstudioapi::askForPassword("Database password")
  }
  env<-globalenv()

  poolproblem<-!exists(".evp_db_pool", envir=env) | class(env$.evp_db_pool)[1]!="Pool"

  if(poolproblem){
    .evp_db_pool<<- pool::dbPool(drv=RMySQL::MySQL(),
                                 host=host,
                                 dbname = dbname,
                                 port=port,
                                 username = username,
                                 password = password)

  }

 env$.evp_db_pool
}
evdb_connect <- function(host="coders.victorianelectionviolence.uk", user="data_writer", dbname="evp", port=3306, password_method="ask"){
  #' Set connection to election violence database
  #'
  #' @param host Location of database host.
  #' @param user Username for accessing the database.
  #' @param dbname Name of database.
  #' @param port Value of port connection.
  #' @param password_method Method of obtaining database password for specified user.
  #' @return The connection to the election violence database.
  #' @export
  #' @examples
  #' # for default connection to the database
  #' con <- evdb_connect()
  #' # using alternative method of entering password information
  #' con <- evdb_connect(password_method="keyring")

  if (password_method=="keyring"){
    password <- keyring::key_get(dbname, user)
  } else if (password_method=="config"){
    password<-config::get()$datawarehouse$pwd
  } else{
    password<- rstudioapi::askForPassword("Database password")
  }
  DBI::dbConnect(RMySQL::MySQL(),
                 host = host,
                 user = user,
                 port=port,
                 password = password,
                 dbname=dbname
  )
}

build_where_condition <- function (col_name, vals, existing_condition, existing_interpolate_list){
  #'Constructs where conditions for querying database.
  #'
  #'@param col_name The column name for there where condition.
  #'@param vals The value or values to include in the query.
  #'@param existing_condition An existing SQL query to append this where condition to.
  #'@param existing_interpolate_list Existing interpolation list to add this interpolation list to.
  #'@export
  if (length(vals)==0){
    warning("zero length value condition in build_where_condition. Query will return empty dataset.")
    return(list(condition= paste(existing_condition, " WHERE 1=2"), interpolate_list=existing_interpolate_list))
  }
  if (identical(vals, "all")|identical(vals, "%")){
    return(list(condition= existing_condition, interpolate_list=existing_interpolate_list))
  } else {
    where_pos<- stringr::str_locate(existing_condition, "WHERE")
    if (is.na(where_pos[1])){
      existing_condition <- paste0(existing_condition, " WHERE ")
    } else {
      existing_condition <- paste0(existing_condition, " AND ")
    }
  }
  expandout <- expand_out_ors (col_name, vals)
  condition <- paste0(existing_condition, expandout[["condition"]])
  interpolate_list <- c(existing_interpolate_list, expandout[["interpolate_list"]])

  list(condition=condition, interpolate_list=interpolate_list)
}
expand_out_ors <- function(col_name, vals){
  res<-list()
  interpolate_list <- list()
  for (this_n in seq_along(vals)){
    if (this_n==1){
      this_condition = "("
    } else {
      this_condition <- paste0(this_condition, " OR ")
    }
    this_condition <- paste0(this_condition, col_name, "=?", col_name, this_n)
    interpolate_list[[paste0(col_name, this_n)]] <- vals[this_n]
  }
  this_condition <- paste0(this_condition, ")")
  res[["condition"]] <- this_condition
  res[["interpolate_list"]]<-interpolate_list
  res
}

get_allocation <- function(user_id = "all", allocation_type="all", document_id="all", user_doc_id="all"){
  #' Get the articles currently allocated to the user.
  #' @param user_id The user_id to check in the database.
  #' @param document_id The set of document_ids to include in the allocation
  #' @return dataframe of the document allocations to the user.
  #' @export

  con <- manage_dbcons()
  this_sql <-"SELECT * FROM portal_userdocumentallocation" # base query

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res<-build_where_condition("allocation_type", allocation_type, res[[1]], res[[2]])
  res<-build_where_condition("document_id", document_id, res[[1]], res[[2]])
  res<-build_where_condition("id", user_doc_id, res[[1]], res[[2]])

  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  allocation<-DBI::dbGetQuery(con, this_safe_sql)


  allocation
}

user_allocated_fromset <- function(user_id, set, not_allocated=FALSE){
  #' Which items allocated (or not allocated) to user
  #'
  #' Returns the elements of a set of documents which are allocated (or not allocated) to a user.
  #' @param user_id The user id to check in the database.
  #' @param set The set of documents to check.
  #' @param not_allocated If FALSE (default) returns the items allocated to the user, if TRUE returns items NOT allocated to the user.
  #' @return A vector numbers representing the document ids from the set which are allocated to the user (or which are not allocated to the user if not_allocated=TRUE).
  #' @examples
  #' # returns the trainingset documents allocated to user 1
  #' user_allocated_fromset(1, set=define_trainingset())
  #'
  #' # returns the trainingset documents not allocated to user 1
  #' user_allocated_fromset(1, set=define_trainingset(), not_allocated=TRUE)
  #'
  #' @export


  user_allocation <- get_allocation(user_id)
  training_allocation <- dplyr::filter(user_allocation, allocation_type=="training")

  doc_ids <- set[set %in% user_allocation$document_id] # document ids which are in training set but not allocated to the user

  doc_ids_needed <- set[!set %in% user_allocation$document_id] # document ids which are in training set but not allocated to the user
   if (not_allocated){
    res<-doc_ids_needed
  } else {
    res<-doc_ids
  }
  res
}


get_user <- function(user_id){
  #' Gets user details.

  #' @param user_id The user id.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM auth_user"

  res<-build_where_condition("id", user_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  users<-DBI::dbGetQuery(con, this_safe_sql)


  users
}

get_status<-function(user_id){
  con <- manage_dbcons()

  this_sql<-"SELECT * FROM auth_user WHERE id=?user_id ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql, user_id=user_id)
  users<-DBI::dbGetQuery(con, this_safe_sql)


  users

}

get_document<-function(document_id){
  #' Returns the document table filtered by document id
  #'
  #' @param document_id Document id or ids to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_document" # base query

  res<-build_where_condition("id", document_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  documents<-DBI::dbGetQuery(con, this_safe_sql)



  documents
}

get_candidate_documents<-function(cand_document_id="all", url="all", status="all", include_ocr=TRUE){
  #' Returns the candidate document table filtered by candidate document id
  #'
  #'Note that excluding the ocr from the download will significantly reduce the time for the query to execute.
  #' @param cand_document_id Candidate document id or ids to filter by.
  #' @param url document url or urls to filter by.
  #' @param status document classification status to filter by.
  #' @param include_ocr should the ocr be downloaded.
  #' @export

  con <- manage_dbcons()
  if(include_ocr){
    this_sql<-"SELECT * FROM portal_candidatedocument" # base query with ocr
  } else {

    this_sql<-"SELECT id, title, url, description, publication_title, publication_location, type, status, page, publication_date, word_count, g_status, status_writer FROM portal_candidatedocument" # base query without ocr

  }

  res<-build_where_condition("id", cand_document_id, this_sql, NULL)
  res<-build_where_condition("url", url, res$condition, res$interpolate_list)
  res<-build_where_condition("status", status, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  cand_documents<-DBI::dbGetQuery(con, this_safe_sql)

  cand_documents
}

get_event_report<-function(event_report_id="all", user_doc_id="all"){
  #' Returns the event_report table filtered by event report id
  #'
  #' @param event_report_id Event report id or ids to filter by.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_eventreport" # base query

  res<-build_where_condition("id", event_report_id, this_sql, NULL)
  res<-build_where_condition("user_doc_id", user_doc_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  eventreport<-DBI::dbGetQuery(con, this_safe_sql)

  eventreport
}

get_tag<-function(tag_id="all", event_report_id="all"){
  #' Returns the tag table filtered by either tag_id (primary key) or event report id
  #'
  #' @param tag_id Primary key from tag table
  #' @param event_report_id Event report id or ids to filter by.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_tag" # base query

  res<-build_where_condition("id", tag_id, this_sql, NULL)
  res<-build_where_condition("event_report_id", event_report_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  tag<-DBI::dbGetQuery(con, this_safe_sql)

  tag
}

get_attribute<-function(attribute_id="all", tag_id="all"){
  #' Returns the tag table filtered by either attribute_id (primary key) or tag_id
  #'
  #' @param attribute_id attribute id or ids to filter by.
  #' @param tag_id Foreign key from tag table
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_attribute" # base query

  res<-build_where_condition("id", attribute_id, this_sql, NULL)
  res<-build_where_condition("tag_id", tag_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  attribute<-DBI::dbGetQuery(con, this_safe_sql)

  attribute
}

users_to_actual<-function(user_id){
  #' Restrict a vector to user ids actually existing in the the database.
  #' @export

  users_from_db<-get_user(user_id)$id
  actual_users <- user_id[user_id %in% users_from_db]
  if (length(actual_users)<length(user_id)){
    warning(paste0("Some user ids not found in database and will not be used: ", paste0(user_id[!user_id %in% actual_users], collapse=", ")))
  }

  actual_users
}

documents_to_actual<-function(document_id){
  #' Restrict a vector to document ids actually existing in the database.
  #' @export

  docs_from_db<-get_document(document_id)$id
  actual_docs <- document_id[document_id %in% docs_from_db]
  if (length(actual_docs)<length(document_id)){
    warning(paste0("Some document ids not found in database and will not be used: ", paste0(document_id[!document_id %in% actual_docs], collapse=", ")))
  }

  actual_docs
}

get_user_mode <- function(user_id="all"){
  #' Returns table of current user modes
  #'
  #' Returns the table of current user modes.
  #' @param user_id Id of the user whose mode to check or a vector of user ids. The default ("all") selects all users.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_userprofile"

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  profiles<-DBI::dbGetQuery(con, this_safe_sql)


  profiles
}

get_archivesearches<-function(archive_search_id="all"){
  #' Returns the table of searches which have been passed to the crawler.
  #'
  #' @param archive_search_id archive_search id or ids to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_archivesearch" # base query

  res<-build_where_condition("id", archive_search_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  archivesearch<-DBI::dbGetQuery(con, this_safe_sql)



  archivesearch
}

get_archivesearchresults<-function(archivesearchresults_id="all", archive_search_id="all"){
  #' Returns the crawler search table
  #'
  #' @param archivesearch_id archive_search id or ids to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_archivesearchresult" # base query

  res<-build_where_condition("id", archivesearchresults_id, this_sql, NULL)
  res<-build_where_condition("archive_search_id", archive_search_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  archivesearch<-DBI::dbGetQuery(con, this_safe_sql)



  archivesearch
}
set_user_mode <- function (user_id, new_mode){
  #' Sets user mode
  #'
  #' Sets the users mode to a new value (training, testing, coding, checking, ideal).
  #' @param user_id Id of the user whose mode to set or a vector of user ids.
  #' @param new_mode The new mode (one of: training, testing, coding, checking, ideal) to set as the users mode(s). For multiple users either a single value should be given to be assigned to all identified users, or a vector of values should be given of the same length as the vector of user_ids.
  #' @export

  con <- manage_dbcons()
  #print(con)

  if(length(new_mode)==1) new_mode <- rep(new_mode, length(user_id))
  if(length(new_mode)!=length(user_id)) warning("length (mode) not equal to length(user_id))")

  current_modes <- get_user_mode(user_id) %>%
    mutate(new_mode=new_mode)
  for (this_user in user_id){
    this_data <- current_modes %>%
      filter(user_id==this_user) %>%
      top_n(1)
    this_id<-this_data$id
    this_mode<-this_data$new_mode

    this_sql<-"UPDATE portal_userprofile set mode=?new_mode WHERE id=?id"
    this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                       id = this_id,
                                       new_mode=this_mode)
    documents<-DBI::dbGetQuery(con, this_safe_sql)



  }
}

killDbConnections <- function () {
  #' Kills all current database connections
  #'
  #' @export

  all_cons <- DBI::dbListConnections(RMySQL::MySQL())

  print(all_cons)

  for(con in all_cons) DBI::dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}
